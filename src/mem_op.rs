use crate::instruction::Operand;
use crate::lexer::{BinaryOp, IntoLexer};
use crate::parse_error::ParseError;
use crate::parser::{parse_expr, parse_operand};
use crate::registers::Register;
use std::fmt;
use std::str::FromStr;

//[source + scale * index + base]
#[derive(PartialEq, Debug, Clone)]
pub struct MemOperand {
    pub source: Option<Register>,
    pub index: Option<Register>,
    pub scale: u8,
    pub displacement: i32,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Num(i32),
    Reg(Register),
    Op(BinaryOp, Box<Expr>, Box<Expr>),
    //todo: rename BinaryOp as it includes the set of Unaries
    UnaryOp(BinaryOp, Box<Expr>),
}

impl FromStr for Expr {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_expr(s.into_lexer()) {
            Ok((x, mut l)) => {
                if l.next().is_none() {
                    Ok(x)
                } else {
                    Err(ParseError::UnusedTokens)
                }
            }
            Err(x) => Err(x),
        }
    }
}

impl Expr {
    fn simplify(&self) -> Expr {
        use BinaryOp::*;
        use Expr::*;
        match self {
            Num(x) => Num(*x),
            Reg(x) => Reg(*x),
            Op(op, left, right) => match (*op, left.simplify(), right.simplify()) {
                (op, Num(x), Num(y)) => Num(op.evaluate(x, y)),
                (Add, Reg(x), Num(y)) => Op(Add, Num(y).boxed(), Reg(x).boxed()),
                (Sub, x, Num(y)) => (Op(Add, Num(-y).boxed(), (x).boxed())).simplify(),
                (Mul, Reg(x), Num(y)) => Op(Mul, Num(y).boxed(), Reg(x).boxed()),

                //x op (l next_op r)
                (Mul, Num(x), Op(Add, y, z)) => Op(
                    Add,
                    Op(Mul, Num(x).boxed(), y.boxed()).boxed(),
                    Op(Mul, Num(x).boxed(), z.boxed()).boxed(),
                )
                .simplify(),
                (Mul, Op(Add, x, y), Num(z)) => Op(
                    Add,
                    Op(Mul, x, Num(z).boxed()).boxed(),
                    Op(Mul, y, Num(z).boxed()).boxed(),
                )
                .simplify(),

                (Mul, Num(x), Op(Mul, box Num(y), z)) => Op(Mul, Num(x * y).boxed(), z),
                (Mul, Op(Mul, box Num(x), y), Num(z)) => Op(Mul, Num(x * z).boxed(), y),

                (Add, Num(x), Op(Add, box Num(y), z)) => Op(Add, Num(x + y).boxed(), z),
                (Add, Op(Add, box Num(x), y), Num(z)) => Op(Add, Num(x + z).boxed(), y),

                (Div, Op(Mul, box Num(x), y), Num(z)) => {
                    if x % z == 0 {
                        Op(Mul, Num(x / z).boxed(), y)
                    } else {
                        Op(Div, Op(Mul, Num(x).boxed(), y).boxed(), Num(z).boxed())
                    }
                }
                (op, l, r) => Op(op, l.boxed(), r.boxed()),
            },
            UnaryOp(op, expr) => match (*op, expr.simplify()) {
                (Add, x) => x,
                (Sub, x) => Op(Mul, Num(-1).boxed(), x.boxed()).simplify(),
                (op, e) => UnaryOp(op, e.boxed()),
            },
        }
    }
    fn boxed(self) -> Box<Expr> {
        Box::new(self)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Expr::Num(x) => x.to_string(),
                Expr::Reg(x) => x.to_string(),
                Expr::Op(op, left, right) => format!(
                    "({} {} {})",
                    (*left).to_string(),
                    op.to_string(),
                    (*right).to_string()
                ),
                Expr::UnaryOp(op, expr) => format!("{}{}", op.to_string(), (*expr).to_string()),
            }
        )
    }
}

impl fmt::Display for MemOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let sep = match self.source {
            Some(source) => {
                write!(f, "{}", source)?;
                " + "
            }
            None => "",
        };

        let sep = match self.index {
            Some(index) => {
                write!(f, "{}{} * {}", sep, self.scale, index)?;
                " + "
            }
            None => sep,
        };
        if self.displacement != 0 || (self.source.is_none() && self.index.is_none()) {
            write!(f, "{}{}", sep, self.displacement)?;
        }
        write!(f, "]")
    }
}

impl TryFrom<Expr> for MemOperand {
    type Error = String;
    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        //expr is binary tree, traverse iter
        fn traverse(tree: Expr, curr: MemOperand) -> Result<MemOperand, String> {
            use BinaryOp::*;
            use Expr::*;
            match tree {
                Num(x) => {
                    if curr.displacement == 0 {
                        return Ok(MemOperand {
                            source: curr.source,
                            index: curr.index,
                            scale: curr.scale,
                            displacement: x,
                        });
                    } else {
                        unreachable!("Simplify should prevent this");
                    }
                }
                Reg(x) => {
                    if curr.source.is_none() {
                        return Ok(MemOperand {
                            source: Some(x),
                            index: curr.index,
                            scale: curr.scale,
                            displacement: curr.displacement,
                        });
                    } else if curr.index.is_none() {
                        return Ok(MemOperand {
                            source: curr.source,
                            index: Some(x),
                            scale: 1,
                            displacement: curr.displacement,
                        });
                    } else {
                        return Err(format!("Too many registers"));
                    }
                }
                Op(Mul, box Num(x), box Reg(y)) => {
                    if vec![1, 2, 4, 8].contains(&x) && curr.index.is_none() {
                        return Ok(MemOperand {
                            source: curr.source,
                            index: Some(y),
                            scale: x as u8,
                            displacement: curr.displacement,
                        });
                        //power of 2 + 1
                    } else if vec![3, 5, 9].contains(&x)
                        && curr.source.is_none()
                        && curr.index.is_none()
                    {
                        return Ok(MemOperand {
                            source: Some(y),
                            index: Some(y),
                            scale: (x - 1) as u8,
                            displacement: curr.displacement,
                        });
                    } else if curr.scale == 1 && curr.source.is_none() {
                        return Ok(MemOperand {
                            source: curr.index,
                            index: Some(y),
                            scale: x as u8,
                            displacement: curr.displacement,
                        });
                    } else if x == 1 && curr.source.is_none() {
                        return Ok(MemOperand {
                            source: Some(y),
                            index: curr.index,
                            scale: curr.scale,
                            displacement: curr.displacement,
                        });
                    } else {
                        return Err(format!("Too many scaled registers"));
                    }
                }
                Op(Add, x, y) => Ok(traverse(*y, traverse(*x, curr)?)?),
                _ => Err(format!("Invalid MemOperand from Expr")),
            }
        }
        traverse(
            value.simplify(),
            MemOperand {
                source: None,
                index: None,
                scale: 0,
                displacement: 0,
            },
        )
    }
}
impl TryFrom<Operand> for MemOperand {
    type Error = ParseError;
    fn try_from(s: Operand) -> Result<Self, Self::Error> {
        if let Operand::Mem(x) = s {
            return Ok(x);
        } else {
            return Err(ParseError::UnexpectedOperandKind(
                "Expected Memory Operand".to_string(),
            ));
        }
    }
}

impl FromStr for MemOperand {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_operand(s.into_lexer()) {
            Ok((x, mut l)) => {
                if l.next().is_none() {
                    MemOperand::try_from(x)
                } else {
                    Err(ParseError::UnusedTokens)
                }
            }
            Err(x) => Err(x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_mem_operand_from_str() {
        let tests: Vec<(&str, &str)> = vec![
            ("[rax]", "[rax]"),
            ("[1 - 1]", "[0]"),
            ("[4 * (rax + -10) + rbx]", "[rbx + 4 * rax + -40]"),
            (
                "[rbx + 4 * ((1 + 1) / 2 * rax + -10)]",
                "[rbx + 4 * rax + -40]",
            ),
            ("[2*(rax + -10) + 1 * rbx]", "[rbx + 2 * rax + -20]"),
            ("[3 * rax]", "[rax + 2 * rax]"),
        ];
        test_correctness::<MemOperand>(&tests);
    }
}
