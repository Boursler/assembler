use crate::instruction::{Instruction, Operand};
use crate::lexer::{BinaryOp, IntoLexer, Keyword, Label, Lexer, Token};
use crate::mem_op::{Expr, MemOperand};
use crate::parse_error::ParseError;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, Clone)]
enum Statement {
    L(Label),
    I(Instruction),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                Statement::L(x) => x.to_string(),
                Statement::I(x) => x.to_string(),
            }
        )
    }
}

#[derive(Debug, Clone)]
struct Function {
    label: Label,
    stmts: Vec<Statement>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "fn {}:", self.label)?;
        for stmt in &self.stmts {
            match stmt {
                Statement::L(x) => writeln!(f, "{}:", x)?,
                Statement::I(x) => writeln!(f, "\t{}", x)?,
            };
        }
        write!(f, "")
    }
}

impl FromStr for Function {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_function(s.into_lexer()) {
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

pub struct Program {
    functions: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.functions
                .iter()
                .fold("".to_string(), |acc, f| acc + &f.to_string())
        )
    }
}

impl FromStr for Program {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse(s.into_lexer())
    }
}

// Use rust move semantics to hide mutation generally enforced by datatypes.
// Because data is moved, orignating structure cannot use the mutated data.
fn concat<T>(mut v: Vec<T>, mut t: Vec<T>) -> Vec<T> {
    v.append(&mut t);
    v
}
fn append<T>(v: Vec<T>, t: T) -> Vec<T> {
    concat(v, vec![t])
}
fn pop<T>(mut v: Vec<T>) -> (Option<T>, Vec<T>) {
    (v.pop(), v)
}

fn next<T>(mut lex: T) -> (Option<Result<Token, String>>, T)
where
    T: Lexer,
{
    (lex.next(), lex)
}

type ParseResult<T, L> = Result<(T, L), ParseError>;

//recvd entire program call parse on every token in program
//building pt
pub fn parse<L>(lex: L) -> Result<Program, ParseError>
where
    L: Lexer,
{
    let p = Program {
        functions: Vec::new(),
    };
    match parse_helper(p, lex) {
        Ok((p, _l)) => Ok(p),
        Err(x) => Err(x),
    }
}
fn parse_helper<L>(p: Program, lex: L) -> ParseResult<Program, L>
where
    L: Lexer,
{
    let lex_copy = lex.clone();
    let (new_p, new_lex) = match parse_function(lex) {
        Ok((f, l)) => (
            Program {
                functions: append(p.functions, f),
            },
            l,
        ),
        Err(ParseError::EmptyLexer) => return Ok((p, lex_copy)),
        Err(e) => return Err(e),
    };
    parse_helper(new_p, new_lex)
}

fn parse_function<L>(lex: L) -> ParseResult<Function, L>
where
    L: Lexer,
{
    let (token, lex) = get_next_token(lex, ParseError::EmptyLexer)?;
    if token != Token::Key(Keyword::Fn) {
        return Err(ParseError::InvalidFunction(format!(
            "Function must start with keyword Fn"
        )));
    }
    let (token, lex) = get_next_token(
        lex,
        ParseError::InvalidFunction(format!("No function name supplied")),
    )?;
    let label = if let Token::Lab(x) = token {
        x
    } else {
        return Err(ParseError::InvalidFunction(format!(
            "Function name must be a valid label"
        )));
    };
    let (token, lex) = get_next_token(
        lex,
        ParseError::InvalidFunction(format!("Colon must follow function name")),
    )?;
    if token != Token::Colon {
        return Err(ParseError::InvalidFunction(format!(
            "No colon found after function name"
        )));
    }
    parse_function_stmts(
        Function {
            label,
            stmts: Vec::new(),
        },
        lex,
    )
}

fn parse_function_stmts<L>(f: Function, lex: L) -> ParseResult<Function, L>
where
    L: Lexer,
{
    let lex_copy = lex.clone();
    let (f, lex) = match parse_stmt(lex) {
        Ok((s, l)) => (
            Function {
                label: f.label,
                stmts: append(f.stmts, s),
            },
            l,
        ),
        Err(x) => {
            if f.stmts.len() != 0 {
                return Ok((f, lex_copy));
            } else {
                return Err(x);
            }
        }
    };

    parse_function_stmts(f, lex)
}

fn parse_stmt<L>(lex: L) -> ParseResult<Statement, L>
where
    L: Lexer,
{
    let lex_copy = lex.clone();
    let (token, lex) = get_next_token(
        lex,
        ParseError::InvalidStatement(format!("No token supplied for statement")),
    )?;
    match token {
        Token::Lab(x) => {
            let (token, lex) = get_next_token(
                lex,
                ParseError::InvalidStatement(format!("Colon must follow label in statement")),
            )?;
            if token != Token::Colon {
                return Err(ParseError::InvalidStatement(format!(
                    "No colon found after label in statement"
                )));
            }
            Ok((Statement::L(x), lex))
        }
        _ => match parse_instr(lex_copy) {
            Ok((x, l)) => Ok((Statement::I(x), l)),
            Err(x) => Err(x),
        },
    }
}

pub fn parse_instr<L>(lex: L) -> ParseResult<Instruction, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(ParseError::InvalidToken(x)),
        None => {
            return Err(ParseError::InvalidInstruction(format!(
                "No instruction supplied"
            )))
        }
    };
    let instr = match token {
        Token::InstrOp(x) => x,
        e => {
            return Err(ParseError::InvalidInstruction(format!(
                "Instruction must begin with an op instead found {:?}",
                e
            )))
        }
    };
    let lex_copy = lex.clone();
    let (op1, lex) = match parse_operand(lex) {
        Ok(x) => x,
        Err(_) => (Operand::Unused, lex_copy),
    };
    let (op2, lex) = if op1 == Operand::Unused {
        (Operand::Unused, lex)
    } else {
        let lex_copy = lex.clone();
        match try_get_next_token(lex)? {
            (Some(Token::Comma), l) => match parse_operand(l) {
                Ok(x) => x,
                Err(x) => return Err(x),
            },
            _ => (Operand::Unused, lex_copy),
        }
    };
    Ok((
        Instruction {
            operation: instr,
            dst: op1,
            src1: op2,
        },
        lex,
    ))
}

pub fn parse_operand<L>(lex: L) -> ParseResult<Operand, L>
where
    L: Lexer,
{
    let lex_copy = lex.clone();
    let (token, lex) = get_next_token(
        lex,
        ParseError::InvalidOperand(format!("No operand supplied")),
    )?;
    match token {
        Token::Reg(x) => Ok((Operand::Reg(x), lex)),
        Token::BracketOpen => match parse_memexpr(lex) {
            Ok((m, l)) => {
                let (n, lex) = next(l);
                match n {
                    Some(Ok(Token::BracketClose)) => Ok((Operand::Mem(m), lex)),
                    Some(Err(x)) => Err(ParseError::InvalidToken(x)),
                    _ => Err(ParseError::InvalidOperand(format!(
                        "No closing bracket for memory operand"
                    ))),
                }
            }
            Err(x) => Err(x),
        },
        _ => match parse_intexpr(lex_copy) {
            Ok((t, l)) => Ok((Operand::Imm(t), l)),
            Err(_) => Err(ParseError::InvalidOperand(format!("No operand supplied"))),
        },
    }
}

fn parse_intexpr<L>(lex: L) -> ParseResult<i32, L>
where
    L: Lexer,
{
    let (x, l) = parse_expr(lex)?;
    Ok((i32::try_from(x)?, l))
}

fn parse_memexpr<L>(lex: L) -> ParseResult<MemOperand, L>
where
    L: Lexer,
{
    let (x, l) = parse_expr(lex)?;
    Ok((MemOperand::try_from(x)?, l))
}

struct ExprOp {
    expr: Expr,
    op: Option<BinaryOp>,
}

pub fn parse_expr<L>(lex: L) -> ParseResult<Expr, L>
where
    L: Lexer,
{
    let (block, lex) = parse_to_block(Vec::new(), lex)?;
    let block = parse_op_category(block, &vec![BinaryOp::Mul, BinaryOp::Div]);
    let block = parse_op_category(block, &vec![BinaryOp::Add, BinaryOp::Sub]);
    let (result, block) = pop(block);
    if block.len() != 0 {
        panic!("parse op category should result in a single expression");
    }
    Ok((result.unwrap().expr, lex))
}

fn get_next_token<L>(lex: L, none_err: ParseError) -> ParseResult<Token, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    match n {
        Some(Ok(x)) => Ok((x, lex)),
        Some(Err(x)) => Err(ParseError::InvalidToken(x)),
        None => Err(none_err),
    }
}

fn try_get_next_token<L>(lex: L) -> ParseResult<Option<Token>, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    match n {
        Some(Ok(x)) => Ok((Some(x), lex)),
        Some(Err(x)) => Err(ParseError::InvalidToken(x)),
        None => Ok((None, lex)),
    }
}

fn parse_to_block<L>(cur: Vec<ExprOp>, lex: L) -> ParseResult<Vec<ExprOp>, L>
where
    L: Lexer,
{
    let (expr, lex) = parse_single_expr(lex)?;
    let (op, lex, f) = match try_get_next_token(lex.clone())? {
        (Some(Token::Op(x)), l) => (Some(x), l, false),
        _ => (None, lex, true),
    };

    let v = append(cur, ExprOp { expr, op });
    if f {
        Ok((v, lex))
    } else {
        parse_to_block(v, lex)
    }
}

fn parse_single_expr<L>(lex: L) -> ParseResult<Expr, L>
where
    L: Lexer,
{
    let (token, lex) = get_next_token(
        lex,
        ParseError::InvalidMemExpr(format!("Unmatched opr and expr")),
    )?;

    match token {
        Token::Num(x) => Ok((Expr::Num(x), lex)),
        Token::Reg(x) => Ok((Expr::Reg(x), lex)),
        Token::Op(BinaryOp::Add) => parse_single_expr(lex),
        Token::Op(BinaryOp::Sub) => {
            let (expr, lex) = parse_single_expr(lex)?;
            Ok((Expr::UnaryOp(BinaryOp::Sub, Box::new(expr)), lex))
        }
        Token::ParenOpen => {
            let (expr, lex) = parse_expr(lex)?;
            if let Ok((Token::ParenClose, lex)) = get_next_token(lex, ParseError::None) {
                Ok((expr, lex))
            } else {
                return Err(ParseError::InvalidMemExpr(format!(
                    "Missing Closing Parenthesis"
                )));
            }
        }
        _ => return Err(ParseError::InvalidMemExpr(format!("unimplemented"))),
    }
}

fn parse_op_category(seq: Vec<ExprOp>, category: &Vec<BinaryOp>) -> Vec<ExprOp> {
    if seq.len() <= 1 {
        return seq;
    }

    let (last, seq) = match pop(seq) {
        (Some(t), s) => (t, s),
        (None, _s) => unreachable!(),
    };

    let seq = parse_op_category(seq, category);

    let (penultimate, seq) = match pop(seq) {
        (Some(t), s) => (t, s),
        (None, _s) => unreachable!(),
    };

    let needs_combined = penultimate
        .op
        .map_or_else(|| false, |op| category.contains(&op));

    if needs_combined {
        let op = penultimate.op.unwrap();
        append(
            seq,
            ExprOp {
                expr: Expr::Op(op, Box::new(penultimate.expr), Box::new(last.expr)),
                op: last.op,
            },
        )
    } else {
        append(append(seq, penultimate), last)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_parse_expr() {
        let tests: Vec<(&str, &str)> = vec![
            ("1 + rax", "(1 + rax)"),
            ("1 + -1", "(1 + -1)"),
            ("-1 + rax", "(-1 + rax)"),
            ("1 + 2 * 3", "(1 + (2 * 3))"),
            ("2 * 3 + 1", "((2 * 3) + 1)"),
            ("2 * -3 + 1", "((2 * -3) + 1)"),
            ("1 + -(2 * 3)", "(1 + -(2 * 3))"),
            ("-(2 * -3) + 1", "(-(2 * -3) + 1)"),
            ("rax * -3 + rax", "((rax * -3) + rax)"),
        ];
        test_correctness::<Expr>(&tests);
    }

    #[test]
    fn test_parse_function() {
        let tests: Vec<(&str, &str)> = vec![(
            "fn coolname: \n add rsp, 8\n\tlabel:\nret",
            "fn coolname:\n\tadd rsp, 8\nlabel:\n\tret\n",
        )];
        test_correctness::<Function>(&tests);
    }
    #[test]
    fn test_parse() {
        let tests: Vec<(&str, &str)> = vec![(
            "fn jumpsomewhere: \n add rsp, 8\n\tlabel:\nret
fn dostuff: \n xor rbx, rbx\n\tlabel:\nret",
            "fn jumpsomewhere:\n\tadd rsp, 8\nlabel:\n\tret
fn dostuff:\n\txor rbx, rbx\nlabel:\n\tret\n",
        )];
        test_correctness::<Program>(&tests);
    }
}
