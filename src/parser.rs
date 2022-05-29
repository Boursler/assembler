use crate::instruction::{Instruction, Operand, Ops};
use crate::lexer::{BinaryOp, IntoLexer, Keyword, Label, Lexer, Token};
use crate::mem_op::{Expr, MemOperand};
use crate::parse_error::ParseError;
use crate::registers::Register;

#[derive(Debug, Clone)]
enum Statement {
    L(Label),
    I(Instruction),
}

#[derive(Debug, Clone)]
struct Function {
    label: Label,
    stmts: Vec<Statement>,
}

struct Program {
    functions: Vec<Function>,
}

// Use rust move semantics to hide mutation generally enforced by datatypes.
// Because data is moved, orignating structure cannot use the mutated data.
fn concat<T>(mut v: Vec<T>, mut t: Vec<T>) -> Vec<T> {
    v.append(&mut t);
    v
}
fn append<T>(mut v: Vec<T>, mut t: T) -> Vec<T> {
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
fn parse<L>(lex: L) -> Result<Program, ParseError>
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
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(ParseError::InvalidToken(x)),
        None => return Ok((p, lex)),
    };
    let (new_p, new_lex) = match token {
        Token::Key(Keyword::Fn) => match parse_function(lex) {
            Ok((f, l)) => (
                Program {
                    functions: append(p.functions, f),
                },
                l,
            ),
            Err(e) => return Err(e),
        },
        _ => return Err(ParseError::InvalidProgram(format!("Invalid Program"))),
    };
    parse_helper(new_p, new_lex)
}

fn parse_function<L>(lex: L) -> ParseResult<Function, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    let label = match n {
        Some(Ok(Token::Lab(x))) => x,
        Some(Ok(_)) => {
            return Err(ParseError::InvalidFunction(format!(
                "Label name must follow keyword fn"
            )))
        }
        Some(Err(x)) => return Err(ParseError::InvalidToken(x)),
        None => {
            return Err(ParseError::InvalidFunction(format!(
                "Label name must follow keyword fn"
            )))
        }
    };
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
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(ParseError::InvalidToken(x)),
        None => {
            return Err(ParseError::InvalidStatement(format!(
                "No token supplied for statement"
            )))
        }
    };
    match token {
        Token::Lab(x) => Ok((Statement::L(x), lex)),
        _ => match parse_instr(lex) {
            Ok((x, l)) => Ok((Statement::I(x), l)),
            Err(x) => Err(x),
        },
    }
}

fn parse_instr<L>(lex: L) -> ParseResult<Instruction, L>
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
        _ => {
            return Err(ParseError::InvalidInstruction(format!(
                "Instruction must begin with an op"
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
        match parse_operand(lex) {
            Ok(x) => x,
            Err(_) => (Operand::Unused, lex_copy),
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
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(ParseError::InvalidToken(x)),
        None => return Err(ParseError::InvalidOperand(format!("No operand supplied"))),
    };
    match token {
        Token::Reg(x) => Ok((Operand::Reg(x), lex)),
        Token::Num(x) => Ok((Operand::Imm(x), lex)),
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
        _ => Err(ParseError::InvalidOperand(format!("No valid operand"))),
    }
}

fn parse_memexpr<L>(lex: L) -> ParseResult<MemOperand, L>
where
    L: Lexer,
{
    let (x, l) = parse_expr(lex)?;
    match MemOperand::try_from(x) {
        Ok(x) => Ok((x, l)),
        Err(e) => Err(ParseError::InvalidMemExpr(e)),
    }
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
    if (seq.len() <= 1) {
        return seq;
    }

    let (last, seq) = match pop(seq) {
        (Some(t), s) => (t, s),
        (None, s) => unreachable!(),
    };

    let seq = parse_op_category(seq, category);

    let (penultimate, seq) = match pop(seq) {
        (Some(t), s) => (t, s),
        (None, s) => unreachable!(),
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
}
