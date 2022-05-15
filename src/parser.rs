use crate::instruction::Instruction;
use crate::lexer::{BinaryOp, Keyword, Label, Lexer, Token};
use crate::registers::Register;

enum Expr {
    Num(i32),
    Reg(Register),
    Op(BinaryOp, Box<Expr>, Box<Expr>),
}

enum Statement {
    L(Label),
    I(Instruction),
}
struct Function {
    label: Label,
    stmts: Vec<Statement>,
}

struct Program {
    functions: Vec<Function>,
}

//recvd entire program call parse on every token in program
//building pt
fn parse<T>(p: Program, lex: T) -> Result<Program, String>
where
    T: Lexer,
{
    match parse_helper(p, lex) {
        Ok((p, _l)) => Ok(p),
        Err(x) => Err(x),
    }
}
fn parse_helper<T>(p: Program, mut lex: T) -> Result<(Program, T), String>
where
    T: Lexer,
{
    let token = match lex.next() {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(x),
        None => return Ok((p, lex)),
    };
    let (new_p, new_lex) = match token {
        Token::Key(Keyword::Fn) => parse_function(p, lex),
        _ => return Err(format!("Invalid Program")),
    }?;
    parse_helper(new_p, new_lex)
}

fn parse_function<T>(p: Program, mut lex: T) -> Result<(Program, T), String>
where
    T: Lexer,
{
    Ok((p, lex))
}
