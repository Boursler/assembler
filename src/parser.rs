use crate::instruction::Instruction;
use crate::lexer::{BinaryOp, Keyword, Label, Lexer, Token};
use crate::registers::Register;

enum Expr {
    Num(i32),
    Reg(Register),
    Op(BinaryOp, Box<Expr>, Box<Expr>),
}

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

//recvd entire program call parse on every token in program
//building pt
fn parse<T>(lex: T) -> Result<Program, String>
where
    T: Lexer,
{
    let p = Program {
        functions: Vec::new(),
    };
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
        Token::Key(Keyword::Fn) => match parse_function(lex) {
            Ok((f, l)) => (
                Program {
                    functions: [p.functions, vec![f]].concat(),
                },
                l,
            ),
            Err(x) => return Err(x),
        },
        _ => return Err(format!("Invalid Program")),
    };
    parse_helper(new_p, new_lex)
}

fn parse_function<T>(mut lex: T) -> Result<(Function, T), String>
where
    T: Lexer,
{
    let label = match lex.next() {
        Some(Ok(Token::Lab(x))) => x,
        Some(Ok(_)) => return Err(format!("Label name must follow keyword fn")),
        Some(Err(x)) => return Err(x),
        None => return Err(format!("Label name must follow keyword fn")),
    };
    parse_function_stmts(
        Function {
            label,
            stmts: Vec::new(),
        },
        lex,
    )
}

fn parse_function_stmts<T>(f: Function, mut lex: T) -> Result<(Function, T), String>
where
    T: Lexer,
{
    Ok((f, lex))
}
