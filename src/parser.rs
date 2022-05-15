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

// Use rust move semantics to hide mutation generally enforced by datatypes.
// Because data is moved, orignating structure cannot use the mutated data.
fn concat<T>(mut v: Vec<T>, mut t: Vec<T>) -> Vec<T> {
    v.append(&mut t);
    v
}
fn append<T>(mut v: Vec<T>, mut t: T) -> Vec<T> {
    concat(v, vec![t])
}

fn next<T>(mut lex: T) -> (Option<Result<Token, String>>, T)
where
    T: Lexer,
{
    (lex.next(), lex)
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
fn parse_helper<T>(p: Program, lex: T) -> Result<(Program, T), String>
where
    T: Lexer,
{
    let (next, lex) = next(lex);
    let token = match next {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(x),
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
            Err(x) => return Err(x),
        },
        _ => return Err(format!("Invalid Program")),
    };
    parse_helper(new_p, new_lex)
}

fn parse_function<T>(lex: T) -> Result<(Function, T), String>
where
    T: Lexer,
{
    let (next, lex) = next(lex);
    let label = match next {
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

fn parse_function_stmts<T>(f: Function, lex: T) -> Result<(Function, T), String>
where
    T: Lexer,
{
    let (next, lex) = next(lex);
    let token = match next {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(x),
        None => return Ok((f, lex)),
    };
    let (new_f, new_lex) = match token {
        Token::Key(Keyword::Fn) => match parse_stmt(lex) {
            Ok((s, l)) => (
                Function {
                    label: f.label,
                    stmts: append(f.stmts, s),
                },
                l,
            ),
            Err(x) => return Err(x),
        },
        _ => return Err(format!("Invalid Program")),
    };

    parse_function_stmts(new_f, new_lex)
}

fn parse_stmt<T>(lex: T) -> Result<(Statement, T), String>
where
    T: Lexer,
{
    Err(format! {"unimplimented"})
}
