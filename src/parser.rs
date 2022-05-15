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

// Parse result must return: A Language construct or Error, Lexer_state (rewound if error was encountered), and optionally accumulated error messages to print

enum GrammarError {
    // Errors have the form ErrorType(msg)
    InvalidProgram(String),
    InvalidFunction(String),
    InvalidToken(String),
    InvalidStatement(String),
    InvalidInstruction(String),
}

type ParseResult<T, L> = Result<(T, L), GrammarError>;

//recvd entire program call parse on every token in program
//building pt
fn parse<L>(lex: L) -> Result<Program, GrammarError>
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
    let (next, lex) = next(lex);
    let token = match next {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(GrammarError::InvalidToken(x)),
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
        _ => return Err(GrammarError::InvalidProgram(format!("Invalid Program"))),
    };
    parse_helper(new_p, new_lex)
}

fn parse_function<L>(lex: L) -> ParseResult<Function, L>
where
    L: Lexer,
{
    let (next, lex) = next(lex);
    let label = match next {
        Some(Ok(Token::Lab(x))) => x,
        Some(Ok(_)) => {
            return Err(GrammarError::InvalidFunction(format!(
                "Label name must follow keyword fn"
            )))
        }
        Some(Err(x)) => return Err(GrammarError::InvalidToken(x)),
        None => {
            return Err(GrammarError::InvalidFunction(format!(
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
    let (next, lex) = next(lex);
    let token = match next {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(GrammarError::InvalidToken(x)),
        None => {
            return Err(GrammarError::InvalidStatement(format!(
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
    Err(GrammarError::InvalidInstruction(format!("unimplimented")))
}
