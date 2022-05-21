use crate::instruction::{Instruction, Operand, Ops};
use crate::lexer::{BinaryOp, Keyword, Label, Lexer, Token};
use crate::mem_op::{Expr, MemOperand};
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
    InvalidOperand(String),
    InvalidMemExpr(String),
    None,
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
    let (n, lex) = next(lex);
    let token = match n {
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
    let (n, lex) = next(lex);
    let label = match n {
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
    let (n, lex) = next(lex);
    let token = match n {
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
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(GrammarError::InvalidToken(x)),
        None => {
            return Err(GrammarError::InvalidInstruction(format!(
                "No instruction supplied"
            )))
        }
    };
    let instr = match token {
        Token::InstrOp(x) => x,
        _ => {
            return Err(GrammarError::InvalidInstruction(format!(
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

fn parse_operand<L>(lex: L) -> ParseResult<Operand, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    let token = match n {
        Some(Ok(x)) => x,
        Some(Err(x)) => return Err(GrammarError::InvalidToken(x)),
        None => return Err(GrammarError::InvalidOperand(format!("No operand supplied"))),
    };
    match token {
        Token::Reg(x) => Ok((Operand::Reg(x), lex)),
        Token::Num(x) => Ok((Operand::Imm(x), lex)),
        Token::BracketOpen => match parse_memexpr(lex) {
            Ok((m, l)) => {
                let (n, lex) = next(l);
                match n {
                    Some(Ok(Token::BracketClose)) => Ok((Operand::Mem(m), lex)),
                    Some(Err(x)) => Err(GrammarError::InvalidToken(x)),
                    _ => Err(GrammarError::InvalidOperand(format!(
                        "No closing bracket for memory operand"
                    ))),
                }
            }
            Err(x) => Err(x),
        },
        _ => Err(GrammarError::InvalidOperand(format!("No valid operand"))),
    }
}

fn parse_memexpr<L>(lex: L) -> ParseResult<MemOperand, L>
where
    L: Lexer,
{
    match parseExpr(lex) {
        //will need a MemOperand from Expr
        Ok((x, l)) => Ok((MemOperand::try_from(x).unwrap(), l)),
        Err(x) => Err(x),
    }
}

struct ExprOp {
    expr: Expr,
    op: Option<BinaryOp>,
}

fn parseExpr<L>(lex: L) -> ParseResult<Expr, L>
where
    L: Lexer,
{
    //    let block = parse_to_block(Vec::new(), lex);
    // let block = parse_muldiv(block);
    // //verify block is of correct form
    // let expr = parse_addsub(block)
    Err(GrammarError::None)
}

fn get_next_token<L>(lex: L, none_err: GrammarError) -> ParseResult<Token, L>
where
    L: Lexer,
{
    let (n, lex) = next(lex);
    match n {
        Some(Ok(x)) => Ok((x, lex)),
        Some(Err(x)) => Err(GrammarError::InvalidToken(x)),
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
        Some(Err(x)) => Err(GrammarError::InvalidToken(x)),
        None => Ok((None, lex)),
    }
}

fn parse_to_block<L>(cur: Vec<ExprOp>, lex: L) -> ParseResult<Vec<ExprOp>, L>
where
    L: Lexer,
{
    let (token, lex) = get_next_token(
        lex,
        GrammarError::InvalidMemExpr(format!("Unmatched opr and expr")),
    )?;

    let (expr, lex) = match token {
        Token::Num(x) => (Expr::Num(x), lex),
        Token::Reg(x) => (Expr::Reg(x), lex),
        Token::ParenOpen => {
            let (expr, lex) = parseExpr(lex)?;
            if let Ok((Token::ParenClose, lex)) = get_next_token(lex, GrammarError::None) {
                (expr, lex)
            } else {
                return Err(GrammarError::InvalidMemExpr(format!(
                    "Missing Closing Parenthesis"
                )));
            }
        }
        _ => return Err(GrammarError::InvalidMemExpr(format!("unimplemented"))),
    };

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

// parse_block(Seq<ExprOp>){

// }

// parse_muldiv(Seq<ExprOp>) {

// }
