use std::fmt;

// Parse result must return: A Language construct or Error, Lexer_state (rewound if error was encountered), and optionally accumulated error messages to print
pub enum ParseError {
    // Errors have the form ErrorType(msg)
    InvalidProgram(String),
    InvalidFunction(String),
    InvalidToken(String),
    InvalidStatement(String),
    InvalidInstruction(String),
    InvalidOperand(String),
    InvalidOp(String),
    InvalidMemExpr(String),
    UnexpectedOperandKind(String),
    UnusedTokens,
    None,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;
        match self {
            InvalidProgram(x) => write!(f, "Invalid Program: {}", x),
            InvalidFunction(x) => write!(f, "Invalid Function: {}", x),
            InvalidToken(x) => write!(f, "Invalid Token: {}", x),
            InvalidStatement(x) => write!(f, "Invalid Statement: {}", x),
            InvalidInstruction(x) => write!(f, "Invalid Instruction: {}", x),
            InvalidOperand(x) => write!(f, "Invalid Operand: {}", x),
            InvalidOp(x) => write!(f, "Invalid Op: {}", x),
            InvalidMemExpr(x) => write!(f, "Invalid Memory Expression: {}", x),
            UnexpectedOperandKind(x) => write!(f, "Unexpected Operand Kind: {}", x),
            UnusedTokens => write!(f, "Unused Tokens"),
            None => write!(f, "None"),
        }
    }
}
