use crate::lexer::IntoLexer;
use crate::mem_op::MemOperand;
use crate::parse_error::ParseError;
use crate::parser::{parse_instr, parse_operand};
use crate::registers::Register;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, PartialEq, Clone)]
pub struct Instruction {
    pub operation: Ops,
    pub dst: Operand,
    pub src1: Operand,
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.operation)?;
        if self.dst != Operand::Unused {
            write!(f, " {}", self.dst)?;
        }
        if self.src1 != Operand::Unused {
            write!(f, ", {}", self.src1)?;
        }
        write!(f, "")
    }
}

impl FromStr for Instruction {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse_instr(s.into_lexer()) {
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

#[derive(Debug, PartialEq, Clone, Eq)]
pub enum Ops {
    Add,
    Sub,
    Mul,
    Xor,
    And,
    Mov,
    Jmp,
    Ret,
}

impl FromStr for Ops {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "add" => Ok(Ops::Add),
            "sub" => Ok(Ops::Sub),
            "mul" => Ok(Ops::Mul),
            "xor" => Ok(Ops::Xor),
            "and" => Ok(Ops::And),
            "mov" => Ok(Ops::Mov),
            "jmp" => Ok(Ops::Jmp),
            "ret" => Ok(Ops::Ret),
            _ => Err(ParseError::InvalidOp(s.to_string())),
        }
    }
}

impl fmt::Display for Ops {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ops::Add => write!(f, "add"),
            Ops::Sub => write!(f, "sub"),
            Ops::Mul => write!(f, "mul"),
            Ops::Xor => write!(f, "xor"),
            Ops::And => write!(f, "and"),
            Ops::Mov => write!(f, "mov"),
            Ops::Jmp => write!(f, "jmp"),
            Ops::Ret => write!(f, "ret"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operand {
    Unused,
    Reg(Register),
    Imm(i32),
    Mem(MemOperand),
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Operand::Unused => write!(f, ""),
            Operand::Reg(x) => write!(f, "{}", x),
            Operand::Imm(x) => write!(f, "{}", x),
            Operand::Mem(x) => write!(f, "{}", x),
        }
    }
}

impl FromStr for Operand {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            return Ok(Operand::Unused);
        }
        match parse_operand(s.into_lexer()) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::*;

    #[test]
    fn test_instruction_from_str() {
        let tests: Vec<(&str, &str)> = vec![
            ("add", "add"),
            ("sub  rax,  [rcx  + 8]", "sub rax, [rcx + 8]"),
            ("mov r8, -30", "mov r8, -30"),
            (
                "xor [r8 + 2*rax - 30], rcx",
                "xor [r8 + 2 * rax + -30], rcx",
            ),
            ("add rsi, -2 * (3 + 1)", "add rsi, -8"),
        ];
        test_correctness::<Instruction>(&tests);
    }
}
