mod mem_op;
mod registers;

use mem_op::MemOperand;
use registers::Register;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct Instruction {
    operation: Ops,
    dst: Operand,
    src1: Operand,
}

impl FromStr for Instruction {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        let (ins, rem) = match s.split_once(char::is_whitespace) {
            None => (s, ""),
            Some(x) => x,
        };

        let mut operands = rem.trim().split(',');
        Ok(Instruction {
            operation: ins.trim().parse::<Ops>()?,
            dst: operands.next().unwrap_or("").trim().parse::<Operand>()?,
            src1: operands.next().unwrap_or("").trim().parse::<Operand>()?,
        })
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::registers::{r8, rax, rcx};
    #[test]
    fn test_instruction_from_str() {
        assert_eq!(
            "add".parse::<Instruction>(),
            Ok(Instruction {
                operation: Ops::Add,
                dst: Operand::Unused,
                src1: Operand::Unused
            })
        );

        assert_eq!(
            "sub   rax,   [rcx + 8]".parse::<Instruction>(),
            Ok(Instruction {
                operation: Ops::Sub,
                dst: Operand::Reg(rax),
                src1: Operand::Mem(MemOperand {
                    source: Some(rcx),
                    index: None,
                    scale: 0,
                    displacement: 8,
                })
            })
        );

        assert_eq!(
            "mov r8, -30".parse::<Instruction>(),
            Ok(Instruction {
                operation: Ops::Mov,
                dst: Operand::Reg(r8),
                src1: Operand::Imm(-30),
            })
        );

        assert_eq!(
            "xor [r8 + 2*rax + -30] , rcx".parse::<Instruction>(),
            Ok(Instruction {
                operation: Ops::Xor,
                dst: Operand::Mem(MemOperand {
                    source: Some(r8),
                    index: Some(rax),
                    scale: 2,
                    displacement: -30,
                }),
                src1: Operand::Reg(rcx),
            })
        );
    }
}

#[derive(Debug, PartialEq)]
enum Ops {
    Add,
    Sub,
    Mul,
    Xor,
    And,
    Mov,
}

impl FromStr for Ops {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "add" => Ok(Ops::Add),
            "sub" => Ok(Ops::Sub),
            "mul" => Ok(Ops::Mul),
            "xor" => Ok(Ops::Xor),
            "and" => Ok(Ops::And),
            "mov" => Ok(Ops::Mov),
            _ => Err(format!("unkown op '{}'", s)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Operand {
    Unused,
    Reg(Register),
    Imm(i32),
    Mem(MemOperand),
}

impl FromStr for Operand {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        if s.is_empty() {
            Ok(Operand::Unused)
        } else if let Ok(x) = s.parse::<Register>() {
            Ok(Operand::Reg(x))
        } else if let Ok(x) = s.parse::<i32>() {
            Ok(Operand::Imm(x))
        } else if let Ok(x) = s.parse::<MemOperand>() {
            Ok(Operand::Mem(x))
        } else {
            Err(format!("Invalid Operand: '{}'", s))
        }
    }
}

fn main() {
    println!(
        "Register struct size: '{}'",
        std::mem::size_of::<Register>()
    );
    println!("Hello, world!");
}
