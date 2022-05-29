#![feature(box_patterns)]
mod instruction;
mod lexer;
mod mem_op;
mod parse_error;
mod parser;
mod registers;
mod test_helpers;

use instruction::Instruction;
use mem_op::MemOperand;
use parse_error::ParseError;
use registers::Register;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::str::FromStr;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: assembler <input_file>");
        return;
    }

    let filename = &args[1];
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Failed to open file {} with error {}", filename, err);
            return;
        }
    };

    let instrs: Vec<Result<Instruction, ParseError>> = BufReader::new(file)
        .lines()
        .map(|line| line.unwrap().trim().parse::<Instruction>())
        .collect();
    for instr in instrs {
        match instr {
            Ok(t) => println!("{}", t),
            Err(e) => println!("error: {}", e),
        }
    }
}
