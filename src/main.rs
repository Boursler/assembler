#![feature(box_patterns)]
mod instruction;
mod lexer;
mod mem_op;
mod parse_error;
mod parser;
mod registers;
mod test_helpers;

use instruction::Instruction;
use lexer::IntoLexer;
use parse_error::ParseError;
use parser::{parse, Program};
use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: assembler <input_file>");
        return;
    }

    let filename = &args[1];
    let input_string =
        fs::read_to_string(filename).unwrap_or_else(|_| "default: rsp + 8 * 2\n".to_string());
    let lexer = input_string.as_str().into_lexer();
    let program = match parse(lexer) {
        Ok(t) => t.to_string(),
        Err(e) => panic!("Parsing resulted in error {}", e),
    };
    println!("{}", program);
}
