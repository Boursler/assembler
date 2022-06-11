# Advanced Functional Programming
## Term Project
### Assembler 2.0
### Briana Oursler, Spring 22

Assembler 2.0 is a project that accepts an input file with a line by line list of assembly instructions supporting operations add, mov, xor, sub, mul, and, jmp, and ret. Assembler parses a language whose input grammar is inspired by Intel assembly syntax. Some important considerations:
    - the parser expects a program to be made up of a list of functions. Functions, in turn, are a list of statements. A statement is an operator and its operand or operands or it is a label. 
    - Functions are prefaced by the keyword fn.
    - Function names end with a colon.
    - Labels also end with a colon.
    
Current limitations include:
    - Still does not output binary, executable or otherwise.
    - Operand needs to add support for label among its supported types.
    
The author currently intends to continue her quest to output executable binary from this project, although that is not supported in this iteration. The parser can also be extended to support more operations and to add a data section.

Functional paradigms found in this project include, the use of higher order functions such as fold, map, and unwrap_or_else, the heavy use of recursion, pattern matching, options, and the recursive data type, Expr, the use of generic associated types as in throughout the ```parse.rs``` file. State and mutation were avoided whenever the author thought it did not conflict with the design of Rust. For example, iterators in Rust force the use of ``mut`` as does ``IO`` actions such as ``fmt::Display``. Ownership was used extensively to hide mutation throughout the project, to allow the interface to mutate a structure without allowing multiple references. 

The project is intended to be a functional programming assignment suitable for a termwide project in Advanced Functional Programming. The author took inspiration from Dr. Andrew Tolmach's Programming Languages course, as well as its text, *Programming Language Pragmatics* by Michael L. Scott. She also made frequent reference to the Intel Handbook and to Dr. Roy Oursler for his expertise on the subject of x86-64 assembly.

Note that in order to run this project, the nightly toolchain must be used. This was to allow pattern matching on Boxes.

To build Assembler:
 - ```cargo +nightly build```
 
To run Assembler: 
  - `cargo +nightly build && cargo +nightly run input.asm`
  
To test Assembler:
 - ```cargo +nightly test```

  
  
  
Any input file can be specified. Please feel free to experiment with the integer algebraic parsing and memoperand parsing!
   
