# Functional Programming
## Term Project
### Briana Oursler, Winter 22

Assembler is a project that accepts an input file with a line by line list of assembly instructions supporting operations add, mov, xor, sub, mul, and, and. Assembler parses intel type assembly syntax. Current limitations include:
 - Limited constant expression evaluation (effectively handles sums on memoperands)
 - Errors represented as strings
 - Does not output machine code
 - Current program is effectively a code formatter of questionable quality. It will replace lines with incorrect (or too complicated but correct) syntax with error messages
 
 The goals of the assembler project were considerably broader, however at 1000+ lines of code, I declare it has met an MVP and some definition of done.
 
 To run Assembler: 
  - `cargo build && cargo run input.txt`
   
