use crate::registers::Register;

enum Expr {
    Num(i32),
    Reg(Register),
    Op(BinaryOp, Box<Expr>, Box<Expr>),
}

enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

enum Tokens {
    Num(i32),
    Op(BinaryOp),
    ParenOpen,
    ParenClose,
    Reg(Register),
}
impl Expr {
    fn lex(s: &str) -> Vec<Tokens> {
        Vec::new()
    }
}

struct ExprIter<'a> {
    s: &'a str,
}

impl<'a> Iterator for ExprIter<'a> {
    type Item = &'a str;
    //mutated variable as this is a requirement to implement Rust iter and allow functional manipulation of input to iterate over containers
    fn next(&mut self) -> Option<Self::Item> {
        fn split(s: &str) -> (&str, &str) {
            let s = s.trim_start();
            let i = s
                .find(|c: char| c.is_whitespace() || "()[]|^&*/+-%<>".contains(c))
                .unwrap_or(s.len());

            if i == 0 {
                match s.char_indices().nth(1) {
                    Some((idx, _)) => s.split_at(idx),
                    None => s.split_at(s.len()),
                }
            } else {
                s.split_at(i)
            }
        }
        let (head, tail) = split(self.s);
        self.s = tail;
        match head {
            "" => None,
            _ => Some(head),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_splitter() {
        let test_str = "rax + 3*2\n";
        let test = ExprIter { s: test_str };
        let out: Vec<&str> = test.collect();
        assert_eq!(out, vec!["rax", "+", "3", "*", "2"]);
    }
}
