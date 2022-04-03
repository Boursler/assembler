use crate::registers::Register;

enum Expr {
    Num(i32),
    Reg(Register),
    Op(BinaryOp, Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq, Eq)]
enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq, Eq)]
enum Token {
    Num(i32),
    Op(BinaryOp),
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    Reg(Register),
}

trait Lexer: Iterator<Item = Result<Token, String>> {}

trait IntoLexer<T>
where
    T: Lexer,
{
    fn into_lexer(self) -> T;
}

struct StrLexer<'a> {
    s: &'a str,
}

impl<'a> TryFrom<&'a str> for Token {
    type Error = String;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        //Some(s)
        use BinaryOp::*;
        use Token::*;
        Ok(match s {
            "+" => Op(Add),
            "-" => Op(Sub),
            "*" => Op(Mul),
            "/" => Op(Div),
            "(" => ParenOpen,
            ")" => ParenClose,
            "[" => BracketOpen,
            "]" => BracketClose,
            t => {
                if let Ok(x) = t.parse::<Register>() {
                    Reg(x)
                } else if let Ok(x) = t.parse::<i32>() {
                    Num(x)
                } else {
                    return Err(format!("lexing error invalid input: {}", t));
                }
            }
        })
    }
}

impl<'a> IntoLexer<StrLexer<'a>> for &'a str {
    fn into_lexer(self) -> StrLexer<'a> {
        StrLexer { s: self }
    }
}

impl<'a> Iterator for StrLexer<'a> {
    type Item = Result<Token, String>;
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
            _ => Some(head.try_into()),
        }
    }
}

impl<'a> Lexer for StrLexer<'a> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registers::rax;
    use BinaryOp::*;
    use Token::*;

    #[test]
    fn test_lexer() {
        let s = "rax + 23*2\n";
        let res: Vec<Token> = vec![Reg(rax), Op(Add), Num(23), Op(Mul), Num(2)];

        let tokens: Vec<Token> = s.into_lexer().map(|x| x.unwrap()).collect();
        assert_eq!(tokens, res);
    }
}
