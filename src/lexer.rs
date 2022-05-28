use crate::instruction::*;
use crate::registers::Register;
use std::fmt;
use std::str::FromStr;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl BinaryOp {
    pub fn evaluate(&self, left: i32, right: i32) -> i32 {
        use BinaryOp::*;
        match *self {
            Add => left + right,
            Sub => left - right,
            Mul => left * right,
            Div => left / right,
        }
    }
}
impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match &self {
                BinaryOp::Add => "+".to_string(),
                BinaryOp::Sub => "-".to_string(),
                BinaryOp::Mul => "*".to_string(),
                BinaryOp::Div => "/".to_string(),
            }
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    Fn,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Label {
    name: String,
}

impl FromStr for Label {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        let len = s.chars().count();
        if len < 2 {
            return Err(format!("Empty string is not a label"));
        }
        let s = match s.strip_suffix(":") {
            Some(x) => x,
            None => return Err(format!("Labels must end in a :")),
        };

        for (idx, c) in s.chars().enumerate() {
            if idx == 0 {
                if !c.is_alphabetic() && !(c == '_') {
                    return Err(format!("Labels must start with alphabetic character or _"));
                }
            } else {
                if !c.is_alphanumeric() && !(c == '_') {
                    return Err(format!(
                        "Labels must contain only alphanumeric or _ characters"
                    ));
                }
            }
        }

        Ok(Label {
            name: s.to_string(),
        })
    }
}
#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Num(i32),
    Op(BinaryOp),
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    Reg(Register),
    Key(Keyword),
    Lab(Label),
    InstrOp(Ops),
}

pub trait Lexer: Iterator<Item = Result<Token, String>> + Clone {}

pub trait IntoLexer<T>
where
    T: Lexer,
{
    fn into_lexer(self) -> T;
}

#[derive(Clone, Copy)]
pub struct StrLexer<'a> {
    s: &'a str,
}

impl<'a> TryFrom<&'a str> for Token {
    type Error = String;
    fn try_from(s: &'a str) -> Result<Self, Self::Error> {
        //Some(s)
        use BinaryOp::*;
        use Keyword::*;
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
            "fn" => Key(Fn),
            t => {
                if let Ok(x) = t.parse::<Register>() {
                    Reg(x)
                } else if let Ok(x) = t.parse::<i32>() {
                    Num(x)
                } else if let Ok(x) = t.parse::<Label>() {
                    Lab(x)
                } else if let Ok(x) = t.parse::<Ops>() {
                    InstrOp(x)
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
        let s = "test: rax + 23*2\n";
        let res: Vec<Token> = vec![
            Lab(Label {
                name: "test".to_string(),
            }),
            Reg(rax),
            Op(Add),
            Num(23),
            Op(Mul),
            Num(2),
        ];

        let tokens: Vec<Token> = s.into_lexer().map(|x| x.unwrap()).collect();
        assert_eq!(tokens, res);
    }
}
