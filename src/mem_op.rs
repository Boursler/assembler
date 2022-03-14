use crate::registers::Register;
use std::fmt;
use std::str::FromStr;

//[source + scale * index + base]
#[derive(PartialEq, Debug)]
pub struct MemOperand {
    pub source: Option<Register>,
    pub index: Option<Register>,
    pub scale: u8,
    pub displacement: i32,
}

impl fmt::Display for MemOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let sep = match self.source {
            Some(source) => {
                write!(f, "{}", source)?;
                " + "
            }
            None => "",
        };

        let sep = match self.index {
            Some(index) => {
                write!(f, "{}{} * {}", sep, self.scale, index)?;
                " + "
            }
            None => sep,
        };
        if self.displacement != 0 {
            write!(f, "{}{}", sep, self.displacement)?;
        }
        write!(f, "]")
    }
}

impl FromStr for MemOperand {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, String> {
        #[derive(PartialEq)]
        struct ParseState {
            source: Option<Register>,
            index: Option<Register>,
            scale: Option<u8>,
            displacement: Option<i32>,
            empty: bool,
        }
        fn parse_helper(p: ParseState, s: &str) -> Result<ParseState, String> {
            if p.empty {
                return match s.strip_prefix("[").unwrap_or("").strip_suffix("]") {
                    Some(st) => parse_helper(
                        ParseState {
                            source: p.source,
                            index: p.index,
                            scale: p.scale,
                            displacement: p.displacement,
                            empty: false,
                        },
                        st,
                    ),
                    _ => Err("poorly formatted memoperand".to_string()),
                };
            }

            if s.is_empty() {
                return if p.source.is_some() || p.index.is_some() || p.displacement.is_some() {
                    Ok(p)
                } else {
                    Err("memoperand does not contain required fields".to_string())
                };
            }

            let (next, rem) = match s.split_once('+') {
                None => (s.trim(), ""),
                Some((x, y)) => (x.trim(), y.trim()),
            };

            let p_next = if let Some((left, right)) = next.split_once('*') {
                // parse scale * idx
                if p.index.is_some() {
                    return Err("Index register already set".to_string());
                }

                let left_value = left.trim().parse::<u8>().ok();
                let right_value = right.trim().parse::<Register>().ok();
                if right_value.is_some() && left_value.is_some() {
                    ParseState {
                        source: p.source,
                        index: right_value,
                        scale: left_value,
                        displacement: p.displacement,
                        empty: p.empty,
                    }
                } else {
                    return Err(format!(
                        "Invalid scale and idx expression idx:{}, scale: {}",
                        right.trim(),
                        left.trim()
                    )
                    .to_string());
                }
            } else if let Ok(x) = next.parse::<Register>() {
                if p.source.is_none() {
                    ParseState {
                        source: Some(x),
                        index: p.index,
                        scale: p.scale,
                        displacement: p.displacement,
                        empty: p.empty,
                    }
                } else if p.index.is_none() {
                    ParseState {
                        source: p.source,
                        index: Some(x),
                        scale: Some(1),
                        displacement: p.displacement,
                        empty: p.empty,
                    }
                } else {
                    return Err("Too many registers in memory operand".to_string());
                }
            } else if let Ok(x) = next.parse::<i32>() {
                ParseState {
                    source: p.source,
                    index: p.index,
                    scale: p.scale,
                    displacement: Some(p.displacement.unwrap_or(0) + x),
                    empty: p.empty,
                }
            } else {
                return Err("Invalid register or numeral literal".to_string());
            };

            parse_helper(p_next, rem)
        }
        let o = parse_helper(
            ParseState {
                source: None,
                index: None,
                scale: None,
                displacement: None,
                empty: true,
            },
            s,
        )?;
        Ok(MemOperand {
            source: o.source,
            index: o.index,
            scale: o.scale.unwrap_or(0),
            displacement: o.displacement.unwrap_or(0),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registers::{r8, rax, rcx};

    #[test]
    fn test_mem_operand_from_str() {
        assert_eq!(
            "[rax]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: Some(rax),
                index: None,
                scale: 0,
                displacement: 0,
            })
        );

        assert_eq!(
            "[rax + r8]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: Some(rax),
                index: Some(r8),
                scale: 1,
                displacement: 0,
            })
        );

        assert_eq!(
            "[rax + 2 * r8]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: Some(rax),
                index: Some(r8),
                scale: 2,
                displacement: 0,
            })
        );

        assert_eq!(
            "[1024]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: None,
                index: None,
                scale: 0,
                displacement: 1024,
            })
        );

        assert_eq!(
            "[4 * rcx]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: None,
                index: Some(rcx),
                scale: 4,
                displacement: 0,
            })
        );

        assert_eq!(
            "[rcx + 4 * rcx + 1023]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: Some(rcx),
                index: Some(rcx),
                scale: 4,
                displacement: 1023,
            })
        );
        assert_eq!(
            "[r8 + 2*rax + -30]".parse::<MemOperand>(),
            Ok(MemOperand {
                source: Some(r8),
                index: Some(rax),
                scale: 2,
                displacement: -30,
            })
        );
    }
}
