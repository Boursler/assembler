use std::fmt;
use std::str::FromStr;

#[cfg(test)]
pub fn test_correctness<T>(correct_tests: &Vec<(&str, &str)>)
where
    T: FromStr + ToString,
    <T as FromStr>::Err: fmt::Display,
{
    for test in correct_tests {
        let out = match test.0.parse::<T>() {
            Ok(t) => t.to_string(),
            Err(e) => panic!("Parsing \"{}\" resulted in error \"{}\"", test.0, e),
        };
        assert_eq!(out, test.1)
    }
}
