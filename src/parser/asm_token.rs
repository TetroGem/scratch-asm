use std::collections::VecDeque;
use std::mem::{replace, take};
use std::sync::Arc;

use anyhow::bail;

use crate::util::SplitAtChecked;

#[derive(Debug)]
pub enum AsmVal {
    Literal(Arc<String>),
    Variable(Arc<String>),
    StackDeref(Box<AsmVal>),
}

#[derive(Debug)]
pub enum AsmToken {
    Val(AsmVal),
    Fn(Arc<String>),
    Tag(Arc<String>),
}

#[derive(Debug)]
pub struct Positioned<T> {
    pub inner: T,
    pub pos: usize,
}

pub fn parse_asm_token(parts: &mut VecDeque<&str>) -> anyhow::Result<AsmToken> {
    let Some(part) = parts.pop_front() else { bail!("Expected value") };
    let Some(first_char) = part.chars().next() else {
        bail!("Value had no characters and could not be parsed");
    };

    match first_char {
        '"' => {
            parts.push_front(part);
            parse_literal(parts).map(Arc::new).map(AsmVal::Literal).map(AsmToken::Val)
        },
        '$' => parse_variable_name(part)
            .map(Into::into)
            .map(Arc::new)
            .map(AsmVal::Variable)
            .map(AsmToken::Val),
        '*' => parse_stack_deref(part, parts).map(AsmVal::StackDeref).map(AsmToken::Val),
        '@' => parse_function_name(part).map(Into::into).map(Arc::new).map(AsmToken::Fn),
        '#' => parse_tag_name(part).map(Into::into).map(Arc::new).map(AsmToken::Tag),
        _ => {
            let mut err_msg = format!("Unable to parse token (Found: {})", part);

            match part.chars().all(|c| c == '.' || c.is_ascii_digit()) {
                true => {
                    err_msg += &format!(
                        "; Did you mean to wrap this number in quotes to make it a literal? (\"{}\")",
                        part
                    )
                },
                false => {
                    err_msg += &format!(
                        "; Did you mean to write a literal (\"{}\"), a variable (${}), or a tag (#{})? ",
                        part, part, part
                    )
                },
            }

            bail!(err_msg)
        },
    }
}

fn parse_literal(parts: &mut VecDeque<&str>) -> anyhow::Result<String> {
    let mut open = false;
    let mut escaped_next = false;
    let mut string = String::new();
    let mut parsed_one_part = false;

    loop {
        let Some(part) = parts.pop_front() else {
            bail!("Literal did not terminate. Are you missing a closing quote?");
        };
        let mut part = String::from(part);
        if replace(&mut parsed_one_part, true) {
            part = String::from(" ") + &part;
        }

        for char in part.chars() {
            let escaped = take(&mut escaped_next);
            match char {
                '"' if !escaped => {
                    if open {
                        return Ok(string);
                    } else {
                        open = true;
                    }
                },
                '\\' if !escaped => {
                    escaped_next = true;
                },
                char => {
                    if !open {
                        bail!("Literal characters must be within quotes");
                    }

                    string.push(char)
                },
            }
        }
    }
}

fn parse_variable_name(part: &str) -> anyhow::Result<&str> {
    if !part.starts_with('$') {
        bail!("Variable names must start with $");
    }
    Ok(part)
}

fn parse_stack_deref<'a>(
    part: &'a str,
    rest: &mut VecDeque<&'a str>,
) -> anyhow::Result<Box<AsmVal>> {
    let Some(("*", val)) = SplitAtChecked::split_at_checked(part, 1) else {
        bail!("Dereference should consist of * with a value after");
    };

    rest.push_front(val);
    let AsmToken::Val(val) = parse_asm_token(rest)? else {
        bail!("Can only dereference values, not functions or tags")
    };

    Ok(Box::new(val))
}

fn parse_function_name(part: &str) -> anyhow::Result<&str> {
    if !part.starts_with('@') {
        bail!("Function names must start with @");
    }
    Ok(part)
}

fn parse_tag_name(part: &str) -> anyhow::Result<&str> {
    if !part.starts_with('#') {
        bail!("Tag names must start with #");
    }
    Ok(part)
}
