use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use anyhow::{anyhow, bail, Context};
use asm_token::{parse_asm_token, AsmToken, AsmVal, Positioned};
use command::{Ap, ApFn, ApTag, ApVal, Command, CommandDiscriminants, LineNumbered};
use parse_command::{ParseCommand, ParseCommandRes};
use scope::{Chunk, Scope, ScopeHeader, ScopeKind, Var};
use strum::IntoEnumIterator;

use crate::util::{spellcheck, PushGet};

pub mod asm_token;
pub mod command;
mod parse_command;
pub mod scope;

#[derive(Debug)]
pub struct Parsed {
    pub scopes: Vec<Scope>,
}

pub fn parse(input: String) -> anyhow::Result<Parsed> {
    let mut scopes: Vec<Scope> = Vec::default();
    let mut scope: Option<&mut Scope> = None;

    for (i, line) in input.lines().enumerate() {
        let line_num = i + 1;

        let (tokens, _comment) = line
            .split_once(';')
            .map(|(tokens, comment)| (tokens, Some(comment)))
            .unwrap_or((line, None));

        let tokens = tokens.trim();

        if tokens.is_empty() {
            continue;
        }

        let res = parse_instruction(tokens).with_context(|| format!("On line {}", line_num))?;

        match res {
            ParseInstructionRes::AppendScope(res) => {
                let Some(current_scope) = scope else {
                    bail!("Cannot execute commands outside of a scope")
                };

                let chunk = match current_scope.chunks.last_mut() {
                    Some(chunk) => chunk,
                    None => current_scope.chunks.push_get(Chunk::new()),
                };

                chunk.body.push(LineNumbered { inner: res.command, line_num });

                if let Some(new_chunk_data) = res.new_chunk {
                    let new_chunk = Chunk::new();

                    if let Some(tag) = new_chunk_data.tag {
                        current_scope.tag_to_chunk_uuid.insert(Arc::clone(&tag), new_chunk.uuid);
                    }

                    current_scope.chunks.push(new_chunk);
                }

                scope = Some(current_scope);
            },
            ParseInstructionRes::CreateScope(new_scope) => {
                scope = Some(scopes.push_get(new_scope));
            },
        }
    }

    Ok(Parsed { scopes })
}

pub enum ParseInstructionRes {
    CreateScope(Scope),
    AppendScope(ParseCommandRes),
}

fn parse_instruction(tokens: &str) -> anyhow::Result<ParseInstructionRes> {
    let mut parts = tokens.split_whitespace();

    let Some(kw) = parts.next() else { bail!("Expected instruction keyword") };
    let kw = kw.to_lowercase();

    let res = try_parse_scope(&kw, &mut parts);

    match res {
        Some(scope_res) => {
            let new_scope = scope_res?;
            Ok(ParseInstructionRes::CreateScope(new_scope))
        },
        None => {
            let command_kind = CommandDiscriminants::iter().find(|c| c.keyword() == kw);
            match command_kind {
                Some(command_kind) => {
                    let mut parts = parts.collect();
                    let tokens = parse_tokens(&mut parts)?;
                    let mut t = TokenVerifier::new(tokens.into_iter());
                    command_kind.parse(&mut t).map(ParseInstructionRes::AppendScope)
                },
                None => {
                    let mut err_msg = format!("Unknown instruction keyword: '{}'", kw);

                    if let Some(maybe_kw) = spellcheck(
                        &kw,
                        CommandDiscriminants::keywords().into_iter().chain(["main", "fn"]),
                    ) {
                        err_msg += &format!("; Perhaps you meant '{}'?", maybe_kw.term);
                    }

                    bail!(err_msg);
                },
            }
        },
    }
}

fn try_parse_scope<'a>(
    kw: &str,
    parts: &mut impl Iterator<Item = &'a str>,
) -> Option<anyhow::Result<Scope>> {
    let scope_kind = match kw {
        "main" => ScopeKind::Main,
        "fn" => {
            let Some(name) = parts.next() else {
                return Some(Err(anyhow!("Function name must be provided after FN keyword")));
            };
            if !name.starts_with('@') {
                return Some(Err(anyhow!("Function names must start with @")));
            }
            ScopeKind::Fn { name: name.into() }
        },
        _ => return None,
    };

    Some(parse_scope(scope_kind, parts))
}

fn parse_scope<'a>(
    scope_kind: ScopeKind,
    parts: impl Iterator<Item = &'a str>,
) -> anyhow::Result<Scope> {
    let vars = parts.enumerate().map(|(i, name)| {
        if !name.starts_with("$") {
            bail!("Variable name must start with $");
        }
        Ok(Var { name: name.into(), offset: i })
    });
    let vars = vars.collect::<anyhow::Result<Vec<_>>>()?;

    let scope = Scope {
        header: Arc::new(ScopeHeader { kind: scope_kind, vars }),
        chunks: Vec::from([]),
        tag_to_chunk_uuid: HashMap::default(),
    };

    Ok(scope)
}

struct TokenVerifier<I: Iterator<Item = AsmToken>> {
    tokens: I,
    pos: usize,
}

impl<I: Iterator<Item = AsmToken>> TokenVerifier<I> {
    fn new(tokens: I) -> Self {
        Self { tokens, pos: 1 }
    }

    fn next_val(&mut self) -> anyhow::Result<ApVal> {
        if let Some(AsmToken::Val(val)) = self.tokens.next() {
            let val = Positioned { inner: val, pos: self.pos };
            self.pos += 1;
            Ok(Arc::new(val))
        } else {
            bail!("Expected value in position {}", self.pos)
        }
    }

    fn next_fn(&mut self) -> anyhow::Result<ApFn> {
        if let Some(AsmToken::Fn(fn_name)) = self.tokens.next() {
            let fn_name = Positioned { inner: fn_name, pos: self.pos };
            self.pos += 1;
            Ok(Arc::new(fn_name))
        } else {
            bail!("Expected function in position {}", self.pos)
        }
    }

    fn next_tag(&mut self) -> anyhow::Result<ApTag> {
        if let Some(AsmToken::Tag(tag_name)) = self.tokens.next() {
            let tag_name = Positioned { inner: tag_name, pos: self.pos };
            self.pos += 1;
            Ok(Arc::new(tag_name))
        } else {
            bail!("Expected tag in position {}", self.pos)
        }
    }

    fn rest_vals(&mut self) -> anyhow::Result<Vec<ApVal>> {
        let mut vals = Vec::default();
        for tok in &mut self.tokens {
            if let AsmToken::Val(val) = tok {
                let val = Positioned { inner: val, pos: self.pos };
                self.pos += 1;
                vals.push(Arc::new(val));
            } else {
                bail!("Expected value in position {}", self.pos)
            }
        }

        Ok(vals)
    }
}

fn parse_tokens(parts: &mut VecDeque<&str>) -> anyhow::Result<Vec<AsmToken>> {
    let mut vals = Vec::default();
    while !parts.is_empty() {
        let val = parse_asm_token(parts).with_context(|| {
            let position = vals.len() + 1;
            format!("Value in position {}", position)
        })?;
        vals.push(val);
    }

    Ok(vals)
}
