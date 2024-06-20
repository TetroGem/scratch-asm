use std::sync::Arc;

use anyhow::bail;

use super::asm_token::AsmToken;
use super::command::{Command, CommandDiscriminants};
use super::TokenVerifier;
use crate::parser::command::ApTag;

pub struct NewChunk {
    pub tag: Option<Arc<String>>,
}

pub struct ParseCommandRes {
    pub command: Command,
    pub new_chunk: Option<NewChunk>,
}

pub trait ParseCommand {
    fn parse<I: Iterator<Item = AsmToken>>(
        self,
        t: &mut TokenVerifier<I>,
    ) -> anyhow::Result<ParseCommandRes>;
}

impl ParseCommand for CommandDiscriminants {
    fn parse<I: Iterator<Item = AsmToken>>(
        self,
        t: &mut TokenVerifier<I>,
    ) -> anyhow::Result<ParseCommandRes> {
        // res constructors
        fn link(command: Command) -> ParseCommandRes {
            ParseCommandRes { command, new_chunk: None }
        }

        fn split_to_jump(command: Command) -> ParseCommandRes {
            ParseCommandRes { command, new_chunk: Some(NewChunk { tag: None }) }
        }

        fn split_to_tag(command: Command, chunk_tag: &ApTag) -> ParseCommandRes {
            ParseCommandRes {
                command,
                new_chunk: Some(NewChunk { tag: Some(Arc::clone(&chunk_tag.inner)) }),
            }
        }

        let res = match self {
            Self::Clear => link(Command::Clear),
            Self::Out => link(Command::Out { msg: t.next_val()? }),
            Self::OutLine => link(Command::OutLine),
            Self::In => link(Command::In { dest: t.next_val()? }),
            Self::Copy => link(Command::Copy { val: t.next_val()?, dest: t.next_val()? }),
            Self::Tag => split_to_tag(Command::Tag, &t.next_tag()?),
            Self::Jump => split_to_jump(Command::Jump { tag: t.next_tag()? }),
            Self::JumpIf => {
                split_to_jump(Command::JumpIf { cond: t.next_val()?, tag: t.next_tag()? })
            },
            Self::Call => {
                split_to_jump(Command::Call { function: t.next_fn()?, args: t.rest_vals()? })
            },
            Self::CallIf => split_to_jump(Command::CallIf {
                cond: t.next_val()?,
                function: t.next_fn()?,
                args: t.rest_vals()?,
            }),
            Self::Mod => link(Command::Mod {
                left: t.next_val()?,
                right: t.next_val()?,
                dest: t.next_val()?,
            }),
            Self::Eq => {
                link(Command::Eq { left: t.next_val()?, right: t.next_val()?, dest: t.next_val()? })
            },
            Self::Gt => {
                link(Command::Gt { left: t.next_val()?, right: t.next_val()?, dest: t.next_val()? })
            },
            Self::Lt => {
                link(Command::Lt { left: t.next_val()?, right: t.next_val()?, dest: t.next_val()? })
            },
            Self::Add => link(Command::Add {
                left: t.next_val()?,
                right: t.next_val()?,
                dest: t.next_val()?,
            }),
            Self::Sub => link(Command::Sub {
                left: t.next_val()?,
                right: t.next_val()?,
                dest: t.next_val()?,
            }),
            Self::Mul => link(Command::Mul {
                left: t.next_val()?,
                right: t.next_val()?,
                dest: t.next_val()?,
            }),
            Self::Div => link(Command::Div {
                left: t.next_val()?,
                right: t.next_val()?,
                dest: t.next_val()?,
            }),
            Self::Rand => {
                link(Command::Rand { min: t.next_val()?, max: t.next_val()?, dest: t.next_val()? })
            },
            Self::Time => link(Command::Time { dest: t.next_val()? }),
            Self::Wait => link(Command::Wait { seconds: t.next_val()? }),
        };

        Ok(res)
    }
}
