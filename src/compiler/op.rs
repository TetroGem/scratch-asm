use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{bail, Context};
use uuid::Uuid;

use super::{
    Block, Body, BoolExpr, BoolExprKind, List, LitExpr, LitExprKind, Message, Op, ScopeTagManager,
};
use crate::compiler::create_message_name;
use crate::parser::command::{ApFn, Command};
use crate::parser::scope::{ScopeKind, Var};
use crate::util::spellcheck;

pub fn command_kind_to_ops(
    kind: Command,
    stdout: &Arc<List>,
    stack: &Arc<List>,
    args: &Arc<List>,
    vars: &[Var],
    tags: &ScopeTagManager,
    scope_kind: &ScopeKind,
    next_message: Option<&Arc<Message>>,
    message_name_to_message: &HashMap<String, Arc<Message>>,
) -> anyhow::Result<Vec<Op>> {
    let get_message = |message_name: &str| {
        let Some(message) = message_name_to_message.get(message_name) else {
            let mut err_msg = format!("No function named '{}' found", message_name);

            if let Some(maybe_fn) = spellcheck(
                message_name,
                message_name_to_message.keys().map(|k: &String| k.as_str()),
            ) {
                err_msg += &format!("; Perhaps you meant '{}'?", maybe_fn.term);
            }

            bail!(err_msg);
        };

        Ok(message)
    };

    let get_message = |function: &ApFn| {
        get_message(&function.inner).with_context(|| format!("In position {}", function.pos))
    };

    match kind {
        Command::Clear => {
            Ok(Vec::from([Op::ListClear { list: Arc::clone(stdout) }, Op::ListPush {
                list: Arc::clone(stdout),
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::new(String::new())),
                },
            }]))
        },
        Command::Out { msg } => Ok(Vec::from([Op::ListSet {
            list: Arc::clone(stdout),
            index: LitExpr {
                uuid: Uuid::new_v4(),
                kind: LitExprKind::ListLen(Arc::clone(&stdout)),
            },
            val: LitExpr {
                uuid: Uuid::new_v4(),
                kind: LitExprKind::Join(
                    Box::new(LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::ListIndex {
                            list: Arc::clone(&stdout),
                            index: Box::new(LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::ListLen(Arc::clone(&stdout)),
                            }),
                        },
                    }),
                    Box::new(LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::from_ap(&msg, &stack, vars)?,
                    }),
                ),
            },
        }])),
        Command::OutLine => Ok(Vec::from([Op::ListPush {
            list: Arc::clone(&stdout),
            val: LitExpr {
                kind: LitExprKind::Literal(Arc::new(String::new())),
                uuid: Uuid::new_v4(),
            },
        }])),
        Command::In { dest } => Ok(Vec::from([Op::Ask, Op::ListSet {
            list: Arc::clone(&stack),
            index: LitExpr {
                uuid: Uuid::new_v4(),
                kind: LitExprKind::from_ap(&dest, &stack, vars)?,
            },
            val: LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::Answer },
        }])),
        Command::Copy { val, dest: addr } => Ok(Vec::from([Op::ListSet {
            list: Arc::clone(&stack),
            index: LitExpr {
                kind: LitExprKind::from_ap(&addr, &stack, vars)?,
                uuid: Uuid::new_v4(),
            },
            val: LitExpr { kind: LitExprKind::from_ap(&val, &stack, vars)?, uuid: Uuid::new_v4() },
        }])),
        Command::Tag => {
            let mut link_ops = Vec::default();
            if let Some(next_message) = &next_message {
                link_ops.push(Op::ListPush {
                    list: Arc::clone(&stack),
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::clone(&next_message.name)),
                    },
                });
            }

            Ok(link_ops)
        },
        Command::Jump { tag } => {
            let chunk_uuid = tags.get_chunk_uuid(&tag)?;
            let message_name = create_message_name(&scope_kind, chunk_uuid, false);
            let broadcast_ops = [Op::ListPush {
                list: Arc::clone(&stack),
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::new(message_name)),
                },
            }];

            let call_ops = broadcast_ops.into_iter().collect();

            Ok(call_ops)
        },
        Command::JumpIf { cond, tag } => {
            let chunk_uuid = tags.get_chunk_uuid(&tag)?;

            let cond =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&cond, &stack, vars)? };

            let mut link_ops = Vec::default();
            if let Some(next_message) = &next_message {
                link_ops.push(Op::ListPush {
                    list: Arc::clone(&stack),
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::clone(&next_message.name)),
                    },
                });
            }

            let message_name = create_message_name(&scope_kind, chunk_uuid, false);
            let broadcast_ops = [Op::ListPush {
                list: Arc::clone(&stack),
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::new(message_name)),
                },
            }];

            let then_body = Body {
                blocks: broadcast_ops
                    .into_iter()
                    .map(|op| Block { uuid: Uuid::new_v4(), op })
                    .collect(),
            };

            let else_body = Body {
                blocks: link_ops.into_iter().map(|op| Block { uuid: Uuid::new_v4(), op }).collect(),
            };

            Ok(Vec::from([Op::IfElse {
                cond: BoolExpr {
                    uuid: Uuid::new_v4(),
                    kind: BoolExprKind::Not(Box::new(BoolExpr {
                        uuid: Uuid::new_v4(),
                        kind: BoolExprKind::Eq(
                            Box::new(cond),
                            Box::new(LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("0".into())),
                            }),
                        ),
                    })),
                },
                then: then_body,
                otherwise: else_body,
            }]))
        },
        Command::Call { function, args: fn_args } => {
            let message = get_message(&function)?;

            let arg_ops = fn_args.into_iter().map(|arg| {
                let arg = LitExprKind::from_ap(&arg, &stack, vars)?;
                let op = Op::ListPush {
                    list: Arc::clone(&args),
                    val: LitExpr { kind: arg, uuid: Uuid::new_v4() },
                };

                Ok(op)
            });

            let arg_ops = arg_ops.collect::<anyhow::Result<Vec<_>>>()?;

            let mut link_ops = Vec::default();
            if let Some(next_message) = &next_message {
                link_ops.push(Op::ListPush {
                    list: Arc::clone(&stack),
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::clone(&next_message.name)),
                    },
                });
            }

            let broadcast_ops = [Op::ListPush {
                list: Arc::clone(&stack),
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::clone(&message.name)),
                },
            }];

            let call_ops = arg_ops.into_iter().chain(link_ops).chain(broadcast_ops).collect();

            Ok(call_ops)
        },
        Command::CallIf { cond, function, args: fn_args } => {
            let message = get_message(&function)?;

            let cond =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&cond, &stack, vars)? };

            let arg_ops = fn_args.into_iter().map(|arg| {
                let arg = LitExprKind::from_ap(&arg, &stack, vars)?;
                let op = Op::ListPush {
                    list: Arc::clone(&args),
                    val: LitExpr { kind: arg, uuid: Uuid::new_v4() },
                };

                Ok(op)
            });

            let arg_ops = arg_ops.collect::<anyhow::Result<Vec<_>>>()?;

            let mut then_link_ops = Vec::default();
            let mut else_link_ops = Vec::default();
            if let Some(next_message) = &next_message {
                then_link_ops.push(Op::ListPush {
                    list: Arc::clone(&stack),
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::clone(&next_message.name)),
                    },
                });
                else_link_ops.push(Op::ListPush {
                    list: Arc::clone(&stack),
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::clone(&next_message.name)),
                    },
                });
            }

            let broadcast_ops = [Op::ListPush {
                list: Arc::clone(&stack),
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::clone(&message.name)),
                },
            }];

            let then_ops = arg_ops.into_iter().chain(then_link_ops).chain(broadcast_ops);
            let then_blocks = then_ops.map(|op| Block { uuid: Uuid::new_v4(), op }).collect();
            let then_body = Body { blocks: then_blocks };

            let else_blocks =
                else_link_ops.into_iter().map(|op| Block { uuid: Uuid::new_v4(), op }).collect();
            let else_body = Body { blocks: else_blocks };

            Ok(Vec::from([Op::IfElse {
                cond: BoolExpr {
                    uuid: Uuid::new_v4(),
                    kind: BoolExprKind::Not(Box::new(BoolExpr {
                        uuid: Uuid::new_v4(),
                        kind: BoolExprKind::Eq(
                            Box::new(cond),
                            Box::new(LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("0".into())),
                            }),
                        ),
                    })),
                },
                then: then_body,
                otherwise: else_body,
            }]))
        },
        Command::Mod { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Mod(Box::new(left), Box::new(right)),
                },
            }]))
        },
        Command::Eq { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };

            // need unique blocks for each branch, they can't share uuids or they'd be the
            // same block in two places
            let then_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };
            let else_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::IfElse {
                cond: BoolExpr {
                    uuid: Uuid::new_v4(),
                    kind: BoolExprKind::Eq(Box::new(left), Box::new(right)),
                },
                then: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: then_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("1".into())),
                            },
                        },
                    }]),
                },
                otherwise: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: else_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("0".into())),
                            },
                        },
                    }]),
                },
            }]))
        },
        Command::Gt { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };

            // need unique blocks for each branch, they can't share uuids or they'd be the
            // same block in two places
            let then_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };
            let else_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::IfElse {
                cond: BoolExpr {
                    uuid: Uuid::new_v4(),
                    kind: BoolExprKind::Gt(Box::new(left), Box::new(right)),
                },
                then: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: then_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("1".into())),
                            },
                        },
                    }]),
                },
                otherwise: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: else_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("0".into())),
                            },
                        },
                    }]),
                },
            }]))
        },
        Command::Lt { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };

            // need unique blocks for each branch, they can't share uuids or they'd be the
            // same block in two places
            let then_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };
            let else_dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::IfElse {
                cond: BoolExpr {
                    uuid: Uuid::new_v4(),
                    kind: BoolExprKind::Lt(Box::new(left), Box::new(right)),
                },
                then: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: then_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("1".into())),
                            },
                        },
                    }]),
                },
                otherwise: Body {
                    blocks: Vec::from([Block {
                        uuid: Uuid::new_v4(),
                        op: Op::ListSet {
                            list: Arc::clone(&stack),
                            index: else_dest,
                            val: LitExpr {
                                uuid: Uuid::new_v4(),
                                kind: LitExprKind::Literal(Arc::new("0".into())),
                            },
                        },
                    }]),
                },
            }]))
        },
        Command::Add { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Add(Box::new(left), Box::new(right)),
                },
            }]))
        },
        Command::Sub { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Sub(Box::new(left), Box::new(right)),
                },
            }]))
        },
        Command::Mul { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Mul(Box::new(left), Box::new(right)),
                },
            }]))
        },
        Command::Div { left, right, dest } => {
            let left =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&left, &stack, vars)? };
            let right =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&right, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Div(Box::new(left), Box::new(right)),
                },
            }]))
        },
        Command::Rand { min, max, dest } => {
            let min =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&min, &stack, vars)? };
            let max =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&max, &stack, vars)? };
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Rand(Box::new(min), Box::new(max)),
                },
            }]))
        },
        Command::Time { dest } => {
            let dest =
                LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_ap(&dest, &stack, vars)? };

            Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: dest,
                val: LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::Timer },
            }]))
        },
        Command::Wait { seconds } => {
            let seconds = LitExpr {
                uuid: Uuid::new_v4(),
                kind: LitExprKind::from_ap(&seconds, &stack, vars)?,
            };

            Ok(Vec::from([Op::Wait { seconds }]))
        },
    }
}
