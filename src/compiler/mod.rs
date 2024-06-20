mod op;

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{bail, Context};
use itertools::Itertools;
use op::command_kind_to_ops;
use uuid::Uuid;

use crate::parser::asm_token::AsmVal;
use crate::parser::command::{ApTag, ApVal, Command};
use crate::parser::scope::{Chunk, ScopeHeader, ScopeKind, Var};
use crate::parser::Parsed;
use crate::util::spellcheck;

fn link_to_json(link: Option<Uuid>) -> String {
    match link {
        None => "null".into(),
        Some(link) => format!(r#""{}""#, link),
    }
}

struct BlockJsonMaker {
    uuid: Uuid,
    op_code: &'static str,
    next: Option<Uuid>,
    prev: Option<Uuid>,
    inputs: String,
    fields: String,
}

impl BlockJsonMaker {
    pub fn to_json(&self) -> String {
        format!(
            r#"
            "{}": {{
                "opcode": "{}",
                "next": {},
                "parent": {},
                "inputs": {{{}}},
                "fields": {{{}}},
                "shadow": false,
                "topLevel": false
            }}
        "#,
            self.uuid,
            self.op_code,
            link_to_json(self.next),
            link_to_json(self.prev),
            self.inputs,
            self.fields,
        )
    }
}

#[derive(Debug)]
pub struct List {
    pub uuid: Uuid,
    pub name: String,
}

#[derive(Debug)]
pub enum LitExprKind {
    Literal(Arc<String>),
    ListLen(Arc<List>),
    ListIndex { list: Arc<List>, index: Box<LitExpr> },
    Add(Box<LitExpr>, Box<LitExpr>),
    Sub(Box<LitExpr>, Box<LitExpr>),
    Mul(Box<LitExpr>, Box<LitExpr>),
    Div(Box<LitExpr>, Box<LitExpr>),
    Mod(Box<LitExpr>, Box<LitExpr>),
    Answer,
    Join(Box<LitExpr>, Box<LitExpr>),
    Rand(Box<LitExpr>, Box<LitExpr>),
    Timer,
}

fn get_stack_offset(var: &Arc<String>, defined: &[Var]) -> anyhow::Result<usize> {
    let Some(var) = defined.iter().find(|v| v.name == **var) else {
        let mut err_msg = format!("No variable named '{}' found within this scope", var);

        if let Some(maybe_tag) = spellcheck(&var, defined.iter().map(|v| v.name.as_str())) {
            err_msg += &format!("; Perhaps you meant '{}'?", maybe_tag.term);
        }

        bail!(err_msg);
    };

    Ok(var.offset)
}

impl LitExprKind {
    fn from_ap(value: &ApVal, stack: &Arc<List>, vars: &[Var]) -> anyhow::Result<Self> {
        LitExprKind::from_asm(&value.inner, stack, vars)
            .with_context(|| format!("In position {}", value.pos))
    }

    fn from_asm(value: &AsmVal, stack: &Arc<List>, vars: &[Var]) -> anyhow::Result<Self> {
        let expr = match value {
            AsmVal::Literal(l) => LitExprKind::Literal(Arc::clone(l)),
            AsmVal::Variable(var_name) => {
                let offset = get_stack_offset(var_name, vars)?;
                LitExprKind::Sub(
                    Box::new(LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::ListLen(Arc::clone(stack)),
                    }),
                    Box::new(LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Literal(Arc::new(offset.to_string())),
                    }),
                )
            },
            AsmVal::StackDeref(addr) => {
                let addr = LitExprKind::from_asm(addr, stack, vars)?;
                LitExprKind::ListIndex {
                    list: Arc::clone(stack),
                    index: Box::new(LitExpr { uuid: Uuid::new_v4(), kind: addr }),
                }
            },
        };

        Ok(expr)
    }
}

#[derive(Debug)]
pub struct LitExpr {
    pub uuid: Uuid,
    pub kind: LitExprKind,
}

#[derive(Debug)]
pub enum BoolExprKind {
    Not(Box<BoolExpr>),
    Eq(Box<LitExpr>, Box<LitExpr>),
    Gt(Box<LitExpr>, Box<LitExpr>),
    Lt(Box<LitExpr>, Box<LitExpr>),
}

#[derive(Debug)]
pub struct BoolExpr {
    pub uuid: Uuid,
    pub kind: BoolExprKind,
}

impl BoolExpr {
    pub fn data(&self, parent: Uuid) -> ExprData {
        match &self.kind {
            BoolExprKind::Not(val) => {
                let val_data = val.data(self.uuid);

                let not_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_not",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(r#""OPERAND": {}"#, val_data.val),
                    fields: String::new(),
                };

                let mut deps = Vec::from([not_block.to_json()]);
                deps.extend(val_data.deps);

                ExprData { val: format!(r#"[2, "{}"]"#, self.uuid), deps }
            },
            BoolExprKind::Eq(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let eq_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_equals",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "OPERAND1": {},
                        "OPERAND2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([eq_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[2, "{}"]"#, self.uuid), deps }
            },
            BoolExprKind::Gt(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let eq_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_gt",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "OPERAND1": {},
                        "OPERAND2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([eq_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[2, "{}"]"#, self.uuid), deps }
            },
            BoolExprKind::Lt(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let eq_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_lt",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "OPERAND1": {},
                        "OPERAND2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([eq_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[2, "{}"]"#, self.uuid), deps }
            },
        }
    }
}

pub struct ExprData {
    val: String,
    deps: Vec<String>,
}

impl LitExpr {
    pub fn data(&self, parent: Uuid) -> ExprData {
        match &self.kind {
            LitExprKind::Literal(s) => {
                ExprData { val: format!(r#"[1, [10, "{}"]]"#, s), deps: Vec::default() }
            },
            LitExprKind::ListLen(list) => {
                let list_len_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "data_lengthoflist",
                    next: None,
                    prev: Some(parent),
                    inputs: String::new(),
                    fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                };

                ExprData {
                    val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid),
                    deps: Vec::from([list_len_block.to_json()]),
                }
            },
            LitExprKind::Sub(a, b) => {
                let a_data = a.data(self.uuid);
                let b_data = b.data(self.uuid);

                let sub_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_subtract",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "NUM1": {},
                        "NUM2": {}
                    "#,
                        a_data.val, b_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([sub_block.to_json()]);
                deps.extend(a_data.deps);
                deps.extend(b_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::ListIndex { list, index } => {
                let index_data = index.data(self.uuid);

                let indexing_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "data_itemoflist",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(r#""INDEX": {}"#, index_data.val),
                    fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                };

                let mut deps = Vec::from([indexing_block.to_json()]);
                deps.extend(index_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Mod(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let mod_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_mod",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "NUM1": {},
                        "NUM2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([mod_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Add(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let mod_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_add",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "NUM1": {},
                        "NUM2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([mod_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Mul(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let mod_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_multiply",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "NUM1": {},
                        "NUM2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([mod_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Div(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let mod_block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_divide",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "NUM1": {},
                        "NUM2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([mod_block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Answer => {
                let block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "sensing_answer",
                    next: None,
                    prev: Some(parent),
                    inputs: String::new(),
                    fields: String::new(),
                };

                ExprData {
                    val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid),
                    deps: Vec::from([block.to_json()]),
                }
            },
            LitExprKind::Join(left, right) => {
                let left_data = left.data(self.uuid);
                let right_data = right.data(self.uuid);

                let block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_join",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "STRING1": {},
                        "STRING2": {}
                    "#,
                        left_data.val, right_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([block.to_json()]);
                deps.extend(left_data.deps);
                deps.extend(right_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Rand(min, max) => {
                let min_data = min.data(self.uuid);
                let max_data = max.data(self.uuid);

                let block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "operator_random",
                    next: None,
                    prev: Some(parent),
                    inputs: format!(
                        r#"
                        "FROM": {},
                        "TO": {}
                    "#,
                        min_data.val, max_data.val
                    ),
                    fields: String::new(),
                };

                let mut deps = Vec::from([block.to_json()]);
                deps.extend(min_data.deps);
                deps.extend(max_data.deps);

                ExprData { val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid), deps }
            },
            LitExprKind::Timer => {
                let block = BlockJsonMaker {
                    uuid: self.uuid,
                    op_code: "sensing_timer",
                    next: None,
                    prev: Some(parent),
                    inputs: String::new(),
                    fields: String::new(),
                };

                ExprData {
                    val: format!(r#"[3, "{}", [7, ""]]"#, self.uuid),
                    deps: Vec::from([block.to_json()]),
                }
            },
        }
    }
}

#[derive(Debug)]
pub struct Message {
    pub uuid: Uuid,
    pub name: Arc<String>,
}

#[derive(Debug)]
pub enum BroadcastInput {
    Message(Arc<Message>),
    Expr(LitExpr),
}

#[derive(Debug)]
pub enum Op {
    OnFlag,
    OnMessage(Arc<Message>),
    ListPush { list: Arc<List>, val: LitExpr },
    ListClear { list: Arc<List> },
    ListRemove { list: Arc<List>, index: LitExpr },
    ListSet { list: Arc<List>, index: LitExpr, val: LitExpr },
    BroadcastSync(BroadcastInput),
    BroadcastAsync(BroadcastInput),
    If { cond: BoolExpr, then: Body },
    IfElse { cond: BoolExpr, then: Body, otherwise: Body },
    Ask,
    Wait { seconds: LitExpr },
}

#[derive(Debug)]
pub struct Block {
    pub uuid: Uuid,
    pub op: Op,
}

pub struct BlockData {
    pub op_code: &'static str,
    pub inputs: String,
    pub fields: String,
    pub deps: Vec<String>,
}

impl Block {
    pub fn data(&self) -> anyhow::Result<BlockData> {
        let block_data = match &self.op {
            Op::OnFlag => BlockData {
                op_code: "event_whenflagclicked",
                inputs: String::new(),
                fields: String::new(),
                deps: Vec::default(),
            },
            Op::OnMessage(message) => BlockData {
                op_code: "event_whenbroadcastreceived",
                inputs: String::new(),
                fields: format!(r#""BROADCAST_OPTION": ["{}", "{}"]"#, message.name, message.uuid),
                deps: Vec::default(),
            },
            Op::ListPush { list, val } => {
                let val_data = val.data(self.uuid);
                BlockData {
                    op_code: "data_addtolist",
                    inputs: format!(r#""ITEM": {}"#, val_data.val),
                    fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                    deps: val_data.deps,
                }
            },
            Op::ListClear { list } => BlockData {
                op_code: "data_deletealloflist",
                inputs: String::new(),
                fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                deps: Vec::default(),
            },
            Op::ListRemove { list, index } => {
                let index_data = index.data(self.uuid);
                BlockData {
                    op_code: "data_deleteoflist",
                    inputs: format!(r#""INDEX": {}"#, index_data.val),
                    fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                    deps: index_data.deps,
                }
            },
            Op::ListSet { list, index, val } => {
                let index_data = index.data(self.uuid);
                let val_data = val.data(self.uuid);

                let mut deps = Vec::default();
                deps.extend(index_data.deps);
                deps.extend(val_data.deps);

                BlockData {
                    op_code: "data_replaceitemoflist",
                    inputs: format!(
                        r#"
                        "INDEX": {},
                        "ITEM": {}
                    "#,
                        index_data.val, val_data.val
                    ),
                    fields: format!(r#""LIST": ["{}", "{}"]"#, list.name, list.uuid),
                    deps,
                }
            },
            Op::BroadcastSync(input) => {
                let (input, deps) = match input {
                    BroadcastInput::Message(message) => (
                        format!(r#"[1, [11, "{}", "{}"]]"#, message.name, message.uuid),
                        Vec::default(),
                    ),
                    BroadcastInput::Expr(expr) => {
                        let expr_data = expr.data(self.uuid);
                        (expr_data.val, expr_data.deps)
                    },
                };

                BlockData {
                    op_code: "event_broadcastandwait",
                    inputs: format!(r#""BROADCAST_INPUT": {}"#, input),
                    fields: String::new(),
                    deps,
                }
            },
            Op::BroadcastAsync(input) => {
                let (input, deps) = match input {
                    BroadcastInput::Message(message) => (
                        format!(r#"[1, [11, "{}", "{}"]]"#, message.name, message.uuid),
                        Vec::default(),
                    ),
                    BroadcastInput::Expr(expr) => {
                        let expr_data = expr.data(self.uuid);
                        (expr_data.val, expr_data.deps)
                    },
                };

                BlockData {
                    op_code: "event_broadcast",
                    inputs: format!(r#""BROADCAST_INPUT": {}"#, input),
                    fields: String::new(),
                    deps,
                }
            },
            Op::If { cond, then } => {
                let cond_data = cond.data(self.uuid);
                let then_data = then.data(None)?;

                let mut deps = Vec::default();
                deps.extend(cond_data.deps);
                deps.extend(then_data.deps);

                BlockData {
                    op_code: "control_if",
                    inputs: format!(
                        r#"
                            "CONDITION": {},
                            "SUBSTACK": [2, "{}"]
                        "#,
                        cond_data.val, then_data.first_uuid
                    ),
                    fields: String::new(),
                    deps,
                }
            },
            Op::IfElse { cond, then, otherwise } => {
                let cond_data = cond.data(self.uuid);
                let then_data = then.data(None)?;
                let else_data = otherwise.data(None)?;

                let mut deps = Vec::default();
                deps.extend(cond_data.deps);
                deps.extend(then_data.deps);
                deps.extend(else_data.deps);

                BlockData {
                    op_code: "control_if_else",
                    inputs: format!(
                        r#"
                            "CONDITION": {},
                            "SUBSTACK": [2, "{}"],
                            "SUBSTACK2": [2, "{}"]
                        "#,
                        cond_data.val, then_data.first_uuid, else_data.first_uuid
                    ),
                    fields: String::new(),
                    deps,
                }
            },
            Op::Ask => BlockData {
                op_code: "sensing_askandwait",
                inputs: r#""QUESTION": [1, [10, ""]]"#.into(),
                fields: String::new(),
                deps: Vec::default(),
            },
            Op::Wait { seconds } => {
                let seconds_data = seconds.data(self.uuid);

                let mut deps = Vec::default();
                deps.extend(seconds_data.deps);

                BlockData {
                    op_code: "control_wait",
                    inputs: format!(r#""DURATION": {}"#, seconds_data.val),
                    fields: String::new(),
                    deps,
                }
            },
        };

        Ok(block_data)
    }
}

#[derive(Debug)]
pub struct Root {
    pub block: Block,
    pub x: f64,
    pub y: f64,
}

#[derive(Debug)]
pub struct Body {
    pub blocks: Vec<Block>,
}

pub struct BodyData {
    pub first_uuid: Uuid,
    pub deps: Vec<String>,
}

impl Body {
    pub fn data(&self, root: Option<&Root>) -> anyhow::Result<BodyData> {
        struct Link<'a> {
            block: &'a Block,
            prev: Option<Uuid>,
            next: Option<Uuid>,
        }

        let blocks =
            [root.map(|root| &root.block)].into_iter().flatten().chain(&self.blocks).collect_vec();
        let block_uuids = blocks.iter().map(|block| block.uuid).collect_vec();

        let mut prev = None;
        let mut next_iter = block_uuids.into_iter().skip(1);

        let mut links = Vec::default();

        for block in blocks {
            let cur_uuid = block.uuid;
            let next = next_iter.next();
            links.push(Link { block, prev, next });
            prev = Some(cur_uuid);
        }

        let Some(first_link) = links.first() else { bail!("Expected at least one link in body") };
        let first_uuid = first_link.block.uuid;

        let mut links = links.into_iter();
        let mut blocks = Vec::default();

        if let Some(root) = root {
            let Some(root_link) = links.next() else { bail!("Expected root to have a root link") };

            let root_block_data = root_link.block.data()?;
            let root_block = format!(
                r#"
            "{}": {{
                "opcode": "{}",
                "next": {},
                "parent": {},
                "inputs": {{{}}},
                "fields": {{{}}},
                "shadow": false,
                "topLevel": true,
                "x": {},
                "y": {}
            }}
        "#,
                root_link.block.uuid,
                root_block_data.op_code,
                link_to_json(root_link.next),
                link_to_json(root_link.prev),
                root_block_data.inputs,
                root_block_data.fields,
                root.x,
                root.y
            );
            blocks.push(root_block);
        }

        for link in links {
            let block_data = link.block.data()?;
            let block = BlockJsonMaker {
                uuid: link.block.uuid,
                op_code: block_data.op_code,
                next: link.next,
                prev: link.prev,
                inputs: block_data.inputs,
                fields: block_data.fields,
            };
            blocks.push(block.to_json());
            blocks.extend(block_data.deps);
        }

        Ok(BodyData { first_uuid, deps: blocks })
    }
}

#[derive(Debug)]
pub struct Chain {
    pub root: Root,
    pub body: Body,
}

#[derive(Debug)]
pub struct Compiled {
    pub stdout: Arc<List>,
    pub stack: Arc<List>,
    pub args: Arc<List>,
    pub chains: Vec<Chain>,
    pub message_name_to_message: HashMap<String, Arc<Message>>,
}

struct LinkedChunk {
    header: Arc<ScopeHeader>,
    tags: ScopeTagManager,
    chunk: Chunk,
    message: Arc<Message>,
    next_message: Option<Arc<Message>>,
    first_in_scope: bool,
}

struct ScopeTagManager {
    scope_tag_to_chunk_uuid: Arc<HashMap<Arc<String>, Uuid>>,
}

impl ScopeTagManager {
    pub fn get_chunk_uuid(&self, tag: &ApTag) -> anyhow::Result<Uuid> {
        self.get_chunk_uuid_by_name(&tag.inner).with_context(|| format!("In position {}", tag.pos))
    }

    fn get_chunk_uuid_by_name(&self, tag: &String) -> anyhow::Result<Uuid> {
        let Some(chunk_uuid) = self.scope_tag_to_chunk_uuid.get(tag) else {
            let mut err_msg = format!("No tag named '{}' found within this scope", tag);

            if let Some(maybe_tag) =
                spellcheck(tag, self.scope_tag_to_chunk_uuid.keys().map(|k| k.as_str()))
            {
                err_msg += &format!("; Perhaps you meant '{}'?", maybe_tag.term);
            }

            bail!(err_msg);
        };

        Ok(*chunk_uuid)
    }
}

fn create_message_name(kind: &ScopeKind, chunk_uuid: Uuid, first_in_scope: bool) -> String {
    let scope_name = match kind {
        ScopeKind::Main => "::main",
        ScopeKind::Fn { name } => name.as_str(),
    };

    match first_in_scope {
        true => scope_name.into(),
        false => format!("{}@{}", scope_name, chunk_uuid),
    }
}

pub fn compile(parsed: Parsed) -> anyhow::Result<Compiled> {
    let stdout = Arc::new(List { uuid: Uuid::new_v4(), name: "::stdout".into() });
    let stack = Arc::new(List { uuid: Uuid::new_v4(), name: "::stack".into() });
    let args = Arc::new(List { uuid: Uuid::new_v4(), name: "::args".into() });

    let mut message_name_to_message = HashMap::new();

    let mut linked_chunks = Vec::default();

    // create messages for chunks
    for scope in parsed.scopes {
        let scope_tag_to_chunk_uuid = Arc::new(scope.tag_to_chunk_uuid);
        let mut next_message = None;
        for (i, chunk) in scope.chunks.into_iter().enumerate().rev() {
            let first_in_scope = i == 0;

            let message_name = create_message_name(&scope.header.kind, chunk.uuid, first_in_scope);
            let message = Arc::new(Message { uuid: Uuid::new_v4(), name: Arc::new(message_name) });
            message_name_to_message.insert(message.name.as_ref().clone(), Arc::clone(&message));

            linked_chunks.push(LinkedChunk {
                header: Arc::clone(&scope.header),
                tags: ScopeTagManager {
                    scope_tag_to_chunk_uuid: Arc::clone(&scope_tag_to_chunk_uuid),
                },
                chunk,
                message: Arc::clone(&message),
                next_message: next_message.take(),
                first_in_scope,
            });

            next_message = Some(message);
        }
    }

    let chains = linked_chunks.into_iter().map(|link| {
        let root_op = match link.header.kind {
            ScopeKind::Main if link.first_in_scope => Op::OnFlag,
            _ => Op::OnMessage(Arc::clone(&link.message)),
        };

        let head = Root { block: Block { op: root_op, uuid: Uuid::new_v4() }, x: 0., y: 0. };

        let mut body = Vec::default();

        if let ScopeKind::Main = link.header.kind {
            if link.first_in_scope {
                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListClear { list: Arc::clone(&stdout) },
                });

                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListClear { list: Arc::clone(&stack) },
                });

                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListClear { list: Arc::clone(&args) },
                });

                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListPush {
                        list: Arc::clone(&stdout),
                        val: LitExpr {
                            uuid: Uuid::new_v4(),
                            kind: LitExprKind::Literal(Arc::new(String::new())),
                        },
                    },
                });
            }
        }

        // remove goto instruction from stack
        body.push(Block {
            uuid: Uuid::new_v4(),
            op: Op::ListRemove {
                list: Arc::clone(&stack),
                index: LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::ListLen(Arc::clone(&stack)),
                },
            },
        });

        // var alloc / arg retrieval
        if link.first_in_scope {
            for var in link.header.vars.iter().rev() {
                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListPush {
                        list: Arc::clone(&stack),
                        val: LitExpr {
                            uuid: Uuid::new_v4(),
                            kind: LitExprKind::ListIndex {
                                list: Arc::clone(&args),
                                index: Box::new(LitExpr {
                                    uuid: Uuid::new_v4(),
                                    kind: LitExprKind::Literal(Arc::new(
                                        (var.offset + 1).to_string(),
                                    )),
                                }),
                            },
                        },
                    },
                });
            }

            // clear args after use
            body.push(Block {
                uuid: Uuid::new_v4(),
                op: Op::ListClear { list: Arc::clone(&args) },
            });
        }

        let vars = &link.header.vars;

        let blocks = link.chunk.body.into_iter().map(|command| {
            command_kind_to_ops(
                command.inner,
                &stdout,
                &stack,
                &args,
                vars,
                &link.tags,
                &link.header.kind,
                link.next_message.as_ref(),
                &message_name_to_message,
            )
            .with_context(|| format!("On line {}", command.line_num))
        });

        let blocks = blocks.collect::<anyhow::Result<Vec<_>>>()?;
        body.extend(blocks.into_iter().flatten().map(|op| Block { uuid: Uuid::new_v4(), op }));

        // var dealloc
        if link.next_message.is_none() {
            for _var in &link.header.vars {
                body.push(Block {
                    uuid: Uuid::new_v4(),
                    op: Op::ListRemove {
                        list: Arc::clone(&stack),
                        index: LitExpr {
                            kind: LitExprKind::ListLen(Arc::clone(&stack)),
                            uuid: Uuid::new_v4(),
                        },
                    },
                });
            }
        }

        // goto next chunk
        body.push(Block {
            uuid: Uuid::new_v4(),
            op: Op::BroadcastSync(BroadcastInput::Expr(LitExpr {
                uuid: Uuid::new_v4(),
                kind: LitExprKind::ListIndex {
                    list: Arc::clone(&stack),
                    index: Box::new(LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::ListLen(Arc::clone(&stack)),
                    }),
                },
            })),
        });

        let body = Body { blocks: body };

        Ok(Chain { root: head, body })
    });

    let chains = chains.collect::<anyhow::Result<Vec<_>>>()?;

    Ok(Compiled { stdout, stack, args, chains, message_name_to_message })
}
