use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::{Read, Write};
use std::mem::{replace, take};
use std::path::Path;
use std::sync::Arc;
use std::{env, fs};

use itertools::Itertools;
use uuid::Uuid;
use zip::ZipWriter;
use zip_extensions::ZipWriterExtensions;

#[derive(Debug)]
enum Command {
    Out(AsmVal),
    OutL,
    In { dest: Arc<AsmVal> },
    Copy { val: Arc<AsmVal>, dest: Arc<AsmVal> },
    Tag,
    Jmp { tag: Arc<String> },
    Call { function: Arc<String>, args: Vec<Arc<AsmVal>> },
    CallIf { cond: Arc<AsmVal>, function: String, args: Vec<Arc<AsmVal>> },
    Add { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Sub { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Mul { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Div { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Mod { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Eq { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Gt { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
    Lt { left: Arc<AsmVal>, right: Arc<AsmVal>, dest: Arc<AsmVal> },
}

#[derive(Debug)]
struct Var {
    name: String,
    offset: usize,
}

#[derive(Debug)]
struct ScopeHeader {
    kind: ScopeKind,
    vars: Vec<Var>,
}

#[derive(Debug)]
enum ScopeKind {
    Main,
    Fn { name: String },
}

#[derive(Debug)]
struct Scope {
    header: Arc<ScopeHeader>,
    chunks: Vec<Chunk>,
    tag_to_chunk_uuid: HashMap<Arc<String>, Uuid>,
}

#[derive(Debug)]
struct Chunk {
    uuid: Uuid,
    body: Vec<Command>,
}

fn main() {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };
    println!("{}", in_path);
    println!("{}", out_path);

    let in_path = Path::new(&in_path);
    assert!(
        in_path.extension().and_then(|x| x.to_str()) == Some("ascm"),
        "input file must be a .ascm file"
    );

    let mut in_file = File::open(in_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    println!("{}", input);

    let mut scopes: Vec<Scope> = Vec::default();
    let mut scope = None;

    for line in input.lines() {
        let (command, _comment) = line
            .split_once(';')
            .map(|(command, comment)| (command, Some(comment)))
            .unwrap_or((line, None));

        if command.is_empty() {
            continue;
        }

        println!("{}", command);

        let mut parts = command.split_whitespace();

        let op = parts.next().expect("first part should be op").to_lowercase();
        let scope_kind = match op.as_str() {
            "main" => Some(ScopeKind::Main),
            "fn" => {
                let name = parts.next().expect("scope must have name");
                assert!(name.starts_with('@'), "scope names must start with @");
                Some(ScopeKind::Fn { name: name.into() })
            },
            _ => None,
        };

        if let Some(scope_kind) = scope_kind {
            if let Some(last_scope) = scope.take() {
                scopes.push(last_scope);
            }

            let vars = parts
                .enumerate()
                .map(|(i, name)| {
                    assert!(name.starts_with("$"), "all vars must start with $");
                    Var { name: name.into(), offset: i }
                })
                .collect_vec();

            scope = Some(Scope {
                header: Arc::new(ScopeHeader { kind: scope_kind, vars }),
                chunks: Vec::from([Chunk { uuid: Uuid::new_v4(), body: Vec::default() }]),
                tag_to_chunk_uuid: HashMap::default(),
            });
            continue;
        }

        let Some(scope) = &mut scope else { panic!("commands are not in a scope") };

        let mut parts = parts.collect();

        struct NewChunk {
            tag: Option<Arc<String>>,
        }

        let mut new_chunk = None;

        let command = match op.as_str() {
            "out" => {
                let msg = parse_asm_val(&mut parts, &scope.header.vars).expect("OUT needs message");
                Command::Out(msg)
            },
            "outl" => Command::OutL,
            "in" => {
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("IN needs dest");
                Command::In { dest: Arc::new(dest) }
            },
            "copy" => {
                let val = parse_asm_val(&mut parts, &scope.header.vars).expect("COPY needs val");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("COPY needs addr");
                Command::Copy { val: Arc::new(val), dest: Arc::new(dest) }
            },
            "tag" => {
                let name = parse_tag_name(&mut parts).expect("TAG needs name");
                new_chunk = Some(NewChunk { tag: Some(Arc::new(name.to_string())) });
                Command::Tag
            },
            "jmp" => {
                let tag = parse_tag_name(&mut parts).expect("JMP needs tag");
                Command::Jmp { tag: Arc::new(tag.into()) }
            },
            "call" => {
                let function = parse_function_name(&mut parts).expect("CALL needs function");
                let mut args = Vec::default();
                while !parts.is_empty() {
                    let val =
                        parse_asm_val(&mut parts, &scope.header.vars).expect("CALL invalid arg");
                    args.push(Arc::new(val));
                }

                new_chunk = Some(NewChunk { tag: None });

                Command::Call { function: Arc::new(function.into()), args }
            },
            "callf" => {
                let cond =
                    parse_asm_val(&mut parts, &scope.header.vars).expect("CALLF invalid cond");
                let function = parse_function_name(&mut parts).expect("CALLF needs function");
                let mut args = Vec::default();
                while !parts.is_empty() {
                    let val =
                        parse_asm_val(&mut parts, &scope.header.vars).expect("CALLF invalid arg");
                    args.push(Arc::new(val));
                }

                new_chunk = Some(NewChunk { tag: None });

                Command::CallIf { cond: Arc::new(cond), function: function.into(), args }
            },
            "mod" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs dest");
                Command::Mod { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "eq" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs dest");
                Command::Eq { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "gt" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs dest");
                Command::Gt { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "lt" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("MOD needs dest");
                Command::Lt { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "add" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("ADD needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("ADD needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("ADD needs dest");
                Command::Add { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "sub" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("SUB needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("SUB needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("SUB needs dest");
                Command::Sub { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "mul" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("MUL needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("MUL needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("MUL needs dest");
                Command::Mul { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            "div" => {
                let left = parse_asm_val(&mut parts, &scope.header.vars).expect("DIV needs left");
                let right = parse_asm_val(&mut parts, &scope.header.vars).expect("DIV needs right");
                let dest = parse_asm_val(&mut parts, &scope.header.vars).expect("DIV needs dest");
                Command::Div { left: Arc::new(left), right: Arc::new(right), dest: Arc::new(dest) }
            },
            op => panic!("unknown op: {}", op),
        };

        let chunk = scope.chunks.last_mut().expect("scope should have a chunk");
        chunk.body.push(command);

        if let Some(new_chunk) = new_chunk {
            let chunk = Chunk { uuid: Uuid::new_v4(), body: Vec::default() };
            if let Some(tag) = new_chunk.tag {
                scope.tag_to_chunk_uuid.insert(tag, chunk.uuid);
            }
            scope.chunks.push(chunk);
        }
    }

    if let Some(def) = scope {
        scopes.push(def);
    }

    println!("{:?}", scopes);

    let compiled = compile(scopes).expect("compilation should succeed");

    println!("{:?}", compiled);

    let exported = export(compiled).expect("export conversion should succeed");

    println!("{:?}", exported);

    let in_filename = in_path.file_name().and_then(|name| name.to_str());
    let out_name = in_filename.map(|name| name.split('.')).and_then(|mut parts| parts.next());
    let out_name = out_name.expect("input path should have a filename prefix");

    let out_folder = Path::new(&out_path).join(out_name);

    println!("{:?}", out_folder);

    let _ = fs::DirBuilder::new().create(&out_folder);
    let out_path_metadata = fs::metadata(&out_folder).expect("out path should exist");
    assert!(out_path_metadata.is_dir(), "out path should be a dir");

    let mut project_file = File::create(out_folder.join("project.json"))
        .expect("should be able to create project file");

    project_file.write_all(exported.as_bytes()).expect("should be able to write to out file");

    let res_path = Path::new("./res");

    fs_extra::dir::copy(
        res_path,
        &out_folder,
        &fs_extra::dir::CopyOptions::new().content_only(true),
    )
    .expect("res copy should succeed");

    let out_zip_path = Path::new(&out_path).join(format!("{}.sb3", out_name));
    let out_zip_file = File::create(out_zip_path).expect("should be able to create out zip file");
    let zip_writer = ZipWriter::new(out_zip_file);
    zip_writer.create_from_directory(&out_folder).expect("creating zip should succeed");

    let _ = fs::remove_dir_all(&out_folder);
}

#[derive(Debug)]
enum AsmVal {
    Literal(Arc<String>),
    StackOffset(usize),
    StackDeref(Box<AsmVal>),
}

fn parse_asm_val(parts: &mut VecDeque<&str>, vars: &[Var]) -> Result<AsmVal, ()> {
    let Some(first) = parts.front() else { return Err(()) };
    let Some(first_char) = first.chars().next() else { return Err(()) };

    match first_char {
        '"' => parse_literal(parts).map(|s| AsmVal::Literal(Arc::new(s))),
        '$' => parse_stack_offset(parts, vars).map(AsmVal::StackOffset),
        '*' => parse_stack_deref(parts, vars).map(AsmVal::StackDeref),
        _ => Err(()),
    }
}

fn parse_literal(parts: &mut VecDeque<&str>) -> Result<String, ()> {
    let mut open = false;
    let mut escaped_next = false;
    let mut string = String::new();
    let mut parsed_one_part = false;

    loop {
        let Some(part) = parts.pop_front() else { return Err(()) };
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
                        return Err(());
                    }

                    string.push(char)
                },
            }
        }
    }
}

fn parse_stack_offset(parts: &mut VecDeque<&str>, vars: &[Var]) -> Result<usize, ()> {
    let Some(var_name) = parts.pop_front() else { return Err(()) };
    let Some(var) = vars.iter().find(|var| var.name == var_name) else { return Err(()) };
    Ok(var.offset)
}

fn parse_stack_deref(parts: &mut VecDeque<&str>, vars: &[Var]) -> Result<Box<AsmVal>, ()> {
    let Some(first) = parts.pop_front() else { return Err(()) };
    let Some(("*", val)) = first.split_at_checked(1) else { return Err(()) };
    parts.push_front(val);
    let val = parse_asm_val(parts, vars)?;
    Ok(Box::new(val))
}

fn parse_function_name<'a>(parts: &mut VecDeque<&'a str>) -> Result<&'a str, ()> {
    let Some(name) = parts.pop_front() else { return Err(()) };
    if !name.starts_with('@') {
        return Err(());
    }
    Ok(name)
}

fn parse_tag_name<'a>(parts: &mut VecDeque<&'a str>) -> Result<&'a str, ()> {
    let Some(name) = parts.pop_front() else { return Err(()) };
    if !name.starts_with('#') {
        return Err(());
    }
    Ok(name)
}

#[derive(Debug)]
struct List {
    uuid: Uuid,
    name: String,
}

#[derive(Debug)]
enum LitExprKind {
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
}

impl LitExprKind {
    fn from_asm(value: &AsmVal, stack: &Arc<List>) -> Self {
        match value {
            AsmVal::Literal(l) => LitExprKind::Literal(Arc::clone(l)),
            AsmVal::StackOffset(offset) => LitExprKind::Sub(
                Box::new(LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::ListLen(Arc::clone(stack)),
                }),
                Box::new(LitExpr {
                    uuid: Uuid::new_v4(),
                    kind: LitExprKind::Literal(Arc::new(offset.to_string())),
                }),
            ),
            AsmVal::StackDeref(addr) => {
                let addr = LitExprKind::from_asm(addr, stack);
                LitExprKind::ListIndex {
                    list: Arc::clone(stack),
                    index: Box::new(LitExpr { uuid: Uuid::new_v4(), kind: addr }),
                }
            },
        }
    }
}

#[derive(Debug)]
struct LitExpr {
    uuid: Uuid,
    kind: LitExprKind,
}

#[derive(Debug)]
enum BoolExprKind {
    Not(Box<BoolExpr>),
    Eq(Box<LitExpr>, Box<LitExpr>),
    Gt(Box<LitExpr>, Box<LitExpr>),
    Lt(Box<LitExpr>, Box<LitExpr>),
}

#[derive(Debug)]
struct BoolExpr {
    uuid: Uuid,
    kind: BoolExprKind,
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

struct ExprData {
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
        }
    }
}

#[derive(Debug)]
struct Message {
    uuid: Uuid,
    name: Arc<String>,
}

#[derive(Debug)]
enum BroadcastInput {
    Message(Arc<Message>),
    Expr(LitExpr),
}

#[derive(Debug)]
enum Op {
    OnFlag,
    OnMessage(Arc<Message>),
    ListPush { list: Arc<List>, val: LitExpr },
    ListClear { list: Arc<List> },
    ListRemove { list: Arc<List>, index: LitExpr },
    ListSet { list: Arc<List>, index: LitExpr, val: LitExpr },
    BroadcastSync(BroadcastInput),
    If { cond: BoolExpr, then: Body },
    IfElse { cond: BoolExpr, then: Body, otherwise: Body },
    Ask,
}

#[derive(Debug)]
struct Block {
    uuid: Uuid,
    op: Op,
}

struct BlockData {
    op_code: &'static str,
    inputs: String,
    fields: String,
    deps: Vec<String>,
}

impl Block {
    pub fn data(&self) -> Result<BlockData, ()> {
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
        };

        Ok(block_data)
    }
}

#[derive(Debug)]
struct Root {
    block: Block,
    x: f64,
    y: f64,
}

#[derive(Debug)]
struct Body {
    blocks: Vec<Block>,
}

struct BodyData {
    first_uuid: Uuid,
    deps: Vec<String>,
}

impl Body {
    pub fn data(&self, root: Option<&Root>) -> Result<BodyData, ()> {
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

        let Some(first_link) = links.first() else { return Err(()) };
        let first_uuid = first_link.block.uuid;

        let mut links = links.into_iter();
        let mut blocks = Vec::default();

        if let Some(root) = root {
            let Some(root_link) = links.next() else { return Err(()) };

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
struct Chain {
    root: Root,
    body: Body,
}

#[derive(Debug)]
struct Compiled {
    stdout: Arc<List>,
    stack: Arc<List>,
    args: Arc<List>,
    chains: Vec<Chain>,
    message_name_to_message: HashMap<String, Arc<Message>>,
}

fn compile(scopes: Vec<Scope>) -> Result<Compiled, ()> {
    let stdout = Arc::new(List { uuid: Uuid::new_v4(), name: "::stdout".into() });
    let stack = Arc::new(List { uuid: Uuid::new_v4(), name: "::stack".into() });
    let args = Arc::new(List { uuid: Uuid::new_v4(), name: "::args".into() });

    let mut message_name_to_message = HashMap::new();

    struct LinkedChunk {
        header: Arc<ScopeHeader>,
        scope_tag_to_chunk_uuid: Arc<HashMap<Arc<String>, Uuid>>,
        chunk: Chunk,
        message: Arc<Message>,
        next_message: Option<Arc<Message>>,
        first_in_scope: bool,
    }

    let mut linked_chunks = Vec::default();

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

    // create messages for chunks
    for scope in scopes {
        let scope_tag_to_chunk_uuid = Arc::new(scope.tag_to_chunk_uuid);
        let mut next_message = None;
        for (i, chunk) in scope.chunks.into_iter().enumerate().rev() {
            let first_in_scope = i == 0;

            let message_name = create_message_name(&scope.header.kind, chunk.uuid, first_in_scope);
            let message = Arc::new(Message { uuid: Uuid::new_v4(), name: Arc::new(message_name) });
            message_name_to_message.insert(message.name.as_ref().clone(), Arc::clone(&message));

            linked_chunks.push(LinkedChunk {
                header: Arc::clone(&scope.header),
                scope_tag_to_chunk_uuid: Arc::clone(&scope_tag_to_chunk_uuid),
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

        let blocks = link.chunk.body.into_iter().map(|command| match command {
            Command::Out(msg) => Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stdout),
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
                            kind: LitExprKind::from_asm(&msg, &stack),
                        }),
                    ),
                },
            }])),
            Command::OutL => Ok(Vec::from([Op::ListPush {
                list: Arc::clone(&stdout),
                val: LitExpr {
                    kind: LitExprKind::Literal(Arc::new(String::new())),
                    uuid: Uuid::new_v4(),
                },
            }])),
            Command::In { dest } => Ok(Vec::from([Op::Ask, Op::ListSet {
                list: Arc::clone(&stack),
                index: LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) },
                val: LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::Answer },
            }])),
            Command::Copy { val, dest: addr } => Ok(Vec::from([Op::ListSet {
                list: Arc::clone(&stack),
                index: LitExpr { kind: LitExprKind::from_asm(&addr, &stack), uuid: Uuid::new_v4() },
                val: LitExpr { kind: LitExprKind::from_asm(&val, &stack), uuid: Uuid::new_v4() },
            }])),
            Command::Tag => Ok(Vec::default()),
            Command::Jmp { tag } => {
                let Some(chunk_uuid) = link.scope_tag_to_chunk_uuid.get(tag.as_ref()) else {
                    println!("ERR: {:?}", tag);
                    return Err(());
                };

                let message_name = create_message_name(&link.header.kind, *chunk_uuid, false);
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
            Command::Call { function, args: fn_args } => {
                let Some(message) = message_name_to_message.get(function.as_ref()) else {
                    println!("ERR: {:?}", function);
                    return Err(());
                };

                let arg_ops = fn_args.into_iter().map(|arg| Op::ListPush {
                    list: Arc::clone(&args),
                    val: LitExpr {
                        kind: LitExprKind::from_asm(&arg, &stack),
                        uuid: Uuid::new_v4(),
                    },
                });

                let mut link_ops = Vec::default();
                if let Some(next_message) = &link.next_message {
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

                let call_ops = arg_ops.chain(link_ops).chain(broadcast_ops).collect();

                Ok(call_ops)
            },
            Command::CallIf { cond, function, args: fn_args } => {
                let Some(message) = message_name_to_message.get(&function) else {
                    println!("ERR: {:?}", function);
                    return Err(());
                };

                let cond =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&cond, &stack) };

                let arg_ops = fn_args.into_iter().map(|arg| Op::ListPush {
                    list: Arc::clone(&args),
                    val: LitExpr {
                        kind: LitExprKind::from_asm(&arg, &stack),
                        uuid: Uuid::new_v4(),
                    },
                });

                let mut then_link_ops = Vec::default();
                let mut else_link_ops = Vec::default();
                if let Some(next_message) = &link.next_message {
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

                let then_ops = arg_ops.chain(then_link_ops).chain(broadcast_ops);
                let then_blocks = then_ops.map(|op| Block { uuid: Uuid::new_v4(), op }).collect();
                let then_body = Body { blocks: then_blocks };

                let else_blocks = else_link_ops
                    .into_iter()
                    .map(|op| Block { uuid: Uuid::new_v4(), op })
                    .collect();
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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };
                let dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };

                // need unique blocks for each branch, they can't share uuids or they'd be the
                // same block in two places
                let then_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };
                let else_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };

                // need unique blocks for each branch, they can't share uuids or they'd be the
                // same block in two places
                let then_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };
                let else_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };

                // need unique blocks for each branch, they can't share uuids or they'd be the
                // same block in two places
                let then_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };
                let else_dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };
                let dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };
                let dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };
                let dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

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
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&left, &stack) };
                let right =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&right, &stack) };
                let dest =
                    LitExpr { uuid: Uuid::new_v4(), kind: LitExprKind::from_asm(&dest, &stack) };

                Ok(Vec::from([Op::ListSet {
                    list: Arc::clone(&stack),
                    index: dest,
                    val: LitExpr {
                        uuid: Uuid::new_v4(),
                        kind: LitExprKind::Div(Box::new(left), Box::new(right)),
                    },
                }]))
            },
        });

        let blocks = blocks.collect::<Result<Vec<_>, _>>()?;
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

    let chains = chains.collect::<Result<Vec<_>, _>>()?;

    Ok(Compiled { stdout, stack, args, chains, message_name_to_message })
}

fn export(compiled: Compiled) -> Result<String, ()> {
    let lists =
        [Arc::clone(&compiled.stdout), Arc::clone(&compiled.stack), Arc::clone(&compiled.args)];

    let lists = lists.into_iter().map(|list| format!(r#""{}": ["{}", []]"#, list.uuid, list.name));
    let lists = lists.collect_vec().join(",");

    let chains = compiled.chains.into_iter().map(|chain| chain.body.data(Some(&chain.root)));
    let chains = chains.collect::<Result<Vec<_>, _>>();
    let Ok(chains) = chains else { return Err(()) };
    let blocks = chains.into_iter().flat_map(|data| data.deps).collect_vec().join(",");

    let broadcasts = compiled
        .message_name_to_message
        .into_values()
        .map(|message| format!(r#""{}": "{}""#, message.uuid, message.name))
        .collect_vec()
        .join(",");

    let global_stage = format!(
        r#"
        {{
            "isStage": true,
            "name": "Stage",
            "variables": {{}},
            "lists": {{{}}},
            "broadcasts": {{{}}},
            "blocks": {{{}}},
            "comments": {{}},
            "currentCostume": 0,
            "costumes": [
                {{
                    "name": "backdrop1",
                    "dataFormat": "svg",
                    "assetId": "cd21514d0531fdffb22204e0ec5ed84a",
                    "md5ext": "cd21514d0531fdffb22204e0ec5ed84a.svg",
                    "rotationCenterX": 240,
                    "rotationCenterY": 180
                }}
            ],
            "sounds": [],
            "volume": 100,
            "layerOrder": 0,
            "tempo": 60,
            "videoTransparency": 50,
            "videoState": "on",
            "textToSpeechLanguage": null
        }}
    "#,
        lists, broadcasts, blocks
    );

    let stdout_monitor = format!(
        r#"
        {{
            "id": "{}",
            "mode": "list",
            "opcode": "data_listcontents",
            "params": {{ "LIST": "{}" }},
            "spriteName": null,
            "value": ["Hello world!", "1", "1"],
            "width": 404,
            "height": 299,
            "x": 5,
            "y": 5,
            "visible": true
        }}
    "#,
        compiled.stdout.uuid, compiled.stdout.name,
    );

    Ok(format!(
        r#"
        {{
            "targets": [{}],
            "monitors": [{}],
            "extensions": [],
            "meta": {{
                "semver": "3.0.0",
                "vm": "2.3.4",
                "agent": "ScratchASM/0.1.0"
            }}
        }}
    "#,
        global_stage, stdout_monitor
    ))
}

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
