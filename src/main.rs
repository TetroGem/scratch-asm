use std::collections::VecDeque;
use std::fs::File;
use std::io::{Read, Write};
use std::mem::{replace, take};
use std::path::Path;
use std::sync::Arc;
use std::{env, fs, os};

use itertools::Itertools;
use uuid::Uuid;
use zip::ZipWriter;
use zip_extensions::ZipWriterExtensions;

#[derive(Debug)]
enum Command {
    Out(AsmVal),
    Store { val: AsmVal, addr: AsmVal },
}

#[derive(Debug)]
struct Var {
    name: String,
    offset: usize,
}

#[derive(Debug)]
enum DefKind {
    Main,
}

#[derive(Debug)]
struct Def {
    kind: DefKind,
    body: Vec<Command>,
    vars: Vec<Var>,
}

fn main() {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };
    println!("{}", in_path);
    println!("{}", out_path);

    let in_path = Path::new(&in_path);
    let mut in_file = File::open(in_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    println!("{}", input);

    let mut defs: Vec<Def> = Vec::default();
    let mut def = None;

    for line in input.lines() {
        let mut parts = line.split_whitespace();

        let op = parts.next().unwrap().to_lowercase();
        let op_def_kind = match op.as_str() {
            "main" => Some(DefKind::Main),
            _ => None,
        };

        if let Some(op_def_kind) = op_def_kind {
            if let Some(last_def) = def.take() {
                defs.push(last_def);
            }

            let vars = parts
                .enumerate()
                .map(|(i, name)| {
                    assert!(name.starts_with("$"), "all vars must start with $");
                    Var { name: name.into(), offset: i }
                })
                .collect_vec();

            def = Some(Def { kind: op_def_kind, body: Vec::default(), vars });
            continue;
        }

        let Some(def) = &mut def else { panic!("no def before commands") };

        let mut parts = parts.collect();
        let command = match op.as_str() {
            "out" => {
                let msg = parse_asm_val(&mut parts, &def.vars).expect("OUT needs message");
                Command::Out(msg)
            },
            "store" => {
                let val = parse_asm_val(&mut parts, &def.vars).expect("STORE needs val");
                let addr = parse_asm_val(&mut parts, &def.vars).expect("STORE needs addr");
                Command::Store { val, addr }
            },
            op => panic!("unknown op: {}", op),
        };

        def.body.push(command);
    }

    if let Some(def) = def {
        defs.push(def);
    }

    println!("{:?}", defs);

    let compiled = compile(defs);

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

    // let _ = fs::remove_dir_all(&out_folder);
}

#[derive(Debug)]
enum AsmVal {
    Literal(String),
    StackOffset(usize),
}

fn parse_asm_val(parts: &mut VecDeque<&str>, vars: &[Var]) -> Result<AsmVal, ()> {
    let Some(first) = parts.front() else { return Err(()) };
    let Some(first_char) = first.chars().next() else { return Err(()) };

    match first_char {
        '"' => parse_literal(parts).map(AsmVal::Literal),
        '$' => parse_stack_offset(parts, vars).map(AsmVal::StackOffset),
        _ => return Err(()),
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

#[derive(Debug)]
struct List {
    uuid: Uuid,
    name: String,
}

#[derive(Debug)]
enum ExprKind {
    Literal(String),
    ListLen(Arc<List>),
    Sub(Box<Expr>, Box<Expr>),
}

impl ExprKind {
    fn from_asm(value: AsmVal, stack: &Arc<List>) -> Self {
        match value {
            AsmVal::Literal(l) => ExprKind::Literal(l),
            AsmVal::StackOffset(offset) => ExprKind::Sub(
                Box::new(Expr { uuid: Uuid::new_v4(), kind: ExprKind::ListLen(Arc::clone(stack)) }),
                Box::new(Expr {
                    uuid: Uuid::new_v4(),
                    kind: ExprKind::Literal(offset.to_string()),
                }),
            ),
        }
    }
}

#[derive(Debug)]
struct Expr {
    uuid: Uuid,
    kind: ExprKind,
}

struct ExprData {
    val: String,
    deps: Vec<String>,
}

impl Expr {
    pub fn data(&self, parent: Uuid) -> ExprData {
        match &self.kind {
            ExprKind::Literal(s) => {
                ExprData { val: format!(r#"[1, [10, "{}"]]"#, s), deps: Vec::default() }
            },
            ExprKind::ListLen(list) => {
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
            ExprKind::Sub(a, b) => {
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
        }
    }
}

#[derive(Debug)]
enum Op {
    FlagClicked,
    ListPush { list: Arc<List>, val: Expr },
    ListClear { list: Arc<List> },
    ListRemove { list: Arc<List>, index: Expr },
    ListSet { list: Arc<List>, index: Expr, val: Expr },
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
    pub fn data(&self) -> BlockData {
        match &self.op {
            Op::FlagClicked => BlockData {
                op_code: "event_whenflagclicked",
                inputs: String::new(),
                fields: String::new(),
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
        }
    }
}

#[derive(Debug)]
struct Root {
    block: Block,
    x: f64,
    y: f64,
}

#[derive(Debug)]
struct Chain {
    head: Root,
    body: Vec<Block>,
}

#[derive(Debug)]
struct Compiled {
    stdout: Arc<List>,
    stack: Arc<List>,
    chains: Vec<Chain>,
}

fn compile(defs: Vec<Def>) -> Compiled {
    let stdout = Arc::new(List { uuid: Uuid::new_v4(), name: "::stdout".into() });
    let stack = Arc::new(List { uuid: Uuid::new_v4(), name: "::stack".into() });

    let chains = defs.into_iter().map(|def| {
        let root_op = match def.kind {
            DefKind::Main => Op::FlagClicked,
        };

        let head = Root { block: Block { op: root_op, uuid: Uuid::new_v4() }, x: 0., y: 0. };

        let mut body = Vec::default();

        if let DefKind::Main = def.kind {
            body.push(Block {
                uuid: Uuid::new_v4(),
                op: Op::ListClear { list: Arc::clone(&stdout) },
            });

            body.push(Block {
                uuid: Uuid::new_v4(),
                op: Op::ListClear { list: Arc::clone(&stack) },
            });
        }

        // var alloc
        for _var in &def.vars {
            body.push(Block {
                uuid: Uuid::new_v4(),
                op: Op::ListPush {
                    list: Arc::clone(&stack),
                    val: Expr { kind: ExprKind::Literal(String::new()), uuid: Uuid::new_v4() },
                },
            });
        }

        let blocks = def.body.into_iter().map(|command| match command {
            Command::Out(msg) => Op::ListPush {
                list: Arc::clone(&stdout),
                val: Expr { kind: ExprKind::from_asm(msg, &stack), uuid: Uuid::new_v4() },
            },
            Command::Store { val, addr } => Op::ListSet {
                list: Arc::clone(&stack),
                index: Expr { kind: ExprKind::from_asm(addr, &stack), uuid: Uuid::new_v4() },
                val: Expr { kind: ExprKind::from_asm(val, &stack), uuid: Uuid::new_v4() },
            },
        });

        let blocks = blocks.map(|op| Block { uuid: Uuid::new_v4(), op });
        for block in blocks {
            body.push(block);
        }

        // var dealloc
        for _var in &def.vars {
            body.push(Block {
                uuid: Uuid::new_v4(),
                op: Op::ListRemove {
                    list: Arc::clone(&stack),
                    index: Expr {
                        kind: ExprKind::ListLen(Arc::clone(&stack)),
                        uuid: Uuid::new_v4(),
                    },
                },
            });
        }

        Chain { head, body }
    });

    let chains = chains.collect();

    Compiled { stdout, stack, chains }
}

fn export(compiled: Compiled) -> Result<String, ()> {
    let lists = [Arc::clone(&compiled.stdout), Arc::clone(&compiled.stack)];

    let lists = lists.into_iter().map(|list| format!(r#""{}": ["{}", []]"#, list.uuid, list.name));
    let lists = lists.collect_vec().join(",");

    let chains = compiled.chains.into_iter().map(|chain| {
        struct Link {
            block: Block,
            prev: Option<Uuid>,
            next: Option<Uuid>,
        }

        let blocks = [chain.head.block].into_iter().chain(chain.body).collect_vec();
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

        let mut links = links.into_iter();
        let mut blocks = Vec::default();

        let Some(root_link) = links.next() else { return Err(()) };

        let root_block_data = root_link.block.data();
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
            chain.head.x,
            chain.head.y
        );
        blocks.push(root_block);

        for link in links {
            let block_data = link.block.data();
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

        Ok(blocks)
    });
    let chains = chains.collect::<Result<Vec<_>, _>>();
    let Ok(chains) = chains else { return Err(()) };
    let blocks = chains.into_iter().flatten().collect_vec().join(",");

    let global_stage = format!(
        r#"
        {{
            "isStage": true,
            "name": "Stage",
            "variables": {{}},
            "lists": {{{}}},
            "broadcasts": {{}},
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
        lists, blocks
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
                "agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
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
