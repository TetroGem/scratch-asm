use std::collections::HashMap;
use std::sync::Arc;

use uuid::Uuid;

use super::command::{Command, LineNumbered};

#[derive(Debug)]
pub enum ScopeKind {
    Main,
    Fn { name: String },
}

#[derive(Debug)]
pub struct Var {
    pub name: String,
    pub offset: usize,
}

#[derive(Debug)]
pub struct ScopeHeader {
    pub kind: ScopeKind,
    pub vars: Vec<Var>,
}

#[derive(Debug)]
pub struct Chunk {
    pub uuid: Uuid,
    pub body: Vec<LineNumbered<Command>>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { uuid: Uuid::new_v4(), body: Default::default() }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub header: Arc<ScopeHeader>,
    pub chunks: Vec<Chunk>,
    pub tag_to_chunk_uuid: HashMap<Arc<String>, Uuid>,
}
