use std::sync::Arc;

use strum::{EnumCount, EnumDiscriminants, EnumIter, VariantArray};

use super::asm_token::{AsmVal, Positioned};

pub type Ap<T> = Arc<Positioned<T>>;

pub type ApVal = Ap<AsmVal>;
pub type ApFn = Ap<Arc<String>>;
pub type ApTag = Ap<Arc<String>>;

#[derive(Debug, EnumDiscriminants)]
#[strum_discriminants(derive(VariantArray, EnumCount, EnumIter))]
pub enum Command {
    Clear,
    Out { msg: ApVal },
    OutLine,
    In { dest: ApVal },
    Copy { val: ApVal, dest: ApVal },
    Tag,
    Jump { tag: ApTag },
    JumpIf { cond: ApVal, tag: ApTag },
    Call { function: ApFn, args: Vec<ApVal> },
    CallIf { cond: ApVal, function: ApFn, args: Vec<ApVal> },
    Add { left: ApVal, right: ApVal, dest: ApVal },
    Sub { left: ApVal, right: ApVal, dest: ApVal },
    Mul { left: ApVal, right: ApVal, dest: ApVal },
    Div { left: ApVal, right: ApVal, dest: ApVal },
    Mod { left: ApVal, right: ApVal, dest: ApVal },
    Eq { left: ApVal, right: ApVal, dest: ApVal },
    Gt { left: ApVal, right: ApVal, dest: ApVal },
    Lt { left: ApVal, right: ApVal, dest: ApVal },
    Rand { min: ApVal, max: ApVal, dest: ApVal },
    Time { dest: ApVal },
    Wait { seconds: ApVal },
}

impl CommandDiscriminants {
    pub const fn keyword(&self) -> &'static str {
        match self {
            Self::Clear => "clear",
            Self::Out => "out",
            Self::OutLine => "outl",
            Self::In => "in",
            Self::Copy => "copy",
            Self::Tag => "tag",
            Self::Jump => "jmp",
            Self::JumpIf => "jmpf",
            Self::Call => "call",
            Self::CallIf => "callf",
            Self::Mod => "mod",
            Self::Eq => "eq",
            Self::Gt => "gt",
            Self::Lt => "lt",
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Rand => "rand",
            Self::Time => "time",
            Self::Wait => "wait",
        }
    }

    pub fn keywords() -> Vec<&'static str> {
        Self::VARIANTS.iter().map(|v| v.keyword()).collect()
    }
}

#[derive(Debug)]
pub struct LineNumbered<T> {
    pub line_num: usize,
    pub inner: T,
}
