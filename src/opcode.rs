use smol_str::SmolStr;

use super::value::Value;

#[derive(Debug, PartialEq, Clone)]
#[repr(u16)]
pub enum OpCode {
    Constant(Value),
    DefineGlobal(SmolStr),
    GetGlobal(SmolStr),
    SetGlobal(SmolStr),
    GetLocal(usize),
    SetLocal(usize),
    Pop,

    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    Greater,
    Less,

    Print,

    Jump(usize),
    JumpIfFalse(usize),
    Loop(usize),

    Call(usize),

    Return,
}
