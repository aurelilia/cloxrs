use super::value::Value;
use crate::value::Closure;
use smol_str::SmolStr;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
#[repr(u16)]
pub enum OpCode {
    Constant(Value),
    DefineGlobal(SmolStr),
    GetGlobal(SmolStr),
    SetGlobal(SmolStr),
    GetUpvalue(usize),
    SetUpvalue(usize),
    GetLocal(usize),
    SetLocal(usize),
    Pop,
    HoistUpvalue,

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

    Closure(Rc<Closure>),
}
