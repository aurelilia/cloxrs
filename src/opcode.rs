use super::value::Value;
use crate::value::Closure;
use smol_str::SmolStr;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum OpCode {
    Constant(Value),
    DefineGlobal(SmolStr),

    GetGlobal(SmolStr),
    SetGlobal(SmolStr),

    GetUpvalue(usize),
    SetUpvalue(usize),

    GetLocal(usize),
    SetLocal(usize),

    GetProperty(SmolStr),
    SetProperty(SmolStr),

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
    Invoke(SmolStr, usize),

    Return,

    Closure(Rc<Closure>),
    Class(SmolStr),
    EndClass,
}
