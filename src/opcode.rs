use super::value::Value;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    Constant(Value),
    DefineGlobal(Rc<String>),
    GetGlobal(Rc<String>),
    SetGlobal(Rc<String>),
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
