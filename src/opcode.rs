use super::value::Value;
use crate::{value::Closure, UInt};
use smol_str::SmolStr;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum OpCode {
    Constant(Constant),
    DefineGlobal(SmolStr),

    GetGlobal(SmolStr),
    SetGlobal(SmolStr),

    GetUpvalue(UInt),
    SetUpvalue(UInt),

    GetLocal(UInt),
    SetLocal(UInt),

    GetProperty(SmolStr),
    SetProperty(SmolStr),
    GetSuper(SmolStr),

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

    Jump(UInt),
    JumpIfFalse(UInt),
    Loop(UInt),

    Call(UInt),
    // Name, args count
    Invoke(SmolStr, UInt),
    // Name, args count
    InvokeSuper(SmolStr, UInt),

    Return,

    Closure(Rc<Closure>),
    Class(SmolStr),
    EndClass(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Nil,
    Bool(bool),
    Number(f64),
    String(SmolStr),
}

impl Constant {
    pub fn value(self) -> Value {
        match self {
            Constant::Nil => Value::Nil,
            Constant::Bool(b) => Value::Bool(b),
            Constant::Number(n) => Value::Number(n),
            Constant::String(s) => Value::String(s),
        }
    }
}
