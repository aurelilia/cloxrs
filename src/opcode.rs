use super::value::Value;
use crate::{interner::StrId, value::Closure, UInt};
use either::Either;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
#[repr(u8)]
pub enum OpCode {
    Constant(Constant),
    DefineGlobal(StrId),

    GetGlobal(StrId),
    SetGlobal(StrId),

    GetUpvalue(UInt),
    SetUpvalue(UInt),

    GetLocal(UInt),
    SetLocal(UInt),

    GetProperty(StrId),
    SetProperty(StrId),
    GetSuper(StrId),

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
    Invoke(StrId, UInt),
    // Name, args count
    InvokeSuper(StrId, UInt),

    Return,

    Closure(Rc<Closure>),
    Class(StrId),
    EndClass(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Nil,
    Bool(bool),
    Number(f64),
    String(StrId),
}

impl Constant {
    pub fn value(self) -> Value {
        match self {
            Constant::Nil => Value::Nil,
            Constant::Bool(b) => Value::Bool(b),
            Constant::Number(n) => Value::Number(n),
            Constant::String(s) => Value::String(Either::Left(s)),
        }
    }
}
