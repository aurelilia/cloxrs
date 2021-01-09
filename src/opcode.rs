use crate::{interner::StrId, value::Closure, UInt};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    ConstNil,
    ConstBool(bool),
    ConstNumber(f64),
    ConstString(StrId),

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
