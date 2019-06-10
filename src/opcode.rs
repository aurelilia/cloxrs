use super::value::Value;

#[derive(Debug, Clone)]
pub enum OpCode {
    Constant(Value), 
    DefineGlobal(String), GetGlobal(String),
    Pop,

    Add, Substract, Multiply, Divide,
    Negate, Not,
    Equal, Greater, Less,

    Print,

    Return
}