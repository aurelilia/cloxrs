use super::value::Value;

#[derive(Debug, Clone)]
pub enum OpCode {
    Constant(Value), 
    DefineGlobal(String), GetGlobal(String), SetGlobal(String),
    GetLocal(usize), SetLocal(usize),
    Pop,

    Add, Substract, Multiply, Divide,
    Negate, Not,
    Equal, Greater, Less,

    Print,

    Return
}