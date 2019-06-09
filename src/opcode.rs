use super::value::Value;

#[derive(Debug, Clone)]
pub enum OpCode {
    Constant(Value),

    Add, Substract, Multiply, Divide,
    Negate, Not,
    Equal, Greater, Less,

    Return
}