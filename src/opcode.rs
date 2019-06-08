use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant(Value),
    True, False, Nil,

    Add, Substract, Multiply, Divide,
    Negate, Not,
    Equal, Greater, Less,

    Return
}