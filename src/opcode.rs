use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant(Value),
    Add,
    Substract,
    Multiply,
    Divide,
    Negate,
    Return
}