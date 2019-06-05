use super::value::Value;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    OpConstant(Value),
    OpAdd,
    OpSubstract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpReturn
}