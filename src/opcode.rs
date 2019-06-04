use super::value::Value;

#[derive(Debug)]
pub enum OpCode {
    OpConstant(Value),
    OpReturn
}