use super::opcode::OpCode;
use super::value::Value;

const VECTOR_PREALLOC: usize = 8;

pub struct OpCodeLine {
    pub code: OpCode,
    pub line: usize
}

pub struct Chunk {
    pub code: Vec<OpCodeLine>,
    pub constants: Vec<Value>
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk { 
            code: Vec::with_capacity(VECTOR_PREALLOC),
            constants: Vec::with_capacity(VECTOR_PREALLOC) 
        }
    }
}