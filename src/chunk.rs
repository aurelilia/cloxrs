use smallvec::SmallVec;

use super::opcode::OpCode;

#[derive(Debug, PartialEq)]
pub struct OpCodeLine {
    pub code: OpCode,
    pub line: usize,
}

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: SmallVec<[OpCodeLine; 16]>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: SmallVec::new(),
        }
    }
}
