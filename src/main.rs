mod chunk;
mod disassembler;
mod opcode;
mod value;

use chunk::{OpCodeLine, Chunk};
use opcode::OpCode;
use value::Value;

fn main() {
    let mut chunk = Chunk::new();
    chunk.code.push(OpCodeLine { code: OpCode::OpReturn, line: 1 });
    chunk.code.push(OpCodeLine { code: OpCode::OpConstant(Value::Double(5.5)), line: 1 });
    disassembler::disassemble_chunk(&chunk, "test");
}
