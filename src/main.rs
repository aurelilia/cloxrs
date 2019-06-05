mod chunk;
mod disassembler;
mod opcode;
mod value;
mod vm;

use chunk::{OpCodeLine, Chunk};
use opcode::OpCode;
use value::Value;
use vm::VM;

fn main() {
    let mut chunk = Chunk::new();
    chunk.code.push(OpCodeLine { code: OpCode::OpConstant(Value::Double(5.5)), line: 1 });
    chunk.code.push(OpCodeLine { code: OpCode::OpConstant(Value::Double(51.5)), line: 1 });
    chunk.code.push(OpCodeLine { code: OpCode::OpAdd, line: 1 });
    chunk.code.push(OpCodeLine { code: OpCode::OpReturn, line: 1 });

    let mut vm = VM::new();

    disassembler::disassemble_chunk(&chunk, "test");

    vm.interpret(chunk);
}
