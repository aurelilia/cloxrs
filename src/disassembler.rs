use super::chunk::{OpCodeLine, Chunk};
use super::opcode::OpCode;

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    for (index, instruction) in chunk.code.iter().enumerate() {
        disassemble_instruction(index, instruction);
    }
}

fn disassemble_instruction(index: usize, instruction: &OpCodeLine) {
    print!("{:04}  L{:03}  ", index, instruction.line);
    match instruction.code {
        _ => { simple_instruction(&instruction.code) }
    }
}

fn simple_instruction(instruction: &OpCode) {
    println!("{:?}", instruction);
}