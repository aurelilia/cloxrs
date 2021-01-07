use smol_str::SmolStr;

use super::chunk::{Chunk, OpCodeLine};

pub fn disassemble_chunk(chunk: &Chunk, name: &Option<SmolStr>) {
    println!("== {} ==", name.as_ref().map(SmolStr::as_str).unwrap_or("SCRIPT"));
    for (index, instruction) in chunk.code.iter().enumerate() {
        disassemble_instruction(index, instruction);
    }
}

fn disassemble_instruction(index: usize, instruction: &OpCodeLine) {
    println!(
        "{:04}  L{:03}  {:?}",
        index, instruction.line, instruction.code
    );
}
