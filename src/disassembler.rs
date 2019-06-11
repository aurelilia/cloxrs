use super::chunk::{Chunk, OpCodeLine};
pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

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
