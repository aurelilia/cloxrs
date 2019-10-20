use super::chunk::{Chunk, OpCodeLine};
use std::rc::Rc;

pub fn disassemble_chunk(chunk: &Chunk, name: &Option<Rc<String>>) {
    println!("== {} ==", name.as_ref().unwrap_or(&Rc::new("SCRIPT".to_string())));

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
