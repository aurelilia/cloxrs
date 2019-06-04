use super::chunk::Chunk;
use super::opcode::OpCode;

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize
}

impl VM {
    pub fn interpret(&mut self, chunk: Chunk) -> InterpretResult {
        self.chunk = chunk;
        self.ip = 0;
        self.run()
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            match self.advance_ip() {
                OpCode::OpReturn => {
                    break InterpretResult::Ok
                },
                OpCode::OpConstant(constant) => {
                    println!("{:?}", constant);
                    continue
                }
            }
        }
    }

    fn advance_ip(&mut self) -> &OpCode {
        self.ip += 1;
        &self.chunk.code[self.ip - 1].code
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError
}