use super::compiler::Compiler;
use super::chunk::Chunk;
use super::opcode::OpCode;
use super::value::Value;

pub struct VM {
    pub chunk: Chunk,
    pub ip: usize,
    pub stack: Vec<Value>
}

impl VM {
    pub fn interpret(&mut self, source: &String) -> InterpretResult {
        self.chunk = Chunk::new();
        self.ip = 0;

        let mut compiler = Compiler::new(&mut self.chunk); 
        compiler.compile(source);

        self.run()
    }

    // TODO: Error handling. Currently, most incorrect bytecode panics
    fn run(&mut self) -> InterpretResult {
        loop {
            self.ip += 1;
            match self.get_current_instruction() {
                OpCode::OpReturn => {
                    println!("{:?}", self.stack.pop().unwrap());
                    break InterpretResult::Ok
                },

                // TODO: Don't just copy them!!
                OpCode::OpConstant(constant) => self.stack.push(*constant),

                OpCode::OpNegate => {
                    let value = self.stack.pop().unwrap();
                    self.stack.push(-value);
                }

                // TODO: I HATE pointers... See comment above...
                OpCode::OpAdd | OpCode::OpSubstract | OpCode::OpMultiply | OpCode::OpDivide => {
                    let opcode = *self.get_current_instruction();
                    let result = self.binary_instruction(&opcode);
                    self.stack.push(result);
                },
            }
        }
    }

    fn get_current_instruction(&self) -> &OpCode {
        &self.chunk.code[self.ip - 1].code
    }

    fn binary_instruction(&mut self, opcode: &OpCode) -> Value {
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        match opcode {
            OpCode::OpAdd => a + b,
            OpCode::OpSubstract => a - b,
            OpCode::OpMultiply => a * b,
            OpCode::OpDivide => a / b,
            _ => panic!("Binary instruction was tried to be processed; opcode was not a binary instruction!!")
        }
    }

    pub fn new() -> VM {
        VM { 
            chunk: Chunk::new(), 
            ip: 0,
            stack: Vec::with_capacity(256)
        }
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError
}