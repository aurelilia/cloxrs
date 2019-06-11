use std::collections::HashMap;

use super::compiler::Compiler;
use super::chunk::Chunk;
use super::disassembler;
use super::opcode::OpCode;
use super::value::Value;

pub struct VM {
    chunk: Chunk,
    ip: usize,

    stack: Vec<Value>,
    globals: HashMap<String, Value>,

    had_error: bool
}

impl VM {
    pub fn interpret(&mut self, source: &String) -> InterpretResult {
        self.chunk = Chunk::new();
        self.ip = 0;

        let mut compiler = Compiler::new(&mut self.chunk);

        if compiler.compile(source) {
            disassembler::disassemble_chunk(&self.chunk, "RESULT");
            self.run()
        } else {
            InterpretResult::CompileError
        }
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            self.ip += 1;

            match &self.chunk.code[self.ip - 1].code {
                OpCode::Constant(constant) => self.stack.push(constant.clone()),

                OpCode::DefineGlobal(global) => {
                    self.globals.insert(global.to_string(), self.stack.pop().expect("Stack was empty?"));
                },

                OpCode::GetGlobal(global) => {
                    let value = self.globals.get(global);
                    if let Some(value) = value {
                        self.stack.push(value.clone());
                    } else {
                        self.runtime_error(&format!("Undefined variable {}.", global));
                        break InterpretResult::RuntimeError;
                    }
                }

                OpCode::SetGlobal(global) => {
                    if let None = self.globals.insert(global.to_string(), self.stack.last().expect("Stack was empty?").clone()) {
                        self.runtime_error(&format!("Undefined variable {}.", global));
                        break InterpretResult::RuntimeError;
                    }
                }

                OpCode::GetLocal(local) => self.stack.push(self.stack[*local].clone()),

                OpCode::SetLocal(local) => {
                    self.stack[*local] = self.stack.last().expect("Stack was empty?").clone();
                }

                OpCode::Pop => { self.stack.pop(); },

                OpCode::Negate | OpCode::Not => {
                    let result = self.unary_instruction();
                    self.stack.push(result)
                }

                OpCode::Add | OpCode::Substract | OpCode::Multiply | OpCode::Divide |
                OpCode::Equal | OpCode::Greater | OpCode::Less => {
                    let result = self.binary_instruction();
                    self.stack.push(result);
                },

                OpCode::Print => println!("{}", self.stack.pop().expect("Stack was empty?")),

                OpCode::Jump(offset) => self.ip += offset,

                OpCode::JumpIfFalse(offset) => {
                    if self.stack.last().expect("Stack was empty?").is_falsey() { self.ip += offset }
                }

                OpCode::Loop(offset) => self.ip -= offset,

                OpCode::Return => break InterpretResult::Ok,
            }

            if self.had_error {
                break InterpretResult::RuntimeError
            }
        }
    }

    fn get_current_instruction(&self) -> &OpCode {
        &self.chunk.code[self.ip - 1].code
    }

    fn unary_instruction(&mut self) -> Value {
        let opcode = self.get_current_instruction();
        match opcode {
            OpCode::Negate => -self.stack.pop().unwrap(),
            OpCode::Not => !self.stack.pop().unwrap(),
            _ => panic!("Unary instruction was tried to be processed; opcode was not a unary instruction!!")
        }.unwrap_or_else(|| {
            self.runtime_error("Unary operation had an invalid operand!");
            Value::Nil
        })
    }

    fn binary_instruction(&mut self) -> Value {
        let opcode = &self.chunk.code[self.ip - 1].code; // TODO: get_current_instruction would be an immutabe borrow...
        let b = self.stack.pop().unwrap();
        let a = self.stack.pop().unwrap();

        match opcode {
            OpCode::Add => a + b,
            OpCode::Substract => a - b,
            OpCode::Multiply => a * b,
            OpCode::Divide => a / b,

            OpCode::Equal => a.equal(b),
            OpCode::Greater => a.greater(b),
            OpCode::Less => a.less(b),

            _ => panic!("Binary instruction was tried to be processed; opcode was not a binary instruction!!")
        }.unwrap_or_else(|| {
            self.runtime_error("Binary operation had invalid operands!");
            Value::Nil
        })
    }
    
    fn runtime_error(&mut self, message: &str) {
        self.had_error = true;
        println!("[Line {}] Runtime error: {}", self.chunk.code[self.ip - 1].line, message);
    }

    pub fn new() -> VM {
        VM { 
            chunk: Chunk::new(), 
            ip: 0,
            stack: Vec::with_capacity(256),
            globals: HashMap::with_capacity(16),
            had_error: false
        }
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError
}