use std::collections::HashMap;

use super::chunk::Chunk;
use super::compiler::Compiler;
use super::disassembler;
use super::opcode::OpCode;
use super::value::Value;
use std::rc::Rc;

pub struct VM {
    chunk: Chunk,
    ip: usize,

    stack: Vec<Value>,
    globals: HashMap<Rc<String>, Value>,
}

impl VM {
    pub fn interpret(&mut self, source: String) -> InterpretResult {
        self.ip = 0;
        let mut compiler = Compiler::new();

        if let Some(chunk) = compiler.compile(source) {
            disassembler::disassemble_chunk(&chunk, "RESULT");
            self.chunk = chunk;
            self.run()
        } else {
            InterpretResult::CompileError
        }
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            self.ip += 1;

            match self.get_current_instruction() {
                OpCode::Constant(constant) => self.stack.push(constant.clone()),

                OpCode::DefineGlobal(global) => {
                    self.globals.insert(
                        global,
                        self.stack.pop().unwrap(),
                    );
                }

                OpCode::GetGlobal(global) => {
                    let value = self.globals.get(&global);
                    if let Some(value) = value {
                        self.stack.push(value.clone());
                    } else {
                        self.print_error(&format!("Undefined variable {}.", global));
                        break InterpretResult::RuntimeError;
                    }
                }

                OpCode::SetGlobal(global) => {
                    if self.globals.insert(
                        Rc::clone(&global),
                        self.stack_last().clone(),
                    ).is_none() {
                        self.print_error(&format!("Undefined variable {}.", global));
                        break InterpretResult::RuntimeError;
                    }
                }

                OpCode::GetLocal(local) => self.stack.push(self.stack[local].clone()),

                OpCode::SetLocal(local) => {
                    self.stack[local] = self.stack_last().clone();
                }

                OpCode::Pop => {
                    self.stack_pop();
                }

                OpCode::Negate | OpCode::Not => {
                    let result = self.unary_instruction();
                    if let Some(result) = result {
                        self.stack.push(result)
                    } else {
                        self.print_error("Unary operation had an invalid operand!");
                        break InterpretResult::RuntimeError
                    }
                }

                OpCode::Add
                | OpCode::Subtract
                | OpCode::Multiply
                | OpCode::Divide
                | OpCode::Equal
                | OpCode::Greater
                | OpCode::Less => {
                    let result = self.binary_instruction();
                    if let Some(result) = result {
                        self.stack.push(result)
                    } else {
                        self.print_error("Binary operation had invalid operands!");
                        break InterpretResult::RuntimeError
                    }
                }

                OpCode::Print => println!("{}", self.stack_pop()),

                OpCode::Jump(offset) => self.ip += offset,

                OpCode::JumpIfFalse(offset) => {
                    if self.stack_last().is_falsey() {
                        self.ip += offset
                    }
                }

                OpCode::Loop(offset) => self.ip -= offset,

                OpCode::Return => break InterpretResult::Ok,
            }
        }
    }

    fn get_current_instruction(&self) -> OpCode {
        self.chunk.code[self.ip - 1].code.clone()
    }

    fn stack_last(&self) -> &Value {
        self.stack.last().expect("Stack was empty?")
    }

    fn stack_pop(&mut self) -> Value {
        self.stack.pop().expect("Stack was empty?")
    }

    fn unary_instruction(&mut self) -> Option<Value> {
        let opcode = self.get_current_instruction();
        match opcode {
            OpCode::Negate => -self.stack_pop(),
            OpCode::Not => !self.stack_pop(),
            _ => panic!("Unary instruction was match with incorrect opcode!"),
        }
    }

    fn binary_instruction(&mut self) -> Option<Value> {
        let opcode = self.get_current_instruction();
        let b = self.stack_pop();
        let a = self.stack_pop();

        match opcode {
            OpCode::Add => a + b,
            OpCode::Subtract => a - b,
            OpCode::Multiply => a * b,
            OpCode::Divide => a / b,

            OpCode::Equal => Some(Value::Bool(a == b)),
            OpCode::Greater => a.greater(b),
            OpCode::Less => a.less(b),

            _ => panic!("Binary instruction was tried to be processed; opcode was not a binary instruction!!")
        }
    }

    fn print_error(&mut self, message: &str) {
        println!(
            "[Line {}] Runtime error: {}",
            self.chunk.code[self.ip - 1].line,
            message
        );
    }

    pub fn new() -> VM {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::with_capacity(256),
            globals: HashMap::with_capacity(16),
        }
    }
}

pub enum InterpretResult {
    Ok,
    CompileError,
    RuntimeError,
}
