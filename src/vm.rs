use std::collections::HashMap;

use super::chunk::Chunk;
use super::compiler::Compiler;
use super::disassembler;
use super::opcode::OpCode;
use super::value::Value;
use std::rc::Rc;
use crate::value::Function;
use crate::MutRc;

pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<Rc<String>, Value>,
}

impl VM {
    pub fn interpret(&mut self, source: String) -> InterpretResult {
        self.frames.clear();
        let mut compiler = Compiler::new(source);

        if let Some(func) = compiler.compile() {
            disassembler::disassemble_chunk(&func.borrow().chunk, &func.borrow().name);
            self.stack.push(Value::Function(Rc::clone(&func)));
            self.new_callframe(func);
            self.run();
            InterpretResult::Ok
        } else {
            InterpretResult::CompileError
        }
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            {
                let mut frame = self.frames.last_mut().unwrap();
                frame.ip += 1;
            }
            let current_inst = self.get_current_instruction();
            let mut frame = self.frames.last_mut().unwrap();

            match current_inst {
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

                OpCode::GetLocal(local) => self.stack.push(self.stack[frame.slot_offset + local].clone()),

                OpCode::SetLocal(local) => {
                    let offset = frame.slot_offset + local;
                    self.stack[offset] = self.stack_last().clone();
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

                OpCode::Jump(offset) => frame.ip += offset,

                OpCode::JumpIfFalse(offset) => {
                    if self.stack_last().is_falsey() {
                        self.frames.last_mut().unwrap().ip += offset;
                    }
                }

                OpCode::Loop(offset) => frame.ip -= offset,

                OpCode::Return => break InterpretResult::Ok,
            }
        }
    }

    /// Oof
    fn get_current_instruction(&self) -> OpCode {
        let frame = self.frames.last().unwrap();
        frame.function.borrow().chunk.code[frame.ip - 1].code.clone()
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
    
    fn new_callframe(&mut self, function: MutRc<Function>) {
        self.frames.push(CallFrame {
            function,
            ip: 0,
            slot_offset: self.stack.len() - 1
        });
    }

    fn print_error(&mut self, message: &str) {
        let frame = self.frames.last().unwrap();
        println!(
            "[Line {}] Runtime error: {}",
            frame.function.borrow().chunk.code[frame.ip - 1].line,
            message
        );
    }

    pub fn new() -> VM {
        VM {
            frames: vec![],
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

pub struct CallFrame {
    function: MutRc<Function>,
    ip: usize,
    slot_offset: usize
}
