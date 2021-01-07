use std::{
    cell::RefCell,
    collections::HashMap,
    fs,
    time::{SystemTime, UNIX_EPOCH},
};

use smol_str::SmolStr;

use super::compiler::Compiler;
use super::disassembler;
use super::opcode::OpCode;
use super::value::Value;
use crate::value::{Function, NativeFun};
use crate::MutRc;
use std::rc::Rc;

type Res = Result<(), Failure>;

pub struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<SmolStr, Value>,
}

impl VM {
    pub fn interpret(&mut self, source: &str) -> Res {
        self.frames.clear();
        let mut compiler = Compiler::new(source);

        let func = compiler.compile().ok_or(Failure::CompileError)?;
        disassembler::disassemble_chunk(&func.borrow().chunk, &func.borrow().name);

        self.define_natives();
        self.stack.push(Value::Function(Rc::clone(&func)));
        self.new_callframe(func);

        self.run()
    }

    fn run(&mut self) -> Res {
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
                    self.globals.insert(global, self.stack.pop().unwrap());
                }

                OpCode::GetGlobal(global) => {
                    let value = self.globals.get(&global);
                    if let Some(value) = value {
                        self.stack.push(value.clone());
                    } else {
                        self.print_error(&format!("Undefined variable {}.", global));
                        break;
                    }
                }

                OpCode::SetGlobal(global) => {
                    if self
                        .globals
                        .insert(global.clone(), self.stack_last().clone())
                        .is_none()
                    {
                        self.print_error(&format!("Undefined variable {}.", global));
                        break;
                    }
                }

                OpCode::GetLocal(local) => {
                    // TODO: FIXME: I have no idea why, but stack slots seem misaligned
                    // sometimes. Most of the time, a value is present at the correct slot;
                    // sometimes though it's shifted down by 1?
                    // This *seems* to work fine though,  did not test it much though...
                    let t = self.stack.get(frame.slot_offset + local + 1).cloned();
                    if let Some(t) = t {
                        self.stack.push(t);
                    } else {
                        self.stack
                            .push(self.stack[frame.slot_offset + local].clone())
                    }
                }

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
                        break;
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
                        break;
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

                OpCode::Call(arg_count) => {
                    if !self.call_value(
                        self.stack[self.stack.len() - arg_count - 1].clone(),
                        arg_count,
                    ) {
                        break;
                    }
                }

                OpCode::Return => {
                    let result = self.stack_pop();

                    let frame = self.frames.pop().unwrap();
                    if self.frames.is_empty() {
                        self.stack_pop();
                        return Ok(());
                    }

                    self.stack.truncate(frame.slot_offset);
                    self.stack.push(result);
                }
            }
        }

        // All terminations of this loop are to be interpreted as an error,
        // return will return directly and prevent hitting this
        Err(Failure::RuntimeError)
    }

    fn get_current_instruction(&self) -> OpCode {
        let frame = self.frames.last().unwrap();
        frame.function.borrow().chunk.code[frame.ip - 1]
            .code
            .clone()
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
            _ => panic!("unknown opcode"),
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

            _ => panic!("unknown opcode"),
        }
    }

    fn new_callframe(&mut self, function: MutRc<Function>) {
        self.frames.push(CallFrame {
            function,
            ip: 0,
            slot_offset: self.stack.len() - 1,
        });
    }

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        let arity = callee.arity();
        if let Some(arity) = arity {
            if arg_count != arity {
                self.print_error(&format!(
                    "Incorrect amount of function arguments (wanted {}, got {})",
                    arity, arg_count
                ));
                return false;
            }
        } else {
            self.print_error("Can only call functions and classes.");
            return false;
        }

        match callee {
            Value::Function(func) => self.call(func, arg_count),
            Value::NativeFun(func) => {
                // TODO: Maybe use smallvec to avoid an allocation every call?
                let args = self.stack.split_off(self.stack.len() - arg_count);
                let result = (func.borrow().func)(&args);
                match result {
                    Ok(value) => {
                        self.stack.push(value);
                        true
                    }

                    Err(msg) => {
                        self.print_error(msg);
                        false
                    }
                }
            }
            _ => panic!("unknown callee"),
        }
    }

    fn call(&mut self, func: MutRc<Function>, arg_count: usize) -> bool {
        self.new_callframe(func);
        self.frames.last_mut().unwrap().slot_offset -= arg_count;
        true
    }

    fn print_error(&mut self, message: &str) {
        let frame = self.frames.last().unwrap();
        println!(
            "[Line {}] Runtime error: {}",
            frame.function.borrow().chunk.code[frame.ip - 1].line,
            message
        );
        println!("Stack trace:");
        for frame in self.frames.iter().rev() {
            let func = frame.function.borrow();
            let line = func.chunk.code[frame.ip - 1].line;
            println!("[Line {}] in {}", line, func)
        }
    }

    fn define_natives(&mut self) {
        self.define_native("clock", 0, |_| {
            let time = SystemTime::now().duration_since(UNIX_EPOCH);
            let time = time.expect("Are we not in 1970 yet?");
            Ok(Value::Number(time.as_secs() as f64))
        });

        self.define_native("readfile", 1, |a| {
            let name = match &a[0] {
                Value::String(name) => name,
                _ => return Err("readfile: First argument must be file name as string"),
            };

            let file = fs::read_to_string(name.as_str());
            Ok(file
                .map(|t| Value::String(SmolStr::new(t)))
                .unwrap_or(Value::Nil))
        });

        self.define_native("writefile", 2, |a| {
            let name = match &a[0] {
                Value::String(name) => name,
                _ => return Err("writefile: First argument must be file name as string"),
            };

            let res = fs::write(name.as_str(), a[1].to_string());
            Ok(Value::Bool(res.is_ok()))
        });
    }

    fn define_native(
        &mut self,
        name: &str,
        arity: usize,
        func: fn(&[Value]) -> Result<Value, &str>,
    ) {
        let name = SmolStr::new_inline(name);
        self.globals.insert(
            name.clone(),
            Value::NativeFun(Rc::new(RefCell::new(NativeFun { name, arity, func }))),
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

pub enum Failure {
    CompileError,
    RuntimeError,
}

pub struct CallFrame {
    function: MutRc<Function>,
    ip: usize,
    slot_offset: usize,
}
