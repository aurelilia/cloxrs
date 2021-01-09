use std::{cell::{Cell, RefCell}, fs, io, rc::Rc, time::{SystemTime, UNIX_EPOCH}};

use either::Either;
use smallvec::SmallVec;

// use super::compiler::Compiler;
use super::disassembler;
use super::opcode::OpCode;
use super::value::Value;
use crate::{
    compiler::Compiler,
    hashmap, hashset,
    interner::{self, Map, StrId},
    value::{BoundMethod, Class, ClosureObj, Instance, NativeFun, Upval, ANY_ARITY},
    vec::SVec,
    HashMap, HashSet, MutRc, UInt,
};

type Res = Result<(), Failure>;
type R<T> = Result<T, String>;

pub struct VM {
    stack: SVec<[Value; 256]>,
    globals: Map<Value>,

    open_upvalues: SVec<[Upval; 16]>,
    closed_upvalues: HashMap<u32, Value>,
    upvalue_count: u32,
    gc_thresh: usize,
}

impl VM {
    pub fn interpret(&mut self, source: &str) -> Res {
        let mut compiler = Compiler::new(source);

        let cls = compiler.compile().ok_or(Failure::CompileError)?;
        disassembler::disassemble_chunk(&cls.function.borrow().chunk, &cls.function.borrow().name);

        self.define_natives();
        let cls = cls.to_obj();
        self.stack.push(Value::Closure(cls.clone()));

        self.run(cls, 0)
    }

    fn run(&mut self, closure: Rc<ClosureObj>, slot_offset: UInt) -> Res {
        let mut frame = CallFrame {
            closure,
            ip: 0,
            slot_offset,
        };

        let current_func = Rc::clone(&frame.closure.function);
        let current_func = current_func.borrow();

        loop {
            let current_inst = &current_func.chunk.code[(frame.ip) as usize].code;
            frame.ip += 1;

            match current_inst {
                OpCode::ConstNil => self.stack.push(Value::Nil),
                OpCode::ConstBool(val) => self.stack.push(Value::Bool(*val)),
                OpCode::ConstNumber(val) => self.stack.push(Value::Number(*val)),
                OpCode::ConstString(str) => self.stack.push(Value::ConstString(*str)),

                OpCode::DefineGlobal(global) => {
                    self.globals.insert(*global, self.stack.pop());
                }

                OpCode::GetGlobal(global) => {
                    let value = self.globals.get(*global);
                    if let Some(value) = value {
                        self.stack.push(value.clone());
                    } else {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(
                            line,
                            &format!("Undefined variable {}.", interner::str(*global)),
                        );
                        break;
                    }
                }

                OpCode::SetGlobal(global) => {
                    if self
                        .globals
                        .insert(*global, self.stack_last().clone())
                        .is_none()
                    {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(
                            line,
                            &format!("Undefined variable {}.", interner::str(*global)),
                        );
                        break;
                    }
                }

                OpCode::GetLocal(local) => {
                    let loc = self.stack[frame.slot_offset + local].clone();
                    self.stack.push(loc);
                }

                OpCode::SetLocal(local) => {
                    let offset = frame.slot_offset + local;
                    self.stack[offset] = self.stack_last().clone();
                }

                OpCode::GetUpvalue(index) => {
                    let upval = &frame.closure.upvalues[*index as usize];
                    let val = match upval.get() {
                        Either::Left(idx) => self.stack[idx].clone(),
                        Either::Right(idx) => self.closed_upvalues[&idx].clone(),
                    };
                    self.stack.push(val)
                }

                OpCode::SetUpvalue(index) => {
                    let val = self.stack.last().clone();
                    let upval = &frame.closure.upvalues[*index as usize];
                    match upval.get() {
                        Either::Left(idx) => self.stack[idx] = val,
                        Either::Right(idx) => {
                            self.closed_upvalues.insert(idx, val);
                        }
                    };
                }

                OpCode::GetProperty(name) => {
                    let inst = if let Value::Instance(inst) = self.stack_pop() {
                        inst
                    } else {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(line, "Only instances have properties.");
                        break;
                    };

                    let field = inst.borrow().fields.get(*name).cloned();
                    if let Some(value) = field {
                        self.stack.push(value);
                    } else {
                        self.bind_method(&inst.borrow().class, &inst, *name);
                    }
                }

                OpCode::SetProperty(name) => {
                    let value = self.stack_pop();
                    let inst = if let Value::Instance(inst) = self.stack_pop() {
                        inst
                    } else {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(line, "Only instances have properties.");
                        break;
                    };

                    inst.borrow_mut().fields.insert(*name, value.clone());
                    self.stack.push(value);
                }

                OpCode::GetSuper(name) => {
                    let superclass = self.stack_pop().into_class();
                    let inst = self.stack_pop().into_instance();
                    self.bind_method(&superclass, &inst, *name);
                }

                OpCode::Pop => {
                    self.stack_pop();
                }

                OpCode::HoistUpvalue => self.pop_or_hoist(),

                OpCode::Negate | OpCode::Not => {
                    let result = self.unary_instruction(current_inst);
                    if let Some(result) = result {
                        self.stack.push(result)
                    } else {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(line, "Unary operation had an invalid operand!");
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
                    let result = self.binary_instruction(current_inst);
                    if let Some(result) = result {
                        self.stack.push(result)
                    } else {
                        let line = current_func.chunk.code[(frame.ip) as usize].line;
                        self.print_error(line, "Binary operation had invalid operands!");
                        break;
                    }
                }

                OpCode::Print => println!("{}", self.stack_pop()),

                OpCode::Jump(offset) => frame.ip += offset,

                OpCode::JumpIfFalse(offset) => {
                    if self.stack_last().is_falsey() {
                        frame.ip += offset;
                    }
                }

                OpCode::Loop(offset) => frame.ip -= offset,

                OpCode::Call(arg_count) => {
                    // todo no cloning!!
                    let result = self.call_value(
                        self.stack[self.stack.len() - arg_count - 1].clone(),
                        *arg_count,
                    );

                    match result {
                        Ok(Some(new_frame)) => {
                            self.run(new_frame.closure, new_frame.slot_offset)?;
                        }

                        Ok(None) => (),

                        Err(msg) => {
                            let line = current_func.chunk.code[(frame.ip) as usize].line;
                            self.print_error(line, &msg)
                        }
                    }
                }

                OpCode::Invoke(method, arg_count) => {
                    let result = self.invoke(*method, *arg_count);

                    match result {
                        Ok(Some(new_frame)) => {
                            self.run(new_frame.closure, new_frame.slot_offset)?;
                        }

                        Ok(None) => (),

                        Err(msg) => {
                            let line = current_func.chunk.code[(frame.ip) as usize].line;
                            self.print_error(line, &msg)
                        }
                    }
                }

                OpCode::InvokeSuper(method, arg_count) => {
                    let superclass = self.stack_pop().into_class();
                    let result = self.invoke_from_class(&superclass, *method, *arg_count);

                    match result {
                        Ok(Some(new_frame)) => {
                            self.run(new_frame.closure, new_frame.slot_offset)?;
                        }

                        Ok(None) => (),

                        Err(msg) => {
                            let line = current_func.chunk.code[(frame.ip) as usize].line;
                            self.print_error(line, &msg)
                        }
                    }
                }

                OpCode::Return => {
                    let result = self.stack_pop();
                    while self.stack.len() != frame.slot_offset {
                        self.pop_or_hoist();
                    }

                    if frame.slot_offset != 0 {
                        self.stack.push(result);
                    } else {
                        self.collect_garbage();
                    }

                    return Ok(());
                }

                OpCode::Closure(cls) => {
                    let ups = cls.upvalues.iter().map(|up| {
                        if up.is_local {
                            self.capture_upvalue(frame.slot_offset + up.index + 1)
                        } else {
                            Rc::clone(&frame.closure.upvalues[up.index as usize])
                        }
                    });
                    let cls = Rc::new(ClosureObj {
                        function: Rc::clone(&cls.function),
                        upvalues: ups.collect(),
                    });
                    self.stack.push(Value::Closure(cls));
                    self.maybe_gc();
                }

                OpCode::Class(name) => {
                    self.stack.push(Value::Class(Rc::new(RefCell::new(Class {
                        name: *name,
                        methods: Map::new(),
                    }))))
                }

                OpCode::EndClass(inherit) => {
                    let mut methods = Map::new();

                    let mut last = self.stack_pop();
                    while let Value::Closure(cls) = last {
                        let name = cls.function.borrow().name.clone().unwrap();
                        methods.insert(name, Value::Closure(cls));
                        last = self.stack_pop();
                    }

                    if *inherit {
                        if let Value::Class(cls) = self.stack_last() {
                            let super_methods = &cls.borrow().methods;
                            methods.add_missing(&super_methods);
                        } else {
                            let line = current_func.chunk.code[(frame.ip) as usize].line;
                            self.print_error(line, "Superclass must be a class.");
                            break;
                        }
                    }

                    last.as_class().borrow_mut().methods = methods;
                }
            }
        }

        // All terminations of this loop are to be interpreted as an error,
        // return will return directly and prevent hitting this
        Err(Failure::RuntimeError)
    }

    fn stack_last(&self) -> &Value {
        self.stack.last()
    }

    fn stack_pop(&mut self) -> Value {
        self.stack.pop()
    }

    fn pop_or_hoist(&mut self) {
        let val = self.stack_pop();
        let upval = self.find_upvalue(self.stack.len());
        if let Some((idx, upval)) = upval {
            upval.set(Either::Right(self.closed_upvalues.len() as u32));

            self.closed_upvalues.insert(self.upvalue_count, val);
            self.upvalue_count += 1;
            self.open_upvalues.remove(idx);
        }
    }

    fn capture_upvalue(&mut self, idx: u32) -> Upval {
        let existing = self.find_upvalue(idx);
        if let Some(exisiting) = existing {
            Rc::clone(exisiting.1)
        } else {
            let new = Rc::new(Cell::new(Either::Left(idx)));
            self.open_upvalues.push(Rc::clone(&new));
            new
        }
    }

    fn find_upvalue(&self, idx: u32) -> Option<(usize, &Upval)> {
        self.open_upvalues
            .iter()
            .enumerate()
            .find(|(_, v)| v.get().left().unwrap() == idx)
    }

    fn bind_method(&mut self, class: &MutRc<Class>, inst: &MutRc<Instance>, name: StrId) {
        let method = class.borrow().methods.get(name).cloned();
        if let Some(method) = method {
            let bound = BoundMethod {
                receiver: Value::Instance(Rc::clone(inst)),
                method: Rc::clone(method.as_closure()),
            };
            self.stack.push(Value::BoundMethod(Rc::new(bound)));
        } else {
            self.stack.push(Value::Nil);
        }
    }

    fn maybe_gc(&mut self) {
        if self.gc_thresh <= self.closed_upvalues.len() {
            self.collect_garbage();
            self.gc_thresh = self.closed_upvalues.len() * 2;
        }
    }

    fn collect_garbage(&mut self) {
        let mut keep = hashset(self.closed_upvalues.len());
        self.mark_all(
            &mut keep,
            self.stack.iter().chain(self.globals.values()),
            true,
        );
        self.closed_upvalues.retain(|k, _| keep.contains(k));
    }

    fn mark_all<'m>(
        &self,
        keep: &mut HashSet<u32>,
        iter: impl Iterator<Item = &'m Value>,
        rec: bool,
    ) {
        for val in iter {
            self.mark_value(keep, val, rec);
        }
    }

    fn mark_value<'m>(&self, keep: &mut HashSet<u32>, val: &'m Value, rec: bool) {
        let mark = |keep: &mut HashSet<u32>, cls: &ClosureObj| {
            for upval in cls.upvalues.iter().filter_map(|v| v.get().right()) {
                keep.insert(upval);
            }
        };

        match val {
            Value::Closure(cls) => mark(keep, cls),
            Value::Class(cls) => self.mark_all(keep, cls.borrow().methods.values(), false),
            Value::Instance(inst) if rec => {
                self.mark_all(keep, inst.borrow().fields.values(), false);
                self.mark_all(keep, inst.borrow().class.borrow().methods.values(), false);
            }
            Value::BoundMethod(method) if rec => self.mark_value(keep, &method.receiver, false),
            _ => (),
        };
    }

    fn unary_instruction(&mut self, opcode: &OpCode) -> Option<Value> {
        match opcode {
            OpCode::Negate => -self.stack_pop(),
            OpCode::Not => !self.stack_pop(),
            _ => panic!("unknown opcode"),
        }
    }

    fn binary_instruction(&mut self, opcode: &OpCode) -> Option<Value> {
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

    fn call_value(&mut self, callee: Value, arg_count: UInt) -> R<Option<CallFrame>> {
        let arity = callee.arity();
        if let Some(arity) = arity {
            if arg_count != arity && arity != ANY_ARITY {
                return Err(format!(
                    "Incorrect amount of function arguments (wanted {}, got {})",
                    arity, arg_count
                ));
            }
        } else {
            return Err("Can only call functions and classes.".to_string());
        }

        match callee {
            Value::Closure(cls) => self.call(cls, arg_count),

            Value::NativeFun(func) => {
                let mut args: SmallVec<[Value; 3]> = SmallVec::new();
                for _ in 0..arg_count {
                    args.push(self.stack.pop())
                }

                self.stack_pop(); // Pop the function still on the stack
                let result = (func.borrow().func)(&args);
                match result {
                    Ok(value) => {
                        self.stack.push(value);
                        Ok(None)
                    }

                    Err(msg) => Err(msg.to_string()),
                }
            }

            Value::Class(class) => {
                let initializer = class
                    .borrow()
                    .methods
                    .get(interner::intern("init"))
                    .cloned();

                let inst = Instance {
                    class,
                    fields: Map::new(),
                };

                let receiver_pos = self.stack.len() - arg_count - 1;
                self.stack[receiver_pos] = Value::Instance(Rc::new(RefCell::new(inst)));

                if let Some(init) = initializer {
                    self.call(init.into_closure(), arg_count)
                } else if arg_count != 0 {
                    Err(format!(
                        "Incorrect amount of constructor arguments (wanted {}, got {})",
                        0, arg_count
                    ))
                } else {
                    Ok(None)
                }
            }

            Value::BoundMethod(method) => {
                let receiver_pos = self.stack.len() - arg_count - 1;
                self.stack[receiver_pos] = method.receiver.clone();
                self.call(Rc::clone(&method.method), arg_count)
            }

            _ => panic!("unknown callee"),
        }
    }

    #[allow(clippy::unnecessary_wraps)]
    fn call(&mut self, closure: Rc<ClosureObj>, arg_count: UInt) -> R<Option<CallFrame>> {
        Ok(Some(CallFrame {
            closure,
            ip: 0,
            slot_offset: self.stack.len() - arg_count - 1,
        }))
    }

    fn invoke(&mut self, name: StrId, arg_count: UInt) -> R<Option<CallFrame>> {
        let receiver_pos = self.stack.len() - arg_count - 1;
        let inst = if let Value::Instance(inst) = &self.stack[receiver_pos] {
            inst
        } else {
            return Err("Only instances have methods.".to_string());
        };

        let field = inst.borrow().fields.get(name).cloned();
        if let Some(field) = field {
            self.stack[receiver_pos] = field.clone();
            return self.call_value(field, arg_count);
        }

        let class = Rc::clone(&inst.borrow().class);
        self.invoke_from_class(&class, name, arg_count)
    }

    fn invoke_from_class(
        &mut self,
        class: &MutRc<Class>,
        name: StrId,
        arg_count: UInt,
    ) -> R<Option<CallFrame>> {
        let method = class.borrow().methods.get(name).cloned();
        if let Some(method) = method {
            self.call(method.into_closure(), arg_count)
        } else {
            Err(format!("Undefined method {}.", interner::str(name)))
        }
    }

    fn print_error(&mut self, line: usize, message: &str) {
        println!("[Line {}] Runtime error: {}", line, message);
    }

    fn define_natives(&mut self) {
        self.define_native("clock", 0, |_| {
            let time = SystemTime::now().duration_since(UNIX_EPOCH);
            let time = time.unwrap();
            Ok(Value::Number(time.as_secs_f64()))
        });

        self.define_native("readfile", 1, |a| {
            let file = fs::read_to_string(a[0].to_string());
            Ok(file
                .map(|t| Value::DynString(Rc::from(t)))
                .unwrap_or(Value::Nil))
        });

        self.define_native("writefile", 2, |a| {
            let res = fs::write(a[1].to_string(), a[0].to_string());
            Ok(Value::Bool(res.is_ok()))
        });

        self.define_native("input", 0, |_| {
            let mut input = String::new();
            io::stdin()
                .read_line(&mut input)
                .expect("Failed to read line!");
            input.pop(); // Newline
            Ok(Value::DynString(Rc::from(input)))
        });
    }

    fn define_native(
        &mut self,
        name: &str,
        arity: UInt,
        func: fn(&[Value]) -> Result<Value, &str>,
    ) {
        let name = interner::intern(name);
        self.globals.insert(
            name,
            Value::NativeFun(Rc::new(RefCell::new(NativeFun { name, arity, func }))),
        );
    }

    pub fn new() -> VM {
        VM {
            stack: SVec::new(),
            globals: Map::new(),
            open_upvalues: SVec::new(),
            closed_upvalues: hashmap(5),
            upvalue_count: 0,
            gc_thresh: 5,
        }
    }
}

pub enum Failure {
    CompileError,
    RuntimeError,
}

#[derive(Debug)]
pub struct CallFrame {
    closure: Rc<ClosureObj>,
    ip: UInt,
    slot_offset: UInt,
}
