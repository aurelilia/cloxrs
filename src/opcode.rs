use crate::{interner::StrId, value::Closure, UInt};
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum OpCode {
    // Const opcodes - push the given value the stack
    ConstNil,
    ConstBool(bool),
    ConstNumber(f64),
    ConstString(StrId),

    /// Define a global with the given name, use top of stack as value
    DefineGlobal(StrId),

    /// Push global to stack
    GetGlobal(StrId),
    /// Set global to value on top of stack
    /// Essentially the same as DefineGlobal, but errors if global doesn't exist
    SetGlobal(StrId),

    /// Get an upvalue in the current function and push it to the stack.
    GetUpvalue(UInt),
    /// Set an upvalue in the current function to the top of the stack.
    SetUpvalue(UInt),

    /// Get a local variable on the stack and push it to the top of the stack.
    GetLocal(UInt),
    /// Set a local variable on the stack to the value on top of the stack.
    SetLocal(UInt),

    /// Get property of value currently on the stack and replace it.
    GetProperty(StrId),
    /// Set property of value below the top of the stack to value on top of the stack.
    SetProperty(StrId),
    /// Get method of superclass.
    GetSuper(StrId),

    /// Pop value off the stack.
    Pop,
    /// Pop value off, hoist into upvalue if captured.
    HoistUpvalue,

    // Simple matho operations, operands on the stack
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Not,
    Equal,
    Greater,
    Less,

    /// Pop and print value on top of stack to stdout
    Print,

    /// Add value to instruction pointer
    Jump(UInt),
    /// Jump if top of stack is false
    JumpIfFalse(UInt),
    /// Subtract value from instruction pointer
    Loop(UInt),

    /// Call: Function is on stack, followed by args on top;
    /// leave stack unmodified and add a new callframe
    /// Will ensure args count is correct
    Call(UInt),
    /// Invoke a class method, faster than bound methods
    Invoke(StrId, UInt),
    /// Invoke a superclass method, faster than bound methods
    InvokeSuper(StrId, UInt),

    /// Return from this callframe, push return value onto stack
    Return,

    /// Build a closure value
    Closure(Rc<Closure>),
    /// Build a class value
    Class(StrId),
    /// Finish a class value: Pops methods from the stack until it hits
    /// the class, then inserts popped methods into it
    EndClass(bool),
}
