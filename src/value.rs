use std::fmt;
use std::mem::discriminant;
use std::ops::*;
use smol_str::SmolStr;

use crate::chunk::Chunk;
use crate::MutRc;

#[derive(Debug, Clone, PartialEq, EnumAsGetters, EnumIsA)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(SmolStr),
    Function(MutRc<Function>),
    NativeFun(MutRc<NativeFun>)
}

fn print_fn_name(name: &Option<SmolStr>, f: &mut fmt::Formatter) -> fmt::Result {
    match name {
        Some(name) => write!(f, "<fn {}>", name),
        None => write!(f, "<script>")
    }
}

#[derive(PartialEq)]
pub struct Function {
    pub name: Option<SmolStr>,
    pub arity: usize,
    pub chunk: Chunk,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&self.name, f)
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&self.name, f)
    }
}

pub struct NativeFun {
    pub name: Option<SmolStr>,
    pub arity: usize,
    pub func: Box<dyn Fn(&[Value]) -> Value>
}

impl fmt::Display for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&self.name, f)
    }
}

impl fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&self.name, f)
    }
}

impl PartialEq for NativeFun {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(value) => !*value,
            Value::Nil => true,
            _ => false,
        }
    }

    fn same_type_as(&self, other: &Value) -> bool {
        discriminant(self) == discriminant(other)
    }

    pub fn less(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) && self.is_number() {
            Some(Value::Bool(self.as_number() < other.as_number()))
        } else {
            None
        }
    }

    pub fn greater(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) && self.is_number() {
            Some(Value::Bool(self.as_number() > other.as_number()))
        } else {
            None
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(val) => write!(f, "{}", val),
            Value::String(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
            Value::Function(func) => write!(f, "{}", func.borrow()),
            Value::NativeFun(func) => write!(f, "{}", func.borrow()),
        }
    }
}

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, other: Value) -> Option<Value> {
        if self.is_number() && other.is_number() {
            Some(Value::Number(self.as_number() + other.as_number()))
        } else {
            Some(Value::String(SmolStr::new(self.to_string() + &other.to_string())))
        }
    }
}

impl Sub for Value {
    type Output = Option<Value>;

    fn sub(self, other: Value) -> Option<Value> {
        if self.is_number() && other.is_number() {
            Some(Value::Number(self.as_number() - other.as_number()))
        } else {
            None
        }
    }
}

impl Mul for Value {
    type Output = Option<Value>;

    fn mul(self, other: Value) -> Option<Value> {
        if self.is_number() && other.is_number() {
            Some(Value::Number(self.as_number() - other.as_number()))
        } else {
            None
        }
    }
}

impl Div for Value {
    type Output = Option<Value>;

    fn div(self, other: Value) -> Option<Value> {
        if self.is_number() && other.is_number() {
            Some(Value::Number(self.as_number() / other.as_number()))
        } else {
            None
        }
    }
}

impl Neg for Value {
    type Output = Option<Value>;

    fn neg(self) -> Option<Value> {
        match self {
            Value::Number(value) => Some(Value::Number(-value)),
            _ => None,
        }
    }
}

impl Not for Value {
    type Output = Option<Value>;

    fn not(self) -> Option<Value> {
        Some(Value::Bool(self.is_falsey()))
    }
}
