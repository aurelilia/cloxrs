use either::Either;
use smallvec::SmallVec;
use std::{cell::Cell, fmt, mem::discriminant, ops::*, rc::Rc};

use crate::{chunk::Chunk, UInt};
use crate::{
    interner::{self, Map, StrId},
    MutRc,
};

// left: open, right: closed
pub type Upval = Rc<Cell<Either<u32, u32>>>;

#[derive(Debug, Clone, PartialEq, EnumAsGetters, EnumIsA, EnumIntoGetters)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    ConstString(StrId),
    DynString(Rc<str>),
    Closure(Rc<ClosureObj>),
    NativeFun(MutRc<NativeFun>),
    Class(MutRc<Class>),
    Instance(MutRc<Instance>),
    BoundMethod(Rc<BoundMethod>),
}

fn print_fn_name(name: &Option<StrId>, f: &mut fmt::Formatter) -> fmt::Result {
    match name {
        Some(name) => write!(f, "<fn {}>", interner::str(*name)),
        None => write!(f, "<script>"),
    }
}

#[derive(PartialEq)]
pub struct Function {
    pub name: Option<StrId>,
    pub arity: UInt,
    pub chunk: Chunk,
    pub upvalue_count: UInt,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub function: MutRc<Function>,
    pub upvalues: SmallVec<[Upvalue; 3]>,
}

impl Closure {
    pub fn to_obj(&self) -> Rc<ClosureObj> {
        Rc::new(ClosureObj {
            function: Rc::clone(&self.function),
            upvalues: SmallVec::new(),
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosureObj {
    pub function: MutRc<Function>,
    pub upvalues: SmallVec<[Upval; 3]>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct Upvalue {
    pub index: UInt,
    pub is_local: bool,
}
pub struct NativeFun {
    pub name: StrId,
    pub arity: UInt,
    pub func: fn(&[Value]) -> Result<Value, &str>,
}

impl fmt::Display for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&Some(self.name), f)
    }
}

impl fmt::Debug for NativeFun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        print_fn_name(&Some(self.name), f)
    }
}

impl PartialEq for NativeFun {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: StrId,
    pub methods: Map<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    pub class: MutRc<Class>,
    pub fields: Map<Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: Rc<ClosureObj>,
}

pub const ANY_ARITY: UInt = 34875874;

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

    pub fn arity(&self) -> Option<UInt> {
        match self {
            Value::Closure(c) => Some(c.function.borrow().arity),
            Value::NativeFun(f) => Some(f.borrow().arity),
            Value::Class(_) => Some(ANY_ARITY),
            Value::BoundMethod(method) => Some(method.method.function.borrow().arity),
            _ => None,
        }
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
            Value::ConstString(val) => write!(f, "{}", interner::str(*val)),
            Value::DynString(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
            Value::Closure(func) => write!(f, "{}", func.function.borrow()),
            Value::NativeFun(func) => write!(f, "{}", func.borrow()),
            Value::Class(cls) => write!(f, "<class {}>", interner::str(cls.borrow().name)),
            Value::Instance(obj) => write!(
                f,
                "<{} instance>",
                interner::str(obj.borrow().class.borrow().name)
            ),
            Value::BoundMethod(meth) => write!(
                f,
                "<bound method {}>",
                interner::str(meth.method.function.borrow().name.unwrap())
            ),
        }
    }
}

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, other: Value) -> Option<Value> {
        if self.is_number() && other.is_number() {
            Some(Value::Number(self.as_number() + other.as_number()))
        } else {
            Some(Value::DynString(Rc::from(
                (self.to_string() + &other.to_string()).as_str(),
            )))
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
