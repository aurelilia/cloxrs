use std::fmt;
use std::mem::discriminant;
use std::ops::*;
use std::rc::Rc;

#[derive(Debug, Clone, EnumAsGetters)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(Rc<String>),
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

    fn to_string(&self) -> String {
        match self {
            Value::Bool(val) => val.to_string(),
            Value::Nil => String::from("nil"),
            Value::Number(val) => val.to_string(),
            Value::String(val) => String::clone(&*val),
        }
    }

    pub fn equal(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            Some(Value::Bool(match self {
                Value::Number(value) => value == other.as_number(),
                Value::Bool(value) => value == other.as_bool(),
                Value::Nil => true,
                Value::String(value) => value == other.as_string(),
            }))
        } else {
            Some(Value::Bool(false))
        }
    }

    pub fn less(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            match self {
                Value::Number(value) => Some(Value::Bool(
                    *value
                        < if let Value::Number(val) = other {
                            val
                        } else {
                            0.0
                        },
                )),
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn greater(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            match self {
                Value::Number(value) => Some(Value::Bool(
                    *value
                        > if let Value::Number(val) = other {
                            val
                        } else {
                            0.0
                        },
                )),
                _ => None,
            }
        } else {
            None
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Number(num) => write!(f, "{}", num),
            Value::String(srg) => write!(f, "{}", srg),
            Value::Bool(boo) => write!(f, "{}", boo),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_num) = other {
                    Some(Value::Number(value + other_num))
                } else {
                    Some(Value::String(Rc::new(self.to_string() + &other.to_string())))
                }
            }

            _ => Some(Value::String(Rc::new(self.to_string() + &other.to_string()))),
        }
    }
}

impl Sub for Value {
    type Output = Option<Value>;

    fn sub(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Some(Value::Number(value - other_val))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl Mul for Value {
    type Output = Option<Value>;

    fn mul(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Some(Value::Number(value * other_val))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl Div for Value {
    type Output = Option<Value>;

    fn div(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Some(Value::Number(value / other_val))
                } else {
                    None
                }
            }
            _ => None,
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
