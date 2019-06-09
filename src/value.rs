use std::mem::discriminant;
use std::ops::*;

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(String)
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Bool(value) => *value,
            Value::Nil => false,
            _ => true
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
            Value::String(val) => val.clone(),
        }
    }

    pub fn equal(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            Option::Some(Value::Bool(match self {
                Value::Number(value) => *value == if let Value::Number(val) = other { val } else { 0.0 },
                Value::Bool(value) => *value == if let Value::Bool(val) = other { val } else { false },
                Value::Nil => true,
                Value::String(value) => *value == if let Value::String(val) = other { val } else { String::new() }
            }))
        } else {
            Option::Some(Value::Bool(false))
        }
    }

    pub fn less(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            match self {
                Value::Number(value) => Option::Some(Value::Bool(
                    *value < if let Value::Number(val) = other { val } else { 0.0 })),
                _ => Option::None
            }
        } else {
            Option::Some(Value::Bool(false))
        }
    }
        
    pub fn greater(&self, other: Value) -> Option<Value> {
        if self.same_type_as(&other) {
            match self {
                Value::Number(value) => Option::Some(Value::Bool(
                    *value > if let Value::Number(val) = other { val } else { 0.0 })),
                _ => Option::None
            }
        } else {
            Option::Some(Value::Bool(false))
        }
    }
}

impl Add for Value {
    type Output = Option<Value>;

    fn add(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                match other {
                    Value::Number(other_val) => Option::Some(Value::Number(value + other_val)),
                    _ => Option::None
                }
            },
            
            Value::String(value) => Option::Some(Value::String(value + &other.to_string())),
            _ => {
                if let Value::String(other_val) = other {
                    Option::Some(Value::String(self.to_string() + &other_val))
                } else {
                    Option::None
                }
            }
        }
    }
} 

impl Sub for Value {
    type Output = Option<Value>;

    fn sub(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Option::Some(Value::Number(value - other_val))
                } else {
                    Option::None
                }
            }
            _ => Option::None
        }
    }
} 

impl Mul for Value {
    type Output = Option<Value>;

    fn mul(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Option::Some(Value::Number(value * other_val))
                } else {
                    Option::None
                }
            }
            _ => Option::None
        }
    }
} 

impl Div for Value {
    type Output = Option<Value>;

    fn div(self, other: Value) -> Option<Value> {
        match self {
            Value::Number(value) => {
                if let Value::Number(other_val) = other {
                    Option::Some(Value::Number(value / other_val))
                } else {
                    Option::None
                }
            }
            _ => Option::None
        }
    }
} 

impl Neg for Value {
    type Output = Option<Value>;

    fn neg(self) -> Option<Value> {
        match self {
            Value::Number(value) => {
                Option::Some(Value::Number(-value))
            }
            _ => Option::None
        }
    }
}

impl Not for Value {
    type Output = Option<Value>;

    fn not(self) -> Option<Value> {
        Option::Some(Value::Bool(self.is_falsey()))
    }
}
