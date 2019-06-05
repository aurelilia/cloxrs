use std::ops::*;

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Double(f64)
}

impl Add for Value {
    type Output = Value;

    fn add(self, other: Value) -> Value {
        match self {
            Value::Double(value) => {
                if let Value::Double(other_val) = other {
                    Value::Double(value + other_val)
                } else {
                    Value::Double(value)
                }
            }
        }
    }
} 

impl Sub for Value {
    type Output = Value;
 
    fn sub(self, other: Value) -> Value {
        match self {
            Value::Double(value) => {
                if let Value::Double(other_val) = other {
                    Value::Double(value - other_val)
                } else {
                    Value::Double(value)
                }
            }
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, other: Value) -> Value {
        match self {
            Value::Double(value) => {
                if let Value::Double(other_val) = other {
                    Value::Double(value * other_val)
                } else {
                    Value::Double(value)
                }
            }
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, other: Value) -> Value {
        match self {
            Value::Double(value) => {
                if let Value::Double(other_val) = other {
                    Value::Double(value / other_val)
                } else {
                    Value::Double(value)
                }
            }
        }
    }
}

impl Neg for Value {
    type Output = Value;

    fn neg(self) -> Value {
        match self {
            Value::Double(value) => {
                Value::Double(-value)
            }
        }
    }
}