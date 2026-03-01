use std::collections::HashMap;
use crate::{compiler, VariableLocation};

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Value {
    Bool(bool),
    U8(u8),
    I8(i8),
    U16(u16),
    I16(i16),
    U32(u32),
    I32(i32),
    U64(u64),
    I64(i64),
    F32(f32),
    F64(f64),
    Unit,
    String(String),
    VariableHandle(VariableLocation),
}

impl Value {
    pub fn type_name(&self) -> String {
        match self {
            Value::Bool(_) => "bool".to_string(),
            Value::U8(_) => "u8".to_string(),
            Value::I8(_) => "i8".to_string(),
            Value::U16(_) => "u16".to_string(),
            Value::I16(_) => "i16".to_string(),
            Value::U32(_) => "u32".to_string(),
            Value::I32(_) => "i32".to_string(),
            Value::U64(_) => "u64".to_string(),
            Value::I64(_) => "i64".to_string(),
            Value::F32(_) => "f32".to_string(),
            Value::F64(_) => "f64".to_string(),
            Value::Unit => "unit".to_string(),
            Value::String(_) => "String".to_string(),
            Value::VariableHandle(location) => {
                match location {
                    VariableLocation::I32 { original_type, ..} | VariableLocation::I64 { original_type, .. } | VariableLocation::FunctionArg { original_type, .. } => {
                        let mut out = String::from("VariableHandle<");
                        match original_type {
                            compiler::Type::Bool => out.push_str("bool"),
                            compiler::Type::U8 => out.push_str("u8"),
                            compiler::Type::I8 => out.push_str("i8"),
                            compiler::Type::U16 => out.push_str("u16"),
                            compiler::Type::I16 => out.push_str("i16"),
                            compiler::Type::U32 => out.push_str("u32"),
                            compiler::Type::I32 => out.push_str("i32"),
                            compiler::Type::U64 => out.push_str("u64"),
                            compiler::Type::I64 => out.push_str("i64"),
                            _ => unreachable!()
                        }
                        out.push('>');
                        out
                    }
                    VariableLocation::F32(_) => String::from("VariableHandle<f32>"),
                    VariableLocation::F64(_) => String::from("VariableHandle<f64>"),
                }
            }
        }
    }
}

pub struct Scope {
    map: HashMap<String, Value>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, key: String, value: Value) {
        self.map.insert(key, value);
    }

    pub fn get(&self, key: &str) -> Option<&Value> {
        self.map.get(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.map.get_mut(key)
    }
}


pub struct InterpreterState {
    stack: Vec<Scope>,
}

impl InterpreterState {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
        }
    }

    pub fn store(&mut self, name: &str, value: Value) {
        for scope in self.stack.iter_mut().rev() {
            if let Some(old_value) = scope.map.get_mut(name) {
                *old_value = value;
                return;
            }
        }
        self.stack.last_mut().map(|scope| scope.insert(name.to_string(), value));
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        for scope in self.stack.iter().rev() {
            if let Some(old_value) = scope.map.get(name) {
                return Some(old_value);
            }
        }
        None
    }

    pub fn push(&mut self) {
        self.stack.push(Scope::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}