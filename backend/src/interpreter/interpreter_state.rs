use std::collections::HashMap;

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