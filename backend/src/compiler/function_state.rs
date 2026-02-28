use std::collections::HashMap;
use crate::compiler::Type;

/// Indicates where a value is currently located in the wasm virtual machine
pub enum VariableLocation {
    /// Indicates that the value is a local variable at what index
    Local(u64),
    /// Indicates that the value is on the stack and what level it is on.
    Stack(u64),
}

pub struct Variable {
    pub r#type: Type,
    pub location: VariableLocation,
}

pub struct Scope {
    map: HashMap<String, Variable>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    
    pub fn get(&self, variable_name: &str) -> Option<&Variable> {
        self.map.get(variable_name)
    }
    
    pub fn store(&mut self, variable_name: &str, location: Variable) {
        self.map.insert(variable_name.to_string(), location);
    }
}

pub struct FunctionState {
    stack: Vec<Scope>,
}

impl FunctionState {
    pub fn new() -> Self {
        Self {
            stack: vec![],
        }
    }
    
    pub fn get(&self, variable_name: &str) -> Option<&Variable> {
        for scope in self.stack.iter().rev() {
            if let Some(location) = scope.get(variable_name) {
                return Some(location);
            }
        }
        None
    }
    
    pub fn store(&mut self, variable_name: &str, location: Variable) {
        self.stack.last_mut().map(|scope| scope.store(variable_name, location));
    }
    
    pub fn push(&mut self) {
        self.stack.push(Scope::new());
    }
    
    pub fn pop(&mut self) {
        self.stack.pop();
    }
}