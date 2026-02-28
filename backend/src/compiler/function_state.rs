use std::collections::HashMap;
use crate::compiler::Type;


pub struct VariableHandle {
    pub r#type: Type,
    pub handle: Variable,
}

pub struct Scope {
    map: HashMap<String, VariableHandle>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
    
    pub fn get(&self, variable_name: &str) -> Option<&VariableHandle> {
        self.map.get(variable_name)
    }
    
    pub fn store(&mut self, variable_name: &str, location: VariableHandle) {
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
    
    pub fn get(&self, variable_name: &str) -> Option<&VariableHandle> {
        for scope in self.stack.iter().rev() {
            if let Some(location) = scope.get(variable_name) {
                return Some(location);
            }
        }
        None
    }
    
    pub fn store(&mut self, variable_name: &str, location: VariableHandle) {
        self.stack.last_mut().map(|scope| scope.store(variable_name, location));
    }
    
    pub fn push(&mut self) {
        self.stack.push(Scope::new());
    }
    
    pub fn pop(&mut self) {
        self.stack.pop();
    }
}