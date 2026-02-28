use std::collections::HashMap;
use wasm_encoder::ValType;
use crate::compiler::Type;


pub struct VariableHandle {
    pub r#type: Type,
    pub handle: u32,
}

impl VariableHandle {
    pub fn new(r#type: Type) -> VariableHandle {
        Self { r#type, handle: 0 }
    }
    
    pub fn set_handle(&mut self, handle: u32) {
        self.handle = handle;
    }
}

pub struct Scope {
    map: HashMap<String, VariableHandle>,
    next_handle: u32,
}

impl Scope {
    pub fn new(next_handle: u32) -> Self {
        Self {
            map: HashMap::new(),
            next_handle,
        }
    }

    pub fn get(&self, variable_name: &str) -> Option<&VariableHandle> {
        self.map.get(variable_name)
    }

    pub fn get_mut(&mut self, variable_name: &str) -> Option<&mut VariableHandle> {
        self.map.get_mut(variable_name)
    }

    pub fn store(&mut self, variable_name: &str, mut location: VariableHandle) {
        location.set_handle(self.next_handle);
        self.map.insert(variable_name.to_string(), location);
    }

    /// Checks if this scope has a variable with the same name and type.
    /// If types, don't align, then we can't replace it
    /// When Some, it contains the index that can be replaced
    pub fn can_replace(&self, variable_name: &str, location: &VariableHandle) -> Option<u32> {
        if let Some(old_location) = self.map.get(variable_name) {
            if old_location.r#type == location.r#type {
                return Some(old_location.handle);
            }
        }
        None
    }
    
    pub fn next_handle(&self) -> u32 {
        self.next_handle
    }
}

pub struct FunctionState {
    stack: Vec<Scope>,
    current_scope: usize,
}

impl FunctionState {
    pub fn new() -> Self {
        Self {
            stack: vec![Scope::new(0)],
            current_scope: 0,
        }
    }

    pub fn get(&self, variable_name: &str) -> Option<&VariableHandle> {
        for scope in self.stack.iter().take(self.current_scope).rev() {
            if let Some(location) = scope.get(variable_name) {
                return Some(location);
            }
        }
        None
    }

    pub fn store(&mut self, variable_name: &str, mut location: VariableHandle) {
        for scope in self.stack.iter_mut().take(self.current_scope).rev() {
            if let Some(handle) = scope.can_replace(variable_name, &location) {
                let Some(old_location) = scope.get_mut(variable_name) else {
                    break;
                };
                location.set_handle(handle);
                *old_location = location;
                return;
            }
        }

        let next_location = self.stack[self.current_scope].next_handle();
        location.set_handle(next_location);
        self.stack[self.current_scope].store(variable_name, location);
    }
    
    pub fn add_scope(&mut self) {
        let next_handle = self.stack[self.current_scope].next_handle();
        self.stack.push(Scope::new(next_handle));
        self.current_scope += 1;
    }

    pub fn push(&mut self) {
        self.current_scope += 1;
    }

    pub fn pop(&mut self) {
        self.current_scope -= 1;
    }
    
    pub fn clear(&mut self) {
        self.stack.clear();
        self.stack.push(Scope::new(0));
    }
}