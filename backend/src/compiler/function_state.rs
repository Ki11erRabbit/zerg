use std::collections::HashMap;
use wasm_encoder::ValType;
use crate::compiler::Type;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum VariableLocation {
    I32 {
        index: u32,
        original_type: Type,
    },
    I64 {
        index: u32,
        original_type: Type,
    },
    F32(u32),
    F64(u32),
    FunctionArg {
        index: u32,
        original_type: Type,
    }
}

impl VariableLocation {
    pub fn get_index(&self) -> u32 {
        match self {
            VariableLocation::I32 { index, .. } => *index,
            VariableLocation::I64 { index, .. } => *index,
            VariableLocation::F32(index) => *index,
            VariableLocation::F64(index) => *index,
            VariableLocation::FunctionArg { index, .. } => *index,
        }
    }
    
    pub fn get_type(&self) -> Type {
        match self {
            VariableLocation::I32 { original_type, .. } => original_type.clone(),
            VariableLocation::I64 { original_type, .. } => original_type.clone(),
            VariableLocation::F32(ty) => Type::F32,
            VariableLocation::F64(ty) => Type::F64,
            VariableLocation::FunctionArg { original_type, .. } => original_type.clone(),
        }
    }
    
    pub fn update_index(
        &mut self, 
        function_arguments: u32,
        i32_var_count: u32,
        i64_var_count: u32,
        f32_var_count: u32,
    ) {
        match self {
            VariableLocation::I32 { index, .. } => {
                *index = function_arguments + *index;
            }
            VariableLocation::I64 { index, .. } => {
                *index = function_arguments + i32_var_count + *index;
            }
            VariableLocation::F32(index) => {
                *index = function_arguments + i32_var_count + i64_var_count + *index;
            }
            VariableLocation::F64(index) => {
                *index = function_arguments + i32_var_count + i64_var_count + f32_var_count + *index;
            }
            VariableLocation::FunctionArg { .. } => {}
        }
    }
}


pub struct Scope {
    map: HashMap<String, VariableLocation>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn get(&self, variable_name: &str) -> Option<&VariableLocation> {
        self.map.get(variable_name)
    }

    pub fn get_mut(&mut self, variable_name: &str) -> Option<&mut VariableLocation> {
        self.map.get_mut(variable_name)
    }

    pub fn store(&mut self, variable_name: &str, location: VariableLocation) {
        self.map.insert(variable_name.to_string(), location);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &VariableLocation)> {
        self.map.iter()
    }
}

pub struct FunctionState {
    stack: Vec<Scope>,
    pub function_arguments: u32,
    pub i32_var_count: u32,
    pub i64_var_count: u32,
    pub f32_var_count: u32,
    pub f64_var_count: u32,
}

impl FunctionState {
    pub fn new() -> Self {
        Self {
            stack: vec![Scope::new()],
            function_arguments: 0,
            i32_var_count: 0,
            i64_var_count: 0,
            f32_var_count: 0,
            f64_var_count: 0,
        }
    }

    pub fn get(&self, variable_name: &str) -> Option<u32> {
        for scope in self.stack.iter().rev() {
            if let Some(location) = scope.get(variable_name) {
                let new_location = match location {
                    VariableLocation::I32 { index, .. } => {
                        self.function_arguments + index
                    }
                    VariableLocation::I64 { index, .. } => {
                        self.function_arguments + self.i32_var_count + index
                    }
                    VariableLocation::F32(index) => {
                        self.f32_var_count + self.i32_var_count + self.i64_var_count + index
                    }
                    VariableLocation::F64(index) => {
                        self.f32_var_count + self.i32_var_count + self.i64_var_count + self.i64_var_count + index
                    }
                    VariableLocation::FunctionArg { index, ..} => {
                        *index
                    }
                };
                return Some(new_location)
            }
        }
        None
    }

    pub fn store_function_argument(&mut self, variable_name: &str, ty: Type, index: u32) {
        self.stack[0].store(variable_name, VariableLocation::FunctionArg { index, original_type: ty });
        self.function_arguments += 1;
    }

    pub fn store(&mut self, variable_name: &str, ty: Type) {
        for scope in self.stack.iter_mut().rev() {
            if let Some(location) = scope.get(variable_name) {
                // Check to see if we have a compatible variable with same name.
                match (location, &ty) {
                    (VariableLocation::I32 { original_type: Type::U8, .. }, Type::U8) => return,
                    (VariableLocation::I32 { original_type: Type::I8, .. }, Type::I8) => return,
                    (VariableLocation::I32 { original_type: Type::U16, .. }, Type::U16) => return,
                    (VariableLocation::I32 { original_type: Type::I16, .. }, Type::I16) => return,
                    (VariableLocation::I32 { original_type: Type::U32, .. }, Type::U32) => return,
                    (VariableLocation::I32 { original_type: Type::I32, .. }, Type::I32) => return,
                    (VariableLocation::I64 { original_type: Type::U64, .. }, Type::U64) => return,
                    (VariableLocation::I64 { original_type: Type::I64, .. }, Type::I64) => return,
                    (VariableLocation::F32(_), Type::F32) => return,
                    (VariableLocation::F64(_), Type::F64) => return,
                    _ => break,
                }
            }
        }

        let location = match &ty {
            Type::Bool | Type::U8 | Type::I8 | Type::U16 | Type::I16
            | Type::U32 | Type::I32 | Type::Unit =>  {
                let location = VariableLocation::I32 {
                    index: self.i32_var_count,
                    original_type: ty,
                };
                self.i32_var_count += 1;
                location
            }
            Type::U64 | Type::I64 | Type::Function { .. } => {
                let location = VariableLocation::I64 {
                    index: self.i64_var_count,
                    original_type: ty,
                };
                self.i64_var_count += 1;
                location
            }
            Type::F32 => {
                let location = VariableLocation::F32(self.f32_var_count);
                self.f32_var_count += 1;
                location
            }
            Type::F64 => {
                let location = VariableLocation::F64(self.f64_var_count);
                self.f64_var_count += 1;
                location
            }
        };

        self.stack.last_mut().map(|scope| scope.store(variable_name, location));

    }


    pub fn push(&mut self) {
        self.stack.push(Scope::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn clear(&mut self) {
        self.stack.clear();
        self.stack.push(Scope::new());
        self.i32_var_count = 0;
        self.i64_var_count = 0;
        self.f32_var_count = 0;
        self.f64_var_count = 0;
        self.function_arguments = 0;
    }

    pub fn local_definitions(&self) -> Vec<(u32, ValType)> {
        vec![
            (self.i32_var_count, ValType::I32),
            (self.i64_var_count, ValType::I64),
            (self.f32_var_count, ValType::F32),
            (self.f64_var_count, ValType::F64)
        ]
    }

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.stack.iter()
    }
}