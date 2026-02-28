use std::collections::HashMap;
use ast::desugared_tree;
use crate::compiler::function_state::FunctionState;

mod function_state;

#[derive(Clone)]
pub enum Type {
    Bool,
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    F32,
    F64,
    Unit,
    Function {
        parameters: Vec<Type>,
        return_type: Box<Type>,
    }
}

struct Definition {
    public: bool,
    r#type: Type,
}

impl Definition {
    pub fn new(public: bool, r#type: Type) -> Self {
        Definition { public, r#type }
    }
}

struct ModuleDef {
    definitions: HashMap<String, Definition>
}

impl ModuleDef {
    fn new() -> ModuleDef {
        Self {
            definitions: HashMap::new()
        }
    }

    pub fn add_function(&mut self, function: &desugared_tree::Function) {
        let desugared_tree::Function {
            public,
            comptime,
            arguments,
            return_type,
            path,
            ..
        } = function;
        if *comptime {
            return
        }
        let mut parameters = Vec::new();
        for param in arguments.iter() {
            let ty = convert_type(&param.r#type);
            parameters.push(ty);
        }
        let return_type = Box::new(convert_type(&return_type));
        let r#type = Type::Function { parameters, return_type };
        let def = Definition::new(*public, r#type);
        let name = path.last().as_ref().unwrap().segment.clone();
        self.definitions.insert(name, def);
    }

    pub fn get_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }
}

pub struct Compiler {
    module_to_def: HashMap<Vec<String>, ModuleDef>,
    state: FunctionState,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            module_to_def: HashMap::new(),
            state: FunctionState::new(),
        }
    }
}



fn convert_type(r#type: &desugared_tree::Type) -> Type {
    match r#type {
        desugared_tree::Type::I8(_) => Type::I8,
        desugared_tree::Type::I16(_) => Type::I16,
        desugared_tree::Type::I32(_) => Type::I32,
        desugared_tree::Type::I64(_) => Type::I64,
        desugared_tree::Type::U8(_) => Type::U8,
        desugared_tree::Type::U16(_) => Type::U16,
        desugared_tree::Type::U32(_) => Type::U32,
        desugared_tree::Type::U64(_) => Type::U64,
        desugared_tree::Type::F32(_) => Type::F32,
        desugared_tree::Type::F64(_) => Type::F64,
        desugared_tree::Type::Unit(_) => Type::Unit,
        desugared_tree::Type::Bool(_) => Type::Bool,
        desugared_tree::Type::Function { args, r#return, .. } => {
            let parameters = args.iter().map(|param| convert_type(&param)).collect();
            let return_type = Box::new(convert_type(&r#return));
            Type::Function { parameters, return_type }
        }
        x => {
            println!("TODO: handle {:?}", x);
            Type::Unit
        }
    }
}