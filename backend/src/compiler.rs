use std::collections::HashMap;
use cranelift_codegen::{isa, settings};
use cranelift_codegen::isa::OwnedTargetIsa;
use cranelift_codegen::settings::Configurable;
use ast::desugared_tree;
use crate::compiler::function_state::FunctionState;

mod function_state;

pub enum CompileError {
    Many(Vec<CompileError>),
}

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

struct ModuleDef<'input> {
    definitions: HashMap<String, Definition>,
    module: Option<desugared_tree::File<'input>>,
}

impl<'input> ModuleDef<'input> {
    fn new(module: desugared_tree::File<'input>) -> Self {
        let mut out = Self {
            definitions: HashMap::new(),
            module: None,
        };

        for func in module.functions.iter() {
            out.add_function(func);
        }
        out.module = Some(module);
        out
    }

    fn add_function(&mut self, function: &desugared_tree::Function) {
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

pub struct Compiler<'input> {
    module_to_def: HashMap<Vec<String>, ModuleDef<'input>>,
    state: FunctionState,
    isa: OwnedTargetIsa,
}

impl<'input> Compiler<'input> {
    pub fn new() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("opt_level", "none").unwrap();
        let flags = settings::Flags::new(flag_builder);

        let isa = isa::lookup_by_name("wasm32").unwrap().finish(flags).unwrap();

        Self {
            module_to_def: HashMap::new(),
            state: FunctionState::new(),
            isa,
        }
    }

    fn load_files(&mut self, files: Vec<desugared_tree::File<'input>>) {
        for file in files {
            let module_path = file.file_path.iter()
                .map(|s| s.to_string_lossy().to_string())
                .map(|s| if s.contains(".zerg") {
                    s.replace(".zerg", "")
                } else {
                    s
                })
                .collect::<Vec<String>>();
            let module = ModuleDef::new(file);

            self.module_to_def.insert(module_path, module);
        }
    }

    pub fn compile_files(&mut self, files: Vec<desugared_tree::File<'input>>) -> Result<(), CompileError> {
        self.load_files(files.clone());

        let mut errors: Vec<CompileError> = Vec::new();

        for file in files {
            match self.compile_file(file) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            }
        }
        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }
        Ok(())
    }

    fn compile_file(&mut self, file: desugared_tree::File<'input>) -> Result<(), CompileError> {

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