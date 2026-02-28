use std::collections::HashMap;
use wasm_encoder::{CodeSection, ExportKind, ExportSection, Function, FunctionSection, Module, TypeSection, ValType};
use ast::desugared_tree;
use crate::compiler::function_state::{FunctionState, VariableHandle, VariableLocation};

mod function_state;

pub enum CompileError {
    Many(Vec<CompileError>),
}

#[derive(Clone, PartialEq)]
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
    type_index: u32,
}

impl Definition {
    pub fn new(type_index: u32) -> Self {
        Definition { type_index }
    }
}

struct ModuleDef<'input> {
    definitions: HashMap<String, Definition>,
    file: Option<desugared_tree::File<'input>>,
}

impl<'input> ModuleDef<'input> {
    fn new(file: desugared_tree::File<'input>) -> (Self, WasmModuleGroup) {
        let module = Module::new();
        let types = TypeSection::new();
        let functions = FunctionSection::new();
        let exports = ExportSection::new();
        let code = CodeSection::new();
        let mut wasm_module = WasmModuleGroup {
            module,
            types,
            functions,
            exports,
            code,
        };
        
        let mut out = Self {
            definitions: HashMap::new(),
            file: None,
        };

        for (i, func) in file.functions.iter().enumerate() {
            out.add_function(i as u32, func, &mut wasm_module);
        }
        out.file = Some(file);
        (out, wasm_module)
    }

    fn add_function(&mut self, index: u32, function: &desugared_tree::Function, module: &mut WasmModuleGroup) {
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
            let ty = convert_type(&param.r#type).1;
            if let Some(ty) = ty {
                parameters.push(ty);
            }
        }
        let return_type = convert_type(&return_type).1;
        let return_type = if let Some(ty) = return_type {
            vec![ty]
        } else {
            Vec::new()
        };
        module.types.ty().function(parameters, return_type);

        let name = path.last().as_ref().unwrap().segment.clone();
        module.functions.function(index);
        if *public {
            module.exports.export(&name, ExportKind::Func, index);
        }
        
        let def = Definition::new(index);
        self.definitions.insert(name, def);
    }

    pub fn get_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }
}

struct WasmModuleGroup {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    code: CodeSection,
}

pub struct Compiler<'input> {
    module_to_def: HashMap<Vec<String>, ModuleDef<'input>>,
    state: FunctionState,
}

impl<'input> Compiler<'input> {
    pub fn new() -> Self {
        Self {
            module_to_def: HashMap::new(),
            state: FunctionState::new(),
        }
    }

    fn load_files(&mut self, files: Vec<desugared_tree::File<'input>>) -> Vec<WasmModuleGroup> {
        let mut wasm_modules = Vec::new();
        for file in files {
            let module_path = file.file_path.iter()
                .map(|s| s.to_string_lossy().to_string())
                .map(|s| if s.contains(".zerg") {
                    s.replace(".zerg", "")
                } else {
                    s
                })
                .collect::<Vec<String>>();
            let (module_def, wasm_module) = ModuleDef::new(file);

            self.module_to_def.insert(module_path, module_def);
            wasm_modules.push(wasm_module);
        }
        wasm_modules
    }

    pub fn compile_files(&mut self, files: Vec<desugared_tree::File<'input>>) -> Result<(), CompileError> {
        let mut wasm_modules = self.load_files(files.clone());

        let mut errors: Vec<CompileError> = Vec::new();

        for (file, module) in files.into_iter().zip(wasm_modules.iter_mut()) {
            match self.compile_file(module, file) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            }
        }
        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }

        Ok(())
    }

    fn compile_file(&mut self, module: &mut WasmModuleGroup, file: desugared_tree::File<'input>) -> Result<(), CompileError> {
        let desugared_tree::File { functions, .. } = file;
        let mut errors: Vec<CompileError> = Vec::new();
        for func in functions {
            match self.compile_function(module, func) {
                Ok(_) => (),
                Err(error) => errors.push(error),
            }
        }

        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }
        Ok(())
    }

    fn compile_function(&mut self, module: &mut WasmModuleGroup, function: desugared_tree::Function) -> Result<(), CompileError> {
        let desugared_tree::Function {
            arguments,
            body,
            ..
        } = function;
        for (i, arg) in arguments.iter().enumerate() {
            let ty = convert_type(&arg.r#type).0;
            self.state.store_function_argument(&arg.name, ty, i as u32);
        }
        self.bind_block(&body);
        let locals = self.state.local_definitions();
        let mut function = Function::new(locals);

        self.state.clear();

        module.code.function(&function);

        Ok(())
    }


}


impl<'input> Compiler<'input> {
    fn bind_block(&mut self, block: &desugared_tree::Block) {
        self.state.push();
        for stmt in block.statements.iter() {
            match stmt {
                desugared_tree::Statement::Let { name, r#type, expr, .. } => {
                    let ty = convert_type(&r#type).0;
                    self.state.store(&name, ty);

                    self.bind_expr(expr);
                }
                desugared_tree::Statement::Expression { expr, ..} => {
                    self.bind_expr(expr);
                }
                desugared_tree::Statement::Assignment { expr, .. } => {
                    self.bind_expr(expr);
                }
                _ => {}
            }
        }
        self.state.pop();
    }

    fn bind_expr(&mut self, expr: &desugared_tree::Expression) {
        match expr {
            desugared_tree::Expression::IfExpression(if_expr) => {
                self.bind_if_expr(if_expr);
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                self.bind_expr(expr);
            }
            _ => {}
        }
    }

    fn bind_if_expr(&mut self, if_expr: &desugared_tree::IfExpression) {
        let desugared_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            ..
        } = if_expr;

        self.bind_expr(condition.as_ref());
        self.bind_block(then_block);
        for (condition, block) in elifs {
            self.bind_expr(condition);
            self.bind_block(block);
        }
        else_block.as_ref().map(|else_block| {
            self.bind_block(else_block);
        });
    }
}



fn convert_type(r#type: &desugared_tree::Type) -> (Type, Option<ValType>) {
    match r#type {
        desugared_tree::Type::I8(_) => (Type::I8, Some(ValType::I32)),
        desugared_tree::Type::I16(_) => (Type::I16, Some(ValType::I32)),
        desugared_tree::Type::I32(_) => (Type::I32, Some(ValType::I32)),
        desugared_tree::Type::I64(_) => (Type::I64, Some(ValType::I64)),
        desugared_tree::Type::U8(_) => (Type::U8, Some(ValType::I32)),
        desugared_tree::Type::U16(_) => (Type::U16, Some(ValType::I32)),
        desugared_tree::Type::U32(_) => (Type::U32, Some(ValType::I32)),
        desugared_tree::Type::U64(_) => (Type::U64, Some(ValType::I64)),
        desugared_tree::Type::F32(_) => (Type::F32, Some(ValType::F32)),
        desugared_tree::Type::F64(_) => (Type::F64, Some(ValType::F64)),
        desugared_tree::Type::Unit(_) => (Type::Unit, None),
        desugared_tree::Type::Bool(_) => (Type::Bool, Some(ValType::I32)),
        desugared_tree::Type::Function { args, r#return, .. } => {
            let parameters = args.iter().map(|param| convert_type(&param).0).collect();
            let return_type = Box::new(convert_type(&r#return).0);
            (Type::Function { parameters, return_type }, Some(ValType::I64))
        }
        x => {
            println!("TODO: handle {:?}", x);
            (Type::Unit, None)
        }
    }
}