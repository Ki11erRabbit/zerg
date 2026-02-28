use std::collections::HashMap;
use wasm_encoder::{BlockType, CodeSection, ExportKind, ExportSection, Function, FunctionSection, Ieee32, Ieee64, Instruction, Module, TypeSection, ValType};
use ast::desugared_tree;
use crate::compiler::function_state::{FunctionState, VariableLocation};

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

        self.compile_block(module, &mut function, body, false)?;
        function.instruction(&Instruction::End);

        self.state.clear();

        module.code.function(&function);

        Ok(())
    }


    fn compile_block(
        &mut self,
        module: &mut WasmModuleGroup,
        function: &mut Function,
        block: desugared_tree::Block,
        yield_value: bool,
    ) -> Result<(), CompileError> {
        let mut errors: Vec<CompileError> = Vec::new();
        let statements_len = block.statements.len();
        for (i, stmt) in block.statements.into_iter().enumerate() {
            let yield_value = if yield_value && i == statements_len - 1 {
                true
            } else {
                false
            };

            match self.compile_statement(module, function, stmt, yield_value) {
                Ok(_) => {},
                Err(error) => errors.push(error),
            }
        }
        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }
        Ok(())
    }

    fn compile_statement(
        &mut self,
        module: &mut WasmModuleGroup,
        function: &mut Function,
        stmt: desugared_tree::Statement,
        yield_value: bool,
    ) -> Result<(), CompileError> {
        match stmt {
            desugared_tree::Statement::Let { name, expr, .. } => {
                self.compile_expr(module, function, expr, true)?;
                let index = self.state.get(&name).unwrap();
                function.instruction(&Instruction::LocalSet(index));
            }
            desugared_tree::Statement::Expression { expr, .. } => {
                self.compile_expr(module, function, expr, yield_value)?;
            }
            desugared_tree::Statement::Assignment { target, expr, .. } => {
                match target {
                    desugared_tree::Expression::Variable { name, .. } => {
                        self.compile_expr(module, function, expr, true)?;
                        let index = self.state.get(&name).unwrap();
                        function.instruction(&Instruction::LocalGet(index));
                    }
                    _ => todo!("other assignment target"),
                }
            }
            desugared_tree::Statement::Comptime { .. } => {
                todo!("comptime statement")
            }
        }

        Ok(())
    }

    fn compile_expr(
        &mut self,
        module: &mut WasmModuleGroup,
        function: &mut Function,
        expr: desugared_tree::Expression,
        yield_value: bool,
    ) -> Result<(), CompileError> {
        match expr {
            desugared_tree::Expression::Variable { name, .. } => {
                if yield_value {
                    let index = self.state.get(&name).unwrap();
                    function.instruction(&Instruction::LocalGet(index));
                }
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                self.compile_expr(module, function, *expr, yield_value)?;
            }
            desugared_tree::Expression::ConstantNumber { value, r#type, .. } => {
                if !yield_value {
                    return Ok(())
                }
                let r#type = convert_type(&r#type).0;
                match r#type {
                    Type::U8 | Type::I8 | Type::U16 | Type::I16 | Type::I32 => {
                        let value = value.parse().unwrap();
                        function.instruction(&Instruction::I32Const(value));
                    }
                    Type::U32 => {
                        let value: u32 = value.parse().unwrap();
                        let value = i32::from_ne_bytes(value.to_ne_bytes());
                        function.instruction(&Instruction::I32Const(value));
                    }
                    Type::U64 => {
                        let value: u64 = value.parse().unwrap();
                        let value = i64::from_ne_bytes(value.to_ne_bytes());
                        function.instruction(&Instruction::I64Const(value));
                    }
                    Type::I64 => {
                        let value: i64 = value.parse().unwrap();
                        function.instruction(&Instruction::I64Const(value));
                    }
                    Type::F32 => {
                        let value: f32 = value.parse().unwrap();
                        function.instruction(&Instruction::F32Const(Ieee32::from(value)));
                    }
                    Type::F64 => {
                        let value: f64 = value.parse().unwrap();
                        function.instruction(&Instruction::F64Const(Ieee64::from(value)));
                    }
                    _ => unreachable!("Non numeric type in constant number")
                }
            }
            desugared_tree::Expression::Return { value, .. } => {
                if let Some(value) = value {
                    self.compile_expr(module, function, *value, true)?;
                    function.instruction(&Instruction::Return);
                } else {
                    function.instruction(&Instruction::Return);
                }
            }
            desugared_tree::Expression::IfExpression(if_expr) => {
                self.compile_if_expr(module, function, if_expr, yield_value)?;
            }
        }

        Ok(())
    }

    fn compile_if_expr(
        &mut self,
        module: &mut WasmModuleGroup,
        function: &mut Function,
        expr: desugared_tree::IfExpression,
        yield_value: bool,
    ) -> Result<(), CompileError> {
        let desugared_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            return_type,
            ..
        } = expr;

        self.compile_expr(module, function, *condition, true)?;

        let block_type = if let Some(r#type) = convert_type(&return_type).1 {
            BlockType::Result(r#type)
        } else {
            BlockType::Empty
        };

        function.instruction(&Instruction::If(block_type));
        self.compile_block(module, function, then_block, yield_value)?;
        for (condition, then_block) in elifs {
            function.instruction(&Instruction::Else);
            self.compile_expr(module, function, condition, true)?;
            function.instruction(&Instruction::If(block_type));
            self.compile_block(module, function, then_block, yield_value)?;
        }
        if let Some(else_block) = else_block {
            function.instruction(&Instruction::Else);
            self.compile_block(module, function, else_block, yield_value)?;
        }
        function.instruction(&Instruction::End);

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