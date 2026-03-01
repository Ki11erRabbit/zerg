use std::collections::HashMap;
use std::path::PathBuf;
use wasm_encoder::{BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, Ieee32, Ieee64, ImportSection, Instruction, Module, TypeSection, ValType};
use ast::{desugared_tree};
use crate::compiler::function_state::{FunctionState, VariableLocation};
use crate::interpreter::{Interpreter, InterpreterError};

pub mod function_state;

#[derive(Debug)]
pub enum CompileError {
    Many(Vec<CompileError>),
    Interpreter(InterpreterError),
}

impl From<InterpreterError> for CompileError {
    fn from(e: InterpreterError) -> Self {
        CompileError::Interpreter(e)
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

impl Type {
    pub fn name(&self) -> String {
        match self {
            Type::Bool => "bool".into(),
            Type::U8 => "u8".into(),
            Type::I8 => "i8".into(),
            Type::U16 => "u16".into(),
            Type::I16 => "i16".into(),
            Type::U32 => "u32".into(),
            Type::I32 => "i32".into(),
            Type::U64 => "u64".into(),
            Type::I64 => "i64".into(),
            Type::F32 => "f32".into(),
            Type::F64 => "f64".into(),
            Type::Unit => "unit".into(),
            Type::Function { parameters, return_type } => {
                let mut out = String::from("(");
                for (i, param) in parameters.iter().enumerate() {
                    out.push_str(&param.name());
                    if i < parameters.len() - 1 {
                        out.push_str(", ");
                    }
                }
                out.push_str(") -> ");
                out.push_str(&return_type.name());
                out
            }
        }
    }
}

pub struct Definition {
    type_index: u32,
    r#type: Type,
}

impl Definition {
    pub fn new(type_index: u32, r#type: Type) -> Self {
        Definition { type_index, r#type }
    }
}

pub struct ComptimeDefinition<'input> {
    pub function: desugared_tree::Function<'input>,
}

impl<'input> ComptimeDefinition<'input> {
    pub fn new(function: desugared_tree::Function<'input>) -> Self {
        ComptimeDefinition { function }
    }
}

pub struct ModuleDef<'input> {
    name: Vec<String>,
    definitions: HashMap<String, Definition>,
    comptime_definitions: HashMap<String, ComptimeDefinition<'input>>,
    imports: HashMap<Vec<String>, u32>,
    file: Option<desugared_tree::File<'input>>,
}

impl<'input> ModuleDef<'input> {
    fn new(file: desugared_tree::File<'input>) -> (Self, WasmModuleGroup) {
        let mut wasm_module = WasmModuleGroup::new();

        let name = file.file_path.iter()
            .map(|s| s.to_string_lossy().to_string())
            .map(|s| if s.contains(".zerg") {
                s.replace(".zerg", "")
            } else {
                s
            })
            .collect::<Vec<String>>();
        
        let mut out = Self {
            name,
            definitions: HashMap::new(),
            comptime_definitions: HashMap::new(),
            imports: HashMap::new(),
            file: None,
        };

        for func in file.functions.iter() {
            out.load_block_imports(&mut wasm_module, &func.body)
        }

        for (i, func) in file.functions.iter().enumerate() {
            out.add_function(wasm_module.next_function_index + i as u32, func, &mut wasm_module);
        }
        out.file = Some(file);
        (out, wasm_module)
    }

    pub fn get_definition(&self, name: &str) -> Option<&Definition> {
        self.definitions.get(name)
    }

    pub fn get_comptime_definition(&self, name: &str) -> Option<&ComptimeDefinition> {
        self.comptime_definitions.get(name)
    }

    fn import_function(&mut self, module: &mut WasmModuleGroup, path: Vec<String>, args: Vec<ValType>, ret: Vec<ValType>) -> u32 {
        let type_index = module.types.len();
        module.types.ty().function(args, ret);
        let Some((end, last)) = path.split_last() else {
            panic!("Import path must contain 2 elements");
        };
        module.imports.import(&last.join("."), end, EntityType::Function(type_index));
        let out = module.next_function_index;
        module.next_function_index += 1;
        self.imports.insert(path, out);

        out
    }

    fn add_function(&mut self, index: u32, function: &desugared_tree::Function<'input>, module: &mut WasmModuleGroup) {
        let desugared_tree::Function {
            public,
            comptime,
            arguments,
            return_type,
            path,
            ..
        } = function;
        let name = path.last().as_ref().unwrap().segment.clone();
        if *comptime {
            self.comptime_definitions.insert(name, ComptimeDefinition::new(function.clone()));
            return
        }
        let mut parameters = Vec::new();
        let mut type_parameters = Vec::new();
        for param in arguments.iter() {
            let (local, ty) = convert_type(&param.r#type);
            type_parameters.push(local);
            if let Some(ty) = ty {
                parameters.push(ty);
            }
        }
        let (local, return_type) = convert_type(&return_type);
        let return_type = if let Some(ty) = return_type {
            vec![ty]
        } else {
            Vec::new()
        };
        module.types.ty().function(parameters, return_type);

        module.functions.function(index);
        if *public {
            module.exports.export(&name, ExportKind::Func, index);
        }

        let r#type = Type::Function { parameters: type_parameters, return_type: Box::new(local) };
        
        let def = Definition::new(index, r#type);
        self.definitions.insert(name, def);
    }

    fn load_block_imports(&mut self, module: &mut WasmModuleGroup, block: &desugared_tree::Block) {
        for stmt in block.statements.iter() {
            match stmt {
                desugared_tree::Statement::Let { expr, .. } => {
                    self.load_expr_imports(module, expr);
                }
                desugared_tree::Statement::Expression { expr, .. } => {
                    self.load_expr_imports(module, expr);
                }
                desugared_tree::Statement::Assignment { target, expr, .. } => {
                    self.load_expr_imports(module, target);
                    self.load_expr_imports(module, expr);
                }
                _ => {}
            }
        }
    }

    fn load_expr_imports(&mut self, module: &mut WasmModuleGroup, expr: &desugared_tree::Expression) {
        match expr {
            desugared_tree::Expression::FunctionCall { name, args, function_type, .. } => {
                for arg in args {
                    self.load_expr_imports(module, arg);
                }

                let mut module_path = name.to_vec_strings();
                let _ = module_path.pop().unwrap();
                if module_path == self.name {
                    return
                }
                let r#type = convert_type(function_type).0;
                let (args, ret) = create_function_type(r#type);
                self.import_function(module, name.to_vec_strings(), args, ret);
            }
            desugared_tree::Expression::Return { value, .. } => {
                if let Some(value) = value {
                    self.load_expr_imports(module, value);
                }
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                self.load_expr_imports(module, expr);
            }
            desugared_tree::Expression::IfExpression(if_expr) => {
                self.load_if_expr_imports(module, if_expr);
            }
            _ => {}
        }
    }

    fn load_if_expr_imports(&mut self, module: &mut WasmModuleGroup, if_expr: &desugared_tree::IfExpression) {
        let desugared_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            ..
        } = if_expr;
        self.load_expr_imports(module, condition);
        self.load_block_imports(module, then_block);
        for (condition, then_block) in elifs {
            self.load_expr_imports(module, condition);
            self.load_block_imports(module, then_block);
        }
        if let Some(else_block) = else_block {
            self.load_block_imports(module, else_block);
        }
    }
}

struct WasmModuleGroup {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    imports: ImportSection,
    code: CodeSection,
    next_function_index: u32,
}

impl WasmModuleGroup {
    pub fn new() -> Self {
        let module = Module::new();
        let types = TypeSection::new();
        let functions = FunctionSection::new();
        let exports = ExportSection::new();
        let imports = ImportSection::new();
        let code = CodeSection::new();
        WasmModuleGroup {
            module,
            types,
            functions,
            exports,
            imports,
            code,
            next_function_index: 0,
        }
    }
}

pub struct Compiler<'input> {
    module_to_def: HashMap<Vec<String>, ModuleDef<'input>>,
    state: FunctionState,
    current_path: Vec<String>,
}

impl<'input> Compiler<'input> {
    pub fn new() -> Self {
        Self {
            module_to_def: HashMap::new(),
            state: FunctionState::new(),
            current_path: Vec::new(),
        }
    }

    pub fn get_module_def(&self, path: &[String]) -> Option<&ModuleDef<'input>> {
        self.module_to_def.get(path)
    }

    /// The location will be offset to the right spot in the variable ordering
    pub fn get_variables(&self) -> Vec<(String, VariableLocation)> {
        let mut out = Vec::new();
        for scope in self.state.iter() {
            for (name, location) in scope.iter() {
                let mut location = location.clone();
                location.update_index(self.state.function_arguments, self.state.i32_var_count, self.state.i64_var_count, self.state.f32_var_count);
                out.push((name.clone(), location));
            }
        }
        out
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

        let mut file_names = Vec::with_capacity(wasm_modules.len());
        for (file, module) in files.into_iter().zip(wasm_modules.iter_mut()) {
            let file_path = file.file_path.iter()
                .map(|s| s.to_string_lossy().to_string())
                .map(|s| s.replace(".zerg", ".wasm"))
                .collect::<Vec<String>>();

            match self.compile_file(module, file) {
                Ok(_) => {
                    file_names.push(file_path);
                },
                Err(error) => errors.push(error),
            }
        }
        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }

        if PathBuf::from("output").exists() {
            std::fs::remove_dir_all("output").unwrap();
            std::fs::create_dir_all("output").unwrap();
        } else {
            std::fs::create_dir_all("output").unwrap();
        }

        assert_eq!(file_names.len(), wasm_modules.len());

        for (mut module, file_name) in wasm_modules.into_iter().zip(file_names.into_iter()) {
            let mut path_buf = PathBuf::from("output");
            path_buf.push(file_name.join("."));
            if path_buf.as_path().exists() {
                std::fs::remove_dir_all(path_buf.as_path()).unwrap();
            }
            module.module.section(&module.types);
            module.module.section(&module.imports);
            module.module.section(&module.functions);
            module.module.section(&module.exports);
            module.module.section(&module.code);

            std::fs::write(path_buf.as_path(), module.module.finish()).unwrap();
        }

        Ok(())
    }

    fn compile_file(&mut self, module: &mut WasmModuleGroup, file: desugared_tree::File<'input>) -> Result<(), CompileError> {
        let desugared_tree::File { functions, file_path, .. } = file;
        let module_path = file_path.iter()
            .map(|s| s.to_string_lossy().to_string())
            .map(|s| if s.contains(".zerg") {
                s.replace(".zerg", "")
            } else {
                s
            })
            .collect::<Vec<String>>();
        self.current_path = module_path;
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

        let def = self.module_to_def.get(&self.current_path).unwrap();
        // if there is a main function, export it as start.
        if let Some(main) = def.get_definition("main") {
            module.exports.export("_start", ExportKind::Func, main.type_index);
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
            desugared_tree::Statement::Comptime { block, .. } => {
                let mut interpreter = Interpreter::new();
                interpreter.interpret_comptime_block(self, function, block)?;
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
            desugared_tree::Expression::FunctionCall { name, args, .. } => {
                // TODO: check if function being called is inline, and evaluate body of function instead

                for arg in args {
                    self.compile_expr(module, function, arg, true)?;
                }

                let mut module_path = name.to_vec_strings();
                let name = module_path.pop().unwrap();

                let module_def = self.module_to_def.get(&module_path).unwrap();
                let def = module_def.get_definition(&name).unwrap();

                let index = if module_path == self.current_path {
                    def.type_index
                } else {
                    let module_def = self.module_to_def.get(&self.current_path).unwrap();
                    let mut path = module_path;
                    path.push(name);
                    let def_index = module_def.imports.get(&path).unwrap();

                    *def_index
                };
                function.instruction(&Instruction::Call(index));
                // TODO: check if function can yield a value. If it does and we don't yield, pop it
            }
            desugared_tree::Expression::ConstantString { .. } => {
                todo!("string literal")
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

fn create_function_type(r#type: Type) -> (Vec<ValType>, Vec<ValType>) {
    match r#type {
        Type::Function { parameters, return_type } => {
            let parameters = parameters.into_iter()
                .map(translate_type)
                .collect();
            let return_type = match *return_type {
                Type::Unit => return (parameters, vec![]),
                x => translate_type(x),
            };
            (parameters, vec![return_type])
        }
        _ => panic!("Passed in a non-function type"),
    }
}

fn translate_type(r#type: Type) -> ValType {
    match r#type {
        Type::Bool | Type::U8 | Type::I8 | Type::U16 | Type::I16 | Type::U32 | Type::I32 => ValType::I32,
        Type::U64 | Type::I64 => ValType::I64,
        Type::F32 => ValType::F32,
        Type::F64 => ValType::F64,
        Type::Unit => ValType::I32,
        Type::Function { .. } => ValType::I64,

    }
}