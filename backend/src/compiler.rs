use std::collections::HashMap;
use std::path::PathBuf;
use wasm_encoder::{BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection, Ieee32, Ieee64, ImportSection, Instruction, MemorySection, MemoryType, Module, NameMap, NameSection, TypeSection, ValType};
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

#[derive(Clone)]
pub struct Definition {
    def_index: u32,
    type_index: u32,
    r#type: Type,
    value: DefinitionValue,
}

impl Definition {
    pub fn new(def_index: u32, type_index: u32, r#type: Type, value: DefinitionValue) -> Self {
        Definition { def_index, type_index, r#type, value }
    }
}

#[derive(Clone)]
pub enum DefinitionValue {
    Function(desugared_tree::Function),
}

pub struct ComptimeDefinition {
    pub function: desugared_tree::Function,
}

impl ComptimeDefinition {
    pub fn new(function: desugared_tree::Function) -> Self {
        ComptimeDefinition { function }
    }
}

pub struct ModuleDef {
    name: Vec<String>,
    definitions: HashMap<String, Definition>,
    comptime_definitions: HashMap<String, ComptimeDefinition>,
    /// u32 is the index into the current module's function table
    imports: HashMap<Vec<String>, u32>,
    /// Right now this is just used for extern functions
    re_exports: HashMap<Vec<String>, u32>,
    file: Option<desugared_tree::File>,
}

impl ModuleDef {
    fn new(file: desugared_tree::File, wasm_module: &mut WasmModuleGroup) -> Self {
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
            re_exports: HashMap::new(),
            file: None,
        };

        for r#extern in file.externals.iter() {
            for func in r#extern.functions.iter() {
                out.add_extern_function(&r#extern.library, wasm_module.next_function_index, func, wasm_module);
                wasm_module.next_function_index += 1;
            }
        }

        out.file = Some(file);
        out
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
        //module.imports.import(&last.join("."), end, EntityType::Function(type_index));
        let out = module.next_function_index;
        module.next_function_index += 1;
        self.imports.insert(path, out);

        out

    }

    fn add_function(&mut self, index: u32, function: &desugared_tree::Function, module: &mut WasmModuleGroup) {
        let desugared_tree::Function {
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
        let type_index = module.types.len();
        module.types.ty().function(parameters, return_type);

        module.functions.function(type_index);

        module.function_name_map.append(index, &path.to_vec_strings().join("::"));

        let r#type = Type::Function { parameters: type_parameters, return_type: Box::new(local) };
        
        let def = Definition::new(index, type_index, r#type, DefinitionValue::Function(function.clone()));
        self.definitions.insert(name, def);
    }

    fn add_extern_function(
        &mut self,
        library: &str,
        function_index: u32,
        function: &desugared_tree::Function,
        module: &mut WasmModuleGroup,
    ) {
        let desugared_tree::Function {
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
        let type_index = module.types.len();
        module.types.ty().function(parameters, return_type);
        module.imports.import(library, &name, EntityType::Function(type_index));

        let r#type = Type::Function { parameters: type_parameters, return_type: Box::new(local) };

        let def = Definition::new(function_index, type_index, r#type, DefinitionValue::Function(function.clone()));
        self.definitions.insert(name, def);

        self.re_exports.insert(path.to_vec_strings(), function_index);
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

pub(crate) struct WasmModuleGroup {
    module: Module,
    types: TypeSection,
    functions: FunctionSection,
    exports: ExportSection,
    imports: ImportSection,
    code: CodeSection,
    memory: MemorySection,
    names: NameSection,
    function_name_map: NameMap,
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
        let memory = MemorySection::new();
        let mut names = NameSection::new();
        names.module("main");
        let function_name_map = NameMap::new();
        WasmModuleGroup {
            module,
            types,
            functions,
            exports,
            imports,
            code,
            memory,
            names,
            function_name_map,
            next_function_index: 0,
        }
    }
}

pub struct Compiler {
    module_to_def: HashMap<Vec<String>, ModuleDef>,
    state: FunctionState,
    current_path: Vec<String>,
    main_module: Vec<String>,
    inlined_vars: Vec<HashMap<String, desugared_tree::Expression>>
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            module_to_def: HashMap::new(),
            state: FunctionState::new(),
            current_path: Vec::new(),
            main_module: Vec::new(),
            inlined_vars: Vec::new(),
        }
    }

    pub fn get_module_def(&self, path: &[String]) -> Option<&ModuleDef> {
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
    
    pub fn push_inlined_vars(&mut self) {
        self.inlined_vars.push(HashMap::new());
    }
    
    pub fn pop_inlined_vars(&mut self) {
        self.inlined_vars.pop();
    }
    
    pub fn set_inlined_var(&mut self, name: String, value: desugared_tree::Expression) {
        self.inlined_vars.last_mut().map(|last| last.insert(name, value));
    }
    
    pub fn remove_inlined_var(&mut self, name: &str) -> Option<desugared_tree::Expression> {
        for scope in self.inlined_vars.iter_mut().rev() {
            if let Some(last) = scope.remove(name) {
                return Some(last);
            }
        }
        None
    }

    fn load_files(&mut self, files: Vec<desugared_tree::File>, wasm_module: &mut WasmModuleGroup) {
        let mut modules = Vec::with_capacity(files.len());
        for file in files.clone() {
            let module_path = file.file_path.iter()
                .map(|s| s.to_string_lossy().to_string())
                .map(|s| if s.contains(".zerg") {
                    s.replace(".zerg", "")
                } else {
                    s
                })
                .collect::<Vec<String>>();
            let module_def= ModuleDef::new(file, wasm_module);
            modules.push((module_path, module_def));
        }

        for (file, (module_path, mut module)) in files.into_iter().zip(modules.into_iter()) {
            for (i, func) in file.functions.iter().enumerate() {
                module.add_function(wasm_module.next_function_index + i as u32, func, wasm_module);
            }
            wasm_module.next_function_index += file.functions.len() as u32;

            self.module_to_def.insert(module_path, module);
        }
    }

    pub fn compile_files(&mut self, files: Vec<desugared_tree::File>) -> Result<(), CompileError> {
        let mut module = WasmModuleGroup::new();
        self.load_files(files.clone(), &mut module);

        let mut errors: Vec<CompileError> = Vec::new();

        for file in files.into_iter() {
            match self.compile_file(&mut module, file) {
                Ok(_) => {},
                Err(error) => errors.push(error),
            }
        }
        if !errors.is_empty() {
            return Err(CompileError::Many(errors));
        }

        let path_buf = PathBuf::from("output.wasm");
        if path_buf.as_path().exists() {
            std::fs::remove_file(&path_buf).unwrap();
        }
        module.memory.memory(MemoryType { minimum: 1, maximum: Some(16), memory64: false, shared: false, page_size_log2: None });
        module.exports.export("memory", ExportKind::Memory, 0);
        module.names.functions(&module.function_name_map);

        module.module.section(&module.types);
        module.module.section(&module.imports);
        module.module.section(&module.functions);
        module.module.section(&module.memory);
        module.module.section(&module.exports);
        module.module.section(&module.code);
        module.module.section(&module.names);
        let bytes = module.module.finish();
        std::fs::write(path_buf.as_path(), bytes).unwrap();

        Ok(())
    }

    fn compile_file(&mut self, module: &mut WasmModuleGroup, file: desugared_tree::File) -> Result<(), CompileError> {
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
            // TODO: change this so that it generates the _start function that then calls main
            module.exports.export("_start", ExportKind::Func, main.def_index);
            self.main_module = self.current_path.clone();
        }

        Ok(())
    }

    fn compile_function(&mut self, module: &mut WasmModuleGroup, function: desugared_tree::Function) -> Result<(), CompileError> {
        let desugared_tree::Function {
            comptime,
            arguments,
            body,
            ..
        } = function;
        if comptime {
            return Ok(());
        }
        for (i, arg) in arguments.iter().enumerate() {
            let ty = convert_type(&arg.r#type).0;
            self.state.store_function_argument(&arg.name, ty, i as u32);
        }
        self.bind_block(&body, false);
        let locals = self.state.local_definitions();
        let mut function = Function::new(locals);

        self.compile_block(module, &mut function, body, false, false)?;
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
        inlining: bool,
    ) -> Result<(), CompileError> {
        let mut errors: Vec<CompileError> = Vec::new();
        let statements_len = block.statements.len();
        self.state.push();
        for (i, stmt) in block.statements.into_iter().enumerate() {
            let yield_value = if yield_value && i == statements_len - 1 {
                true
            } else {
                false
            };

            match self.compile_statement(module, function, stmt, yield_value, inlining) {
                Ok(_) => {},
                Err(error) => errors.push(error),
            }
        }
        self.state.pop();
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
        inlining: bool,
    ) -> Result<(), CompileError> {
        match stmt {
            desugared_tree::Statement::Let { name, expr, .. } => {
                self.compile_expr(module, function, expr, true, inlining)?;
                let index = self.state.get(&name).unwrap();
                function.instruction(&Instruction::LocalSet(index));
            }
            desugared_tree::Statement::Expression { expr, .. } => {
                self.compile_expr(module, function, expr, yield_value, inlining)?;
            }
            desugared_tree::Statement::Assignment { target, expr, .. } => {
                match target {
                    desugared_tree::Expression::Variable { name, .. } => {
                        let name = if inlining {
                            format!("inline<{}>", name)
                        } else {
                            name.to_string()
                        };
                        self.compile_expr(module, function, expr, true, inlining)?;
                        let index = self.state.get(&name).unwrap();
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    _ => todo!("other assignment target"),
                }
            }
            desugared_tree::Statement::Comptime { block, .. } => {
                let mut interpreter = Interpreter::new(inlining);
                interpreter.interpret_comptime_block(self, module, function, block)?;
            }
        }

        Ok(())
    }

    pub(crate) fn compile_expr(
        &mut self,
        module: &mut WasmModuleGroup,
        function: &mut Function,
        expr: desugared_tree::Expression,
        yield_value: bool,
        inlining: bool,
    ) -> Result<(), CompileError> {
        match expr {
            desugared_tree::Expression::Variable { name, .. } => {
                if yield_value && !inlining {
                    let index = self.state.get(&name).unwrap();
                    function.instruction(&Instruction::LocalGet(index));
                }
                if yield_value && inlining {
                    let inlined_name = format!("inline<{name}>");
                    if let Some(index) = self.state.get(&inlined_name) {
                        function.instruction(&Instruction::LocalGet(index));
                    } else {
                        let Some(expr) = self.remove_inlined_var(&inlined_name) else {
                            return Ok(())
                        };
                        self.compile_expr(module, function, expr, yield_value, false)?;
                    }
                }
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                self.compile_expr(module, function, *expr, yield_value, inlining)?;
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
            desugared_tree::Expression::ConstantBool { value, .. } => {
                if yield_value {
                    let value = if value { 1 } else { 0 };
                    function.instruction(&Instruction::I32Const(value));
                }
            }
            desugared_tree::Expression::Return { value, .. } => {
                if let Some(value) = value {
                    self.compile_expr(module, function, *value, true, inlining)?;
                    if !inlining {
                        function.instruction(&Instruction::Return);
                    }
                } else {
                    if !inlining {
                        function.instruction(&Instruction::Return);
                    }
                }
            }
            desugared_tree::Expression::IfExpression(if_expr) => {
                self.compile_if_expr(module, function, if_expr, yield_value, inlining)?;
            }
            desugared_tree::Expression::FunctionCall { name, args, return_type, .. } => {
                let path = name.to_vec_strings();
                let mut module_path = name.to_vec_strings();
                let name = module_path.pop().unwrap();

                {
                    let def = {
                        let module_def = self.module_to_def.get(&module_path).unwrap();
                        let def = module_def.get_definition(&name).unwrap();
                        def
                    };

                    match def.value.clone() {
                        DefinitionValue::Function(fun) => {
                            if fun.inline {
                                self.push_inlined_vars();
                                for (arg, expr) in fun.arguments.iter().zip(args.into_iter()) {
                                    self.set_inlined_var(format!("inline<{}>", arg.name), expr);
                                }
                                let body = fun.body.clone();

                                let result = self.compile_block(module, function, body, yield_value, true);
                                self.pop_inlined_vars();
                                return result;
                            }
                        }
                    }
                }

                for arg in args {
                    self.compile_expr(module, function, arg, true, false)?;
                }

                let module_def = self.module_to_def.get(&module_path).unwrap();
                let def = module_def.get_definition(&name).unwrap();


                let index = if let Some(index) = module_def.re_exports.get(&path) {
                    *index
                } else {
                    def.def_index
                };
                function.instruction(&Instruction::Call(index));
                // Check if the function will yield a value and if it does but we don't yield it, then drop it
                // If unit and we yield, just load a i32.
                match return_type {
                    desugared_tree::Type::Unit(_) if yield_value => {
                        function.instruction(&Instruction::I32Const(0));
                    }
                    desugared_tree::Type::Unit(_) if !yield_value => {}
                    _ if !yield_value => {
                        function.instruction(&Instruction::Drop);
                    }
                    _ => {}
                }
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
        inlining: bool,
    ) -> Result<(), CompileError> {
        let desugared_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            return_type,
            ..
        } = expr;

        self.compile_expr(module, function, *condition, true, inlining)?;

        let block_type = if let Some(r#type) = convert_type(&return_type).1 {
            BlockType::Result(r#type)
        } else {
            BlockType::Empty
        };

        function.instruction(&Instruction::If(block_type));
        self.compile_block(module, function, then_block, yield_value, inlining)?;
        let elif_count = elifs.len();
        for (condition, then_block) in elifs {
            function.instruction(&Instruction::Else);
            self.compile_expr(module, function, condition, true, inlining)?;
            function.instruction(&Instruction::If(block_type));
            self.compile_block(module, function, then_block, yield_value, inlining)?;

        }
        if let Some(else_block) = else_block {
            function.instruction(&Instruction::Else);
            self.compile_block(module, function, else_block, yield_value, inlining)?;
        }
        for _ in 0..elif_count {
            function.instruction(&Instruction::End);
        }
        function.instruction(&Instruction::End);

        Ok(())
    }
}


impl Compiler {
    fn bind_block(&mut self, block: &desugared_tree::Block, inline: bool) {
        self.state.push();
        for stmt in block.statements.iter() {
            match stmt {
                desugared_tree::Statement::Let { name, r#type, expr, .. } => {
                    let ty = convert_type(&r#type).0;
                    if inline {
                        self.state.store(&format!("inline<{name}>"), ty);
                    } else {
                        self.state.store(&name, ty);
                    }

                    self.bind_expr(expr, inline);
                }
                desugared_tree::Statement::Expression { expr, ..} => {
                    self.bind_expr(expr, inline);
                }
                desugared_tree::Statement::Assignment { expr, .. } => {
                    self.bind_expr(expr, inline);
                }
                _ => {}
            }
        }
        self.state.pop();
    }

    fn bind_expr(&mut self, expr: &desugared_tree::Expression, inline: bool) {
        match expr {
            desugared_tree::Expression::IfExpression(if_expr) => {
                self.bind_if_expr(if_expr, inline);
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                self.bind_expr(expr, true);
            }
            desugared_tree::Expression::FunctionCall { name, args, .. } => {
                for arg in args {
                    self.bind_expr(arg, true);
                }
                let path = name.to_vec_strings();
                let mut module_path = name.to_vec_strings();
                let name = module_path.pop().unwrap();
                let def = {
                    let module_def = self.module_to_def.get(&module_path).unwrap();
                    let def = module_def.get_definition(&name).unwrap();
                    def
                };

                match def.value.clone() {
                    DefinitionValue::Function(function) => {
                        if function.inline {
                            self.bind_block(&function.body, true)
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn bind_if_expr(&mut self, if_expr: &desugared_tree::IfExpression, inline: bool) {
        let desugared_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            ..
        } = if_expr;

        self.bind_expr(condition.as_ref(), inline);
        self.bind_block(then_block, inline);
        for (condition, block) in elifs {
            self.bind_expr(condition, inline);
            self.bind_block(block, inline);
        }
        else_block.as_ref().map(|else_block| {
            self.bind_block(else_block, inline);
        });
    }
}



pub fn convert_type(r#type: &desugared_tree::Type) -> (Type, Option<ValType>) {
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