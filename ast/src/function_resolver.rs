use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use crate::{desugared_tree, parse_tree, OwnedPath, Span};

pub type ResolverResult<T> = Result<T, ResolverError>;

#[derive(Debug)]
pub enum ResolverError {
    Many(Vec<ResolverError>),
    UnknownFunction(String, PathBuf, Span),
    CannotInferType(PathBuf, Span),
}

impl std::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolverError::Many(errors) => {
                for error in errors {
                    writeln!(f, "{}", error)?;
                }
                Ok(())
            }
            ResolverError::UnknownFunction(name, path, span) => {
                writeln!(f, "Unknown function {} in {:?}:{}", name, path, span)
            }
            ResolverError::CannotInferType(path, span) => {
                writeln!(f, "Cannot infer type in {:?}:{}", path, span)
            }
        }
    }
}

impl std::error::Error for ResolverError {}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub argument_types: Vec<TypeInfo>,
    pub return_type: TypeInfo,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeInfo {
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
    Bool,
    String,
    Unit,
    Generic {
        name: String,
        args: Vec<TypeInfo>,
    },
    Custom {
        name: String,
    },
    Function {
        args: Vec<TypeInfo>,
        return_type: Box<TypeInfo>,
    }
}

pub struct FileDefinitions {
    /// The full file path including the name of the function
    exports: HashSet<Vec<String>>,
    /// The full file path including the name of the function
    internals: HashSet<Vec<String>>,
}

impl FileDefinitions {
    pub fn new() -> Self {
        Self {
            exports: HashSet::new(),
            internals: HashSet::new(),
        }
    }

    pub fn insert_export(&mut self, path: Vec<String>) {
        self.exports.insert(path);
    }

    pub fn insert_internal(&mut self, path: Vec<String>) {
        self.internals.insert(path);
    }

    pub fn lookup_export(&self, path: &[String]) -> bool {
        self.exports.contains(path)
    }

    pub fn lookup_internal(&self, path: &[String]) -> bool {
        self.internals.contains(path)
    }
}

pub struct FunctionResolver {
    /// The path includes just the file
    files: HashMap<Vec<String>, FileDefinitions>,
    /// The path includes just the files that are currently in scope
    current_paths: HashSet<Vec<String>>,
    /// The path includes just the files that are currently in scope
    current_comptime_paths: HashSet<Vec<String>>,
    current_module: Vec<String>,
    /// Maps function paths to their signatures (argument types and return type)
    function_signatures: HashMap<Vec<String>, FunctionSignature>,
    /// Stack of variable scopes - each scope maps variable names to their types
    variable_scopes: Vec<HashMap<String, TypeInfo>>,
}

impl FunctionResolver {
        /// Match argument types to function parameters, collecting generic substitutions
        fn match_generics(
            param_types: &[TypeInfo],
            arg_types: &[TypeInfo],
        ) -> Option<HashMap<String, TypeInfo>> {
            let mut generics = HashMap::new();
            if param_types.len() != arg_types.len() {
                return None;
            }
            for (param, arg) in param_types.iter().zip(arg_types.iter()) {
                match param {
                    TypeInfo::Custom { name } => {
                        generics.insert(name.clone(), arg.clone());
                    }
                    // For nested generics, could recurse here
                    _ => {
                        if param != arg {
                            return None;
                        }
                    }
                }
            }
            Some(generics)
        }

        /// Substitute generics in a type using the collected substitutions
        fn substitute_generics(ty: &TypeInfo, generics: &HashMap<String, TypeInfo>) -> TypeInfo {
            match ty {
                TypeInfo::Custom { name } => {
                    if let Some(subst) = generics.get(name) {
                        subst.clone()
                    } else {
                        TypeInfo::Custom { name: name.clone() }
                    }
                }
                TypeInfo::Generic { name, args } => {
                    TypeInfo::Generic {
                        name: name.clone(),
                        args: args.iter().map(|a| Self::substitute_generics(a, generics)).collect(),
                    }
                }
                TypeInfo::Function { args, return_type } => {
                    TypeInfo::Function {
                        args: args.iter().map(|a| Self::substitute_generics(a, generics)).collect(),
                        return_type: Box::new(Self::substitute_generics(return_type, generics)),
                    }
                }
                _ => ty.clone(),
            }
        }
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            current_paths: HashSet::new(),
            current_comptime_paths: HashSet::new(),
            current_module: Vec::new(),
            function_signatures: HashMap::new(),
            variable_scopes: vec![HashMap::new()], // Start with one global scope
        }
    }

    fn add_current_path(&mut self, path: Vec<String>) {
        self.current_paths.insert(path);
    }

    fn add_current_comptime_path(&mut self, path: Vec<String>) {
        self.current_comptime_paths.insert(path);
    }

    fn set_current_module(&mut self, module: Vec<String>) {
        self.current_module = module;
    }

    fn clear_paths(&mut self) {
        self.current_paths.clear();
        self.current_comptime_paths.clear();
        self.current_module.clear();
    }

    /// Push a new variable scope onto the stack
    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    /// Pop the current variable scope from the stack
    fn pop_scope(&mut self) {
        // Keep at least one scope (global scope)
        if self.variable_scopes.len() > 1 {
            self.variable_scopes.pop();
        }
    }

    /// Add a variable to the current scope
    fn add_variable(&mut self, name: String, type_info: TypeInfo) {
        if let Some(current_scope) = self.variable_scopes.last_mut() {
            current_scope.insert(name, type_info);
        }
    }

    /// Look up a variable in the current scope chain (searching from innermost to outermost)
    fn lookup_variable(&self, name: &str) -> Option<TypeInfo> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(type_info) = scope.get(name) {
                return Some(type_info.clone());
            }
        }
        None
    }

    /// Get the type of the last expression in a block (the block's yield type)
    fn get_block_yield_type(block: &desugared_tree::Block) -> Option<TypeInfo> {
        // Find the last expression statement
        for stmt in block.statements.iter().rev() {
            if let desugared_tree::Statement::Expression { expr, .. } = stmt {
                return Self::get_expression_type(expr);
            }
        }
        // If no expression statement, block yields unit
        Some(TypeInfo::Unit)
    }

    /// Resolve a function by name and argument types (for operator overloading)
    fn resolve_function_with_types(&self, name: &str, arg_types: &[TypeInfo]) -> Option<Vec<String>> {
        let paths = self.generate_paths(name);

        for base_path in &paths {
            // Try to find an exact match with type signature
            let mut typed_path = base_path.clone();
            for arg_type in arg_types {
                typed_path.push(format!("{:?}", arg_type).to_lowercase());
            }
            
            if let Some(def) = self.files.get(&typed_path[..(typed_path.len() - arg_types.len() - 1)]) {
                if def.lookup_export(&typed_path) {
                    return Some(typed_path);
                }
            }

            // Fall back to untyped path
            if let Some(def) = self.files.get(&base_path[..(base_path.len() - 1)]) {
                if def.lookup_export(base_path) {
                    return Some(base_path.clone());
                }
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(name.to_string());
        
        // Try typed version
        let mut typed_local_path = local_path.clone();
        for arg_type in arg_types {
            typed_local_path.push(format!("{:?}", arg_type).to_lowercase());
        }
        
        if let Some(def) = self.files.get(&typed_local_path[..(typed_local_path.len() - arg_types.len() - 1)]) {
            if def.lookup_internal(&typed_local_path) {
                return Some(typed_local_path);
            }
        }
        
        if let Some(def) = self.files.get(&local_path[..(local_path.len() - 1)]) {
            if def.lookup_internal(&local_path) {
                return Some(local_path);
            }
        }

        None
    }

    /// Infer the type of a numeric constant
    fn infer_number_type(value: &str) -> TypeInfo {
        // For now, default to i32. In a real implementation, you might analyze the value
        // or use context to determine the correct type
        if value.contains('.') {
            TypeInfo::F64
        } else {
            TypeInfo::I32
        }
    }

    /// Get the type of an expression after resolution
    fn get_expression_type(expr: &desugared_tree::Expression) -> Option<TypeInfo> {
        match expr {
            desugared_tree::Expression::Variable { r#type, .. } => {
                Self::desugared_type_to_typeinfo(r#type)
            }
            desugared_tree::Expression::ConstantNumber { r#type, .. } => {
                Self::desugared_type_to_typeinfo(r#type)
            }
            desugared_tree::Expression::ConstantString { .. } => {
                Some(TypeInfo::String)
            }
            desugared_tree::Expression::FunctionCall { return_type, .. } => {
                Self::desugared_type_to_typeinfo(return_type)
            }
            desugared_tree::Expression::Parenthesized { expr, .. } => {
                Self::get_expression_type(expr)
            }
            desugared_tree::Expression::Return { .. } => {
                Some(TypeInfo::Unit)
            }
            desugared_tree::Expression::IfExpression(if_expr) => {
                Self::desugared_type_to_typeinfo(&if_expr.return_type)
            }
        }
    }

    /// Convert a desugared Type to TypeInfo
    fn desugared_type_to_typeinfo(ty: &desugared_tree::Type) -> Option<TypeInfo> {
        match ty {
            desugared_tree::Type::U8(_) => Some(TypeInfo::U8),
            desugared_tree::Type::I8(_) => Some(TypeInfo::I8),
            desugared_tree::Type::U16(_) => Some(TypeInfo::U16),
            desugared_tree::Type::I16(_) => Some(TypeInfo::I16),
            desugared_tree::Type::U32(_) => Some(TypeInfo::U32),
            desugared_tree::Type::I32(_) => Some(TypeInfo::I32),
            desugared_tree::Type::U64(_) => Some(TypeInfo::U64),
            desugared_tree::Type::I64(_) => Some(TypeInfo::I64),
            desugared_tree::Type::F32(_) => Some(TypeInfo::F32),
            desugared_tree::Type::F64(_) => Some(TypeInfo::F64),
            desugared_tree::Type::Bool(_) => Some(TypeInfo::Bool),
            desugared_tree::Type::String(_) => Some(TypeInfo::String),
            desugared_tree::Type::Unit(_) => Some(TypeInfo::Unit),
            desugared_tree::Type::Custom { name, .. } => {
                Some(TypeInfo::Custom { name: name.to_string() })
            }
            desugared_tree::Type::Generic { name, args, .. } => {
                let arg_types = args.iter()
                    .filter_map(|arg| Self::desugared_type_to_typeinfo(arg))
                    .collect();
                Some(TypeInfo::Generic { name: name.to_string(), args: arg_types })
            }
            desugared_tree::Type::Function { args, r#return, .. } => {
                let args = args.iter().filter_map(|arg| Self::desugared_type_to_typeinfo(arg)).collect();
                let return_type = Self::desugared_type_to_typeinfo(r#return.as_ref()).unwrap();
                Some(TypeInfo::Function { args, return_type: Box::new(return_type) })
            }
        }
    }

    fn typeinfo_to_desugared_type(info: &TypeInfo, span: Span) -> desugared_tree::Type<'static> {
        use std::borrow::Cow;
        
        match info {
            TypeInfo::U8 => desugared_tree::Type::U8(span),
            TypeInfo::I8 => desugared_tree::Type::I8(span),
            TypeInfo::U16 => desugared_tree::Type::U16(span),
            TypeInfo::I16 => desugared_tree::Type::I16(span),
            TypeInfo::U32 => desugared_tree::Type::U32(span),
            TypeInfo::I32 => desugared_tree::Type::I32(span),
            TypeInfo::U64 => desugared_tree::Type::U64(span),
            TypeInfo::I64 => desugared_tree::Type::I64(span),
            TypeInfo::F32 => desugared_tree::Type::F32(span),
            TypeInfo::F64 => desugared_tree::Type::F64(span),
            TypeInfo::Bool => desugared_tree::Type::Bool(span),
            TypeInfo::String => desugared_tree::Type::String(span),
            TypeInfo::Unit => desugared_tree::Type::Unit(span),
            TypeInfo::Custom { name } => desugared_tree::Type::Custom {
                name: Cow::Owned(name.clone()),
                span,
            },
            TypeInfo::Generic { name, args } => {
                let desugared_args = args.iter()
                    .map(|arg| Self::typeinfo_to_desugared_type(arg, span))
                    .collect();
                desugared_tree::Type::Generic {
                    name: Cow::Owned(name.clone()),
                    args: desugared_args,
                    span,
                }
            }
            TypeInfo::Function { args, return_type } => {
                let desugared_args = args.iter()
                    .map(|arg| Self::typeinfo_to_desugared_type(arg, span))
                    .collect();
                let desugared_return = Self::typeinfo_to_desugared_type(return_type, span);
                desugared_tree::Type::Function {
                    args: desugared_args,
                    r#return: Box::new(desugared_return),
                    span,
                }
            }
        }
    }

    fn signature_to_desugared_type(sig: &FunctionSignature, span: Span) -> desugared_tree::Type<'static> {
        let args = sig.argument_types.iter()
            .map(|arg| Self::typeinfo_to_desugared_type(arg, span))
            .collect();
        let return_type = Self::typeinfo_to_desugared_type(&sig.return_type, span);
        desugared_tree::Type::Function {
            args,
            r#return: Box::new(return_type),
            span,
        }
    }

    fn parse_tree_type_to_typeinfo(r#type: &parse_tree::Type) -> TypeInfo {
        match r#type {
            parse_tree::Type::U8(_) => TypeInfo::U8,
            parse_tree::Type::I8(_) => TypeInfo::I8,
            parse_tree::Type::U16(_) => TypeInfo::U16,
            parse_tree::Type::I16(_) => TypeInfo::I16,
            parse_tree::Type::U32(_) => TypeInfo::U32,
            parse_tree::Type::I32(_) => TypeInfo::I32,
            parse_tree::Type::U64(_) => TypeInfo::U64,
            parse_tree::Type::I64(_) => TypeInfo::I64,
            parse_tree::Type::F32(_) => TypeInfo::F32,
            parse_tree::Type::F64(_) => TypeInfo::F64,
            parse_tree::Type::Bool(_) => TypeInfo::Bool,
            parse_tree::Type::String(_) => TypeInfo::String,
            parse_tree::Type::Unit(_) => TypeInfo::Unit,
            parse_tree::Type::Custom { name, .. } => TypeInfo::Custom {
                name: name.to_string(),
            },
            parse_tree::Type::Generic { name, args, .. } => {
                TypeInfo::Generic {
                    name: name.to_string(),
                    args: args.iter().map(Self::parse_tree_type_to_typeinfo).collect(),
                }
            }
            parse_tree::Type::Function { args, r#return, .. } => {
                let args = args.iter().map(Self::parse_tree_type_to_typeinfo).collect();
                let return_type = Self::parse_tree_type_to_typeinfo(r#return.as_ref());
                TypeInfo::Function {
                    args,
                    return_type: Box::new(return_type),
                }
            }
        }
    }

    fn generate_paths(&self, item: &str) -> Vec<Vec<String>> {
        self.current_paths.iter().map(|path| {
            let mut path = path.clone();
            path.push(item.to_string());
            path
        }).collect()
    }

    fn generate_comptime_paths(&self, item: &str) -> Vec<Vec<String>> {
        self.current_comptime_paths.iter().map(|path| {
            let mut path = path.clone();
            path.push(item.to_string());
            path
        }).collect()
    }

    fn resolve_item(&self, item: &str) -> Option<Vec<String>> {
        let paths = self.generate_paths(item);

        for path in &paths {
            if let Some(def) = self.files.get(&path[..(path.len() - 1)]) {
                if def.lookup_export(path) {
                    return Some(path.clone());
                }
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if let Some(def) = self.files.get(&local_path[..(local_path.len() - 1)]) {
            if def.lookup_internal(&local_path) {
                return Some(local_path);
            }
        }

        None
    }

    fn resolve_comptime_item(&self, item: &str) -> Option<Vec<String>> {
        let paths = self.generate_comptime_paths(item);

        for path in &paths {
            if let Some(def) = self.files.get(&path[..(path.len() - 1)]) {
                if def.lookup_export(path) {
                    return Some(path.clone());
                }
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if let Some(def) = self.files.get(&local_path[..(local_path.len() - 1)]) {
            if def.lookup_internal(&local_path) {
                return Some(local_path);
            }
        }

        None
    }

    fn load_file<'input>(&mut self, (path, file): &(PathBuf, parse_tree::File<'input>)) {
        let segments: Vec<String> = Self::generate_segments(&path);

        let mut file_def = FileDefinitions::new();

        for function in file.function_iter() {
            let arg_types: Vec<TypeInfo> = function.arguments.arguments.iter()
                .map(|arg| Self::parse_tree_type_to_typeinfo(&arg.r#type))
                .collect();
            let return_type = Self::parse_tree_type_to_typeinfo(&function.return_type);

            match &function.kind {
                parse_tree::FunctionKind::Named { name, .. } => {
                    let mut func_path = segments.clone();
                    func_path.push(name.to_string());
                    if function.public {
                        file_def.insert_export(func_path.clone());
                    } else {
                        file_def.insert_internal(func_path.clone());
                    }
                    self.function_signatures.insert(func_path, FunctionSignature {
                        argument_types: arg_types,
                        return_type,
                    });
                }
                parse_tree::FunctionKind::Operator { operator, .. } => {
                    let name = match operator {
                        parse_tree::Operator::Plus => "+",
                        parse_tree::Operator::Minus => {
                            if function.arguments.len() == 1 {
                                "-negate"
                            } else {
                                "-minus"
                            }
                        }
                        parse_tree::Operator::Multiply => "*",
                        parse_tree::Operator::Divide => "/",
                        parse_tree::Operator::Remainder => "%",
                        parse_tree::Operator::Equals => "==",
                        parse_tree::Operator::NotEquals => "!=",
                        parse_tree::Operator::LessThan => "<",
                        parse_tree::Operator::GreaterThan => ">",
                        parse_tree::Operator::LessThanEquals => "<=",
                        parse_tree::Operator::GreaterThanEquals => ">=",
                        parse_tree::Operator::Or => "||",
                        parse_tree::Operator::And => "&&",
                        parse_tree::Operator::Not => "!",
                    };
                    let mut func_path = segments.clone();
                    func_path.push(name.to_string());
                    if function.public {
                        file_def.insert_export(func_path.clone());
                    } else {
                        file_def.insert_internal(func_path.clone());
                    }
                    self.function_signatures.insert(func_path, FunctionSignature {
                        argument_types: arg_types,
                        return_type,
                    });
                }
            }
        }

        self.files.insert(segments, file_def);
    }

    pub fn resolve<'input>(&mut self, files: Vec<(PathBuf, parse_tree::File<'input>)>) -> ResolverResult<Vec<desugared_tree::File<'input>>> {
        for pair in &files {
            self.load_file(pair);
        }

        let mut errors = Vec::new();
        let mut resolved_files = Vec::new();
        for (path, file) in files {
            match self.resolve_file(path, file) {
                Ok(resolved_file) => resolved_files.push(resolved_file),
                Err(e) => errors.push(e),
            }
            self.clear_paths();
        }

        Ok(resolved_files)
    }

    fn resolve_file<'input>(&mut self, path: PathBuf, file: parse_tree::File<'input>) -> ResolverResult<desugared_tree::File<'input>> {
        let segments: Vec<String> = Self::generate_segments(&path);
        self.set_current_module(segments.clone());

        let parse_tree::File {
            top_level_statements,
            span,
        } = file;
        let mut functions = Vec::new();
        for statement in top_level_statements {
            match statement {
                parse_tree::TopLevelStatement::Import(path) => {
                    let path = path.to_vec_strings();
                    self.add_current_path(path);
                }
                parse_tree::TopLevelStatement::ComptimeImport(path) => {
                    let path = path.to_vec_strings();
                    self.add_current_comptime_path(path);
                }
                parse_tree::TopLevelStatement::Function(function) => {
                    functions.push(function);
                }
            }
        }

        let mut new_functions = Vec::with_capacity(functions.len());
        for function in functions {
            new_functions.push(self.resolve_function(&path, function)?);
        }
        let functions = new_functions;

        Ok(desugared_tree::File {
            file_path: path,
            functions,
            span
        })
    }

    fn resolve_function<'input>(&mut self, path: &PathBuf, function: parse_tree::Function<'input>) -> ResolverResult<desugared_tree::Function<'input>> {
        let parse_tree::Function {
            public,
            comptime,
            inline,
            kind,
            type_args,
            arguments,
            return_type,
            body,
            span
        } = function;

        let body = self.resolve_block(path, body, comptime)?;

        let mut segments = Self::generate_segments(path);
        match kind {
            parse_tree::FunctionKind::Named { name, .. } => {
                segments.push(name.to_string());
            }
            parse_tree::FunctionKind::Operator { operator, .. } => {
                let name = match operator {
                    parse_tree::Operator::Plus => "+",
                    parse_tree::Operator::Minus => {
                        if arguments.len() == 1 {
                            "-negate"
                        } else {
                            "-minus"
                        }
                    }
                    parse_tree::Operator::Multiply => "*",
                    parse_tree::Operator::Divide => "/",
                    parse_tree::Operator::Remainder => "%",
                    parse_tree::Operator::Equals => "==",
                    parse_tree::Operator::NotEquals => "!=",
                    parse_tree::Operator::LessThan => "<",
                    parse_tree::Operator::GreaterThan => ">",
                    parse_tree::Operator::LessThanEquals => "<=",
                    parse_tree::Operator::GreaterThanEquals => ">=",
                    parse_tree::Operator::Or => "||",
                    parse_tree::Operator::And => "&&",
                    parse_tree::Operator::Not => "!",
                };
                segments.push(name.to_string());
            }
        }
        let path = OwnedPath::from(segments);
        let arguments = Self::translate_function_arguments(arguments);
        let return_type = Self::translate_type(return_type);

        Ok(desugared_tree::Function {
            public,
            comptime,
            inline,
            path,
            type_args,
            arguments,
            return_type,
            body,
            span,
        })
    }

    fn resolve_block<'input>(&mut self, path: &PathBuf, block: parse_tree::Block<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Block<'input>> {
        let parse_tree::Block { statements, span } = block;
        
        // Push a new scope for this block
        self.push_scope();
        
        let mut new_statements = Vec::with_capacity(statements.len());
        let mut errors = Vec::new();
        for statement in statements {
            match self.resolve_statement(path, statement, in_comptime) {
                Ok(resolved_statement) => new_statements.push(resolved_statement),
                Err(e) => errors.push(e),
            }
        }

        // Pop the scope when leaving the block
        self.pop_scope();

        if errors.is_empty() {
            let statements = new_statements;
            return Ok(desugared_tree::Block {
                statements,
                span
            });
        }
        Err(ResolverError::Many(errors))
    }

    fn resolve_statement<'input>(&mut self, path: &PathBuf, statement: parse_tree::Statement<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Statement<'input>> {
        let statement = match statement {
            parse_tree::Statement::Let { name ,r#type, expr, span } => {
                let r#type = Self::translate_type(r#type);
                let expr = self.resolve_expression(path, expr, in_comptime)?;

                // Track the variable's type in the current scope
                if let Some(type_info) = Self::desugared_type_to_typeinfo(&r#type) {
                    self.add_variable(name.to_string(), type_info);
                }

                desugared_tree::Statement::Let { name, r#type, expr, span }
            }
            parse_tree::Statement::Assignment { target, expr, span } => {
                let target = self.resolve_expression(path, target, in_comptime)?;
                let expr = self.resolve_expression(path, expr, in_comptime)?;
                desugared_tree::Statement::Assignment { target, expr, span }
            }
            parse_tree::Statement::Expression { expr, span } => {
                let expr = self.resolve_expression(path, expr, in_comptime)?;
                desugared_tree::Statement::Expression { expr, span }
            }
            parse_tree::Statement::Comptime { block, span } => {
                let block = self.resolve_block(path, block, true)?;
                desugared_tree::Statement::Comptime { block, span }
            }
        };
        Ok(statement)
    }

    fn resolve_expression<'input>(&mut self, path: &PathBuf, expr: parse_tree::Expression<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Expression<'input>> {
        let expr = match expr {
            parse_tree::Expression::Return { value, span } => {
                let value = if let Some(value) = value {
                    Some(Box::new(self.resolve_expression(path, *value, in_comptime)?))
                } else {
                    None
                };
                desugared_tree::Expression::Return { value, span }
            }
            parse_tree::Expression::Parenthesized { expr, span } => {
                let expr = Box::new(self.resolve_expression(path, *expr, in_comptime)?);
                desugared_tree::Expression::Parenthesized { expr, span }
            }
            parse_tree::Expression::Variable { name, span } => {
                // Use the tracked type from the Let statement if available, using scope lookup
                let r#type = self.lookup_variable(&name)
                    .map(|ti| Self::typeinfo_to_desugared_type(&ti, span))
                    .unwrap_or_else(|| desugared_tree::Type::Unit(span));
                desugared_tree::Expression::Variable { name, r#type, span }
            }
            parse_tree::Expression::ConstantNumber { value, span } => {
                let inferred_type = Self::infer_number_type(&value);
                let r#type = Self::typeinfo_to_desugared_type(&inferred_type, span);
                desugared_tree::Expression::ConstantNumber { value, r#type, span }
            }
            parse_tree::Expression::ConstantString { value, span } => {
                desugared_tree::Expression::ConstantString { value, span }
            }
            parse_tree::Expression::FunctionCall { name, args, span } => {
                let mut new_args = Vec::with_capacity(args.len());
                let mut errors = Vec::new();
                for arg in args {
                    match self.resolve_expression(path, arg, in_comptime) {
                        Ok(res) => new_args.push(res),
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(ResolverError::Many(errors))
                }
                let args = new_args;

                let action = if in_comptime {
                    Self::resolve_item
                } else {
                    Self::resolve_comptime_item
                };

                let resolved_path = action(self, &name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Look up the function signature to get the return type and function type
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    // Collect argument types
                    let arg_types: Vec<_> = args.iter()
                        .map(|e| Self::get_expression_type(e))
                        .collect::<Option<Vec<_>>>()
                        .unwrap_or_default();
                    // Try to match generics
                    let generics = Self::match_generics(&sig.argument_types, &arg_types);
                    let (resolved_return, func_type) = if let Some(generics) = generics {
                        let substituted_return = Self::substitute_generics(&sig.return_type, &generics);
                        let substituted_sig = FunctionSignature {
                            argument_types: sig.argument_types.iter()
                                .map(|t| Self::substitute_generics(t, &generics))
                                .collect(),
                            return_type: substituted_return.clone(),
                        };
                        (
                            Self::typeinfo_to_desugared_type(&substituted_return, span),
                            Self::signature_to_desugared_type(&substituted_sig, span)
                        )
                    } else {
                        (
                            Self::typeinfo_to_desugared_type(&sig.return_type, span),
                            Self::signature_to_desugared_type(&sig, span)
                        )
                    };
                    (resolved_return, func_type)
                } else {
                    (desugared_tree::Type::Unit(span), desugared_tree::Type::Unit(span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    return_type,
                    function_type,
                    span,
                }
            }
            parse_tree::Expression::OperatorFunctionCall {
                operator,
                args,
                span
            } => {
                let mut new_args = Vec::with_capacity(args.len());
                let mut errors = Vec::new();
                for arg in args {
                    match self.resolve_expression(path, arg, in_comptime) {
                        Ok(res) => new_args.push(res),
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(ResolverError::Many(errors))
                }
                let args = new_args;

                let name = match operator {
                    parse_tree::Operator::Plus => "+",
                    parse_tree::Operator::Minus => {
                        if args.len() == 1 {
                            "-negate"
                        } else {
                            "-minus"
                        }
                    }
                    parse_tree::Operator::Multiply => "*",
                    parse_tree::Operator::Divide => "/",
                    parse_tree::Operator::Remainder => "%",
                    parse_tree::Operator::Equals => "==",
                    parse_tree::Operator::NotEquals => "!=",
                    parse_tree::Operator::LessThan => "<",
                    parse_tree::Operator::GreaterThan => ">",
                    parse_tree::Operator::LessThanEquals => "<=",
                    parse_tree::Operator::GreaterThanEquals => ">=",
                    parse_tree::Operator::Or => "||",
                    parse_tree::Operator::And => "&&",
                    parse_tree::Operator::Not => "!",
                };

                let action = if in_comptime {
                    Self::resolve_item
                } else {
                    Self::resolve_comptime_item
                };

                let resolved_path = action(self, name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Look up the function signature to get the return type and function type
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span)
                    )
                } else {
                    (desugared_tree::Type::Unit(span), desugared_tree::Type::Unit(span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    return_type,
                    function_type,
                    span,
                }
            }
            parse_tree::Expression::BinOp { lhs, rhs, op, span } => {
                let lhs_res = self.resolve_expression(path, *lhs, in_comptime);
                let rhs_res = self.resolve_expression(path, *rhs, in_comptime);

                let (lhs, rhs) = match (lhs_res, rhs_res) {
                    (Ok(lhs), Ok(rhs)) => (lhs, rhs),
                    (Err(err), Ok(_)) => {
                        return Err(err);
                    }
                    (Ok(_), Err(err)) => {
                        return Err(err);
                    }
                    (Err(err1), Err(err2)) => {
                        return Err(ResolverError::Many(vec![err1, err2]))
                    }
                };

                // Get the types of the arguments for overload resolution
                let lhs_type = Self::get_expression_type(&lhs);
                let rhs_type = Self::get_expression_type(&rhs);

                let args = vec![lhs, rhs];

                let name = match op {
                    parse_tree::Operator::Plus => "+",
                    parse_tree::Operator::Minus => "-minus",
                    parse_tree::Operator::Multiply => "*",
                    parse_tree::Operator::Divide => "/",
                    parse_tree::Operator::Remainder => "%",
                    parse_tree::Operator::Equals => "==",
                    parse_tree::Operator::NotEquals => "!=",
                    parse_tree::Operator::LessThan => "<",
                    parse_tree::Operator::GreaterThan => ">",
                    parse_tree::Operator::LessThanEquals => "<=",
                    parse_tree::Operator::GreaterThanEquals => ">=",
                    parse_tree::Operator::Or => "||",
                    parse_tree::Operator::And => "&&",
                    _ => unreachable!("! operator in bin"),
                };

                // Build the argument types for overload resolution
                let arg_types: Vec<TypeInfo> = vec![lhs_type, rhs_type]
                    .into_iter()
                    .collect::<Option<Vec<_>>>()
                    .unwrap_or_default();

                // Try to resolve with types first, fall back to untyped
                let resolved_path = if arg_types.len() == 2 {
                    self.resolve_function_with_types(name, &arg_types)
                        .or_else(|| {
                            let action = if in_comptime {
                                Self::resolve_item
                            } else {
                                Self::resolve_comptime_item
                            };
                            action(self, name)
                        })
                } else {
                    let action = if in_comptime {
                        Self::resolve_item
                    } else {
                        Self::resolve_comptime_item
                    };
                    action(self, name)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Look up the function signature to get the return type and function type
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span)
                    )
                } else {
                    (desugared_tree::Type::Unit(span), desugared_tree::Type::Unit(span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    return_type,
                    function_type,
                    span,
                }
            }
            parse_tree::Expression::Not { value, span } => {
                let value = self.resolve_expression(path, *value, in_comptime)?;
                let value_type = Self::get_expression_type(&value);
                let args = vec![value];

                let name = "!";

                // Build the argument types for overload resolution
                let arg_types: Vec<TypeInfo> = value_type.into_iter().collect();

                // Try to resolve with types first, fall back to untyped
                let resolved_path = if !arg_types.is_empty() {
                    self.resolve_function_with_types(name, &arg_types)
                        .or_else(|| {
                            let action = if in_comptime {
                                Self::resolve_item
                            } else {
                                Self::resolve_comptime_item
                            };
                            action(self, name)
                        })
                } else {
                    let action = if in_comptime {
                        Self::resolve_item
                    } else {
                        Self::resolve_comptime_item
                    };
                    action(self, name)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Look up the function signature to get the return type and function type
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span)
                    )
                } else {
                    (desugared_tree::Type::Unit(span), desugared_tree::Type::Unit(span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    return_type,
                    function_type,
                    span,
                }
            }
            parse_tree::Expression::Negation { value, span } => {
                let value = self.resolve_expression(path, *value, in_comptime)?;
                let value_type = Self::get_expression_type(&value);
                let args = vec![value];

                let name = "-negate";

                // Build the argument types for overload resolution
                let arg_types: Vec<TypeInfo> = value_type.into_iter().collect();

                // Try to resolve with types first, fall back to untyped
                let resolved_path = if !arg_types.is_empty() {
                    self.resolve_function_with_types(name, &arg_types)
                        .or_else(|| {
                            let action = if in_comptime {
                                Self::resolve_item
                            } else {
                                Self::resolve_comptime_item
                            };
                            action(self, name)
                        })
                } else {
                    let action = if in_comptime {
                        Self::resolve_item
                    } else {
                        Self::resolve_comptime_item
                    };
                    action(self, name)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Look up the function signature to get the return type and function type
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span)
                    )
                } else {
                    (desugared_tree::Type::Unit(span), desugared_tree::Type::Unit(span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    return_type,
                    function_type,
                    span,
                }
            }
            parse_tree::Expression::IfExpression(if_expr) => desugared_tree::Expression::IfExpression(self.resolve_if_expression(path, if_expr, in_comptime)?),

            parse_tree::Expression::DottedFunctionCall { .. } => unreachable!("DotedFunctionCall"),
        };
        Ok(expr)
    }

    fn resolve_if_expression<'input>(&mut self, path: &PathBuf, if_expr: parse_tree::IfExpression<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::IfExpression<'input>> {
        let parse_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            span
        } = if_expr;
        let mut errors = Vec::new();

        let condition = match self.resolve_expression(path, *condition, in_comptime) {
            Ok(res) => res,
            Err(err) => {
                errors.push(err);
                desugared_tree::Expression::Return { value: None, span }
            }
        };

        let then_block = match self.resolve_block(path, then_block, in_comptime) {
            Ok(res) => res,
            Err(err) => {
                errors.push(err);
                desugared_tree::Block { statements: Vec::new(), span }
            }
        };

        let mut new_elifs = Vec::with_capacity(elifs.len());
        for (condition, block) in elifs {
            let condition = match self.resolve_expression(path, condition, in_comptime) {
                Ok(res) => res,
                Err(err) => {
                    errors.push(err);
                    desugared_tree::Expression::Return { value: None, span }
                }
            };
            let block = match self.resolve_block(path, block, in_comptime) {
                Ok(res) => res,
                Err(err) => {
                    errors.push(err);
                    desugared_tree::Block { statements: Vec::new(), span }
                }
            };
            new_elifs.push((condition, block));
        }

        let elifs = new_elifs;

        let else_block = if let Some(else_block) = else_block {
            match self.resolve_block(path, else_block, in_comptime) {
                Ok(res) => Some(res),
                Err(err) => {
                    errors.push(err);
                    None
                }
            }
        } else {
            None
        };

        if !errors.is_empty() {
            return Err(ResolverError::Many(errors));
        }

        // Determine the return type of the if expression
        // If there's no else block, the if expression yields unit
        // Otherwise, we need to check the last expressions in all blocks
        let return_type = if else_block.is_none() {
            // No else block means the if expression always yields unit
            TypeInfo::Unit
        } else {
            // Get the yield type from then_block
            let then_type = Self::get_block_yield_type(&then_block);
            
            // Check all elifs for consistency
            let _elifs_types: Vec<_> = elifs.iter()
                .map(|(_, block)| Self::get_block_yield_type(block))
                .collect();
            
            // Get the yield type from else_block
            let _else_type = else_block.as_ref().and_then(Self::get_block_yield_type);
            
            // All branches must have the same type
            // For now, use the then_type as the unified type
            // In a full type checker, this would verify all branches match
            then_type.unwrap_or(TypeInfo::Unit)
        };

        let return_type_desugared = Self::typeinfo_to_desugared_type(&return_type, span);

        Ok(desugared_tree::IfExpression {
            condition: Box::new(condition),
            then_block,
            elifs,
            else_block,
            return_type: return_type_desugared,
            span,
        })
    }
}

impl FunctionResolver {

    fn generate_segments(path: &PathBuf) -> Vec<String> {
        path.iter().map(|os_str| {
            os_str.to_string_lossy().to_string()
        }).collect()
    }

    fn translate_function_arguments(args: parse_tree::FunctionArguments) -> desugared_tree::FunctionArguments {
        let parse_tree::FunctionArguments {
            arguments,
            span,
        } = args;

        let arguments = arguments.into_iter()
            .map(|arg| {
                let parse_tree::FunctionArgument {
                    lazy, name, r#type, span
                } = arg;

                desugared_tree::FunctionArgument {
                    lazy,
                    name,
                    r#type: Self::translate_type(r#type),
                    span
                }
            }).collect();


        desugared_tree::FunctionArguments {
            arguments,
            span
        }
    }

    fn translate_type(r#type: parse_tree::Type) -> desugared_tree::Type {
        match r#type {
            parse_tree::Type::U8(span) => desugared_tree::Type::U8(span),
            parse_tree::Type::I8(span) => desugared_tree::Type::I8(span),
            parse_tree::Type::U16(span) => desugared_tree::Type::U16(span),
            parse_tree::Type::I16(span) => desugared_tree::Type::I16(span),
            parse_tree::Type::U32(span) => desugared_tree::Type::U32(span),
            parse_tree::Type::I32(span) => desugared_tree::Type::I32(span),
            parse_tree::Type::U64(span) => desugared_tree::Type::U64(span),
            parse_tree::Type::I64(span) => desugared_tree::Type::I64(span),
            parse_tree::Type::F32(span) => desugared_tree::Type::F32(span),
            parse_tree::Type::F64(span) => desugared_tree::Type::F64(span),
            parse_tree::Type::Bool(span) => desugared_tree::Type::Bool(span),
            parse_tree::Type::String(span) => desugared_tree::Type::String(span),
            parse_tree::Type::Unit(span) => desugared_tree::Type::Unit(span),
            parse_tree::Type::Custom { name, span } => desugared_tree::Type::Custom { name, span},
            parse_tree::Type::Generic { name, args, span} => {
                let args = args.into_iter().map(Self::translate_type).collect::<Vec<_>>();
                desugared_tree::Type::Generic { name, args, span }
            }
            parse_tree::Type::Function { args, r#return, span } => {
                let args = args.into_iter().map(Self::translate_type).collect();
                let r#return = Box::new(Self::translate_type(*r#return));
                desugared_tree::Type::Function { args, r#return, span }
            }
        }
    }
}