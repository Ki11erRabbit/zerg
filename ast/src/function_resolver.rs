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

impl TypeInfo {
    /// Returns true if this TypeInfo represents any numeric type.
    fn is_numeric(&self) -> bool {
        matches!(self,
            TypeInfo::U8 | TypeInfo::I8 | TypeInfo::U16 | TypeInfo::I16 |
            TypeInfo::U32 | TypeInfo::I32 | TypeInfo::U64 | TypeInfo::I64 |
            TypeInfo::F32 | TypeInfo::F64
        )
    }

    /// Returns true if this TypeInfo represents a floating-point type.
    fn is_float(&self) -> bool {
        matches!(self, TypeInfo::F32 | TypeInfo::F64)
    }
}

pub struct FileDefinitions {
    /// The full file path including the name of the function
    exports: HashSet<Vec<String>>,
    /// The full file path including the name of the function
    internals: HashSet<Vec<String>>,
    /// Functions declared in extern blocks — visible within the module like internals,
    /// but flagged separately so the code generator knows they are linked externally.
    externals: HashSet<Vec<String>>,
}

impl FileDefinitions {
    pub fn new() -> Self {
        Self {
            exports: HashSet::new(),
            internals: HashSet::new(),
            externals: HashSet::new(),
        }
    }

    pub fn insert_export(&mut self, path: Vec<String>) {
        self.exports.insert(path);
    }

    pub fn insert_internal(&mut self, path: Vec<String>) {
        self.internals.insert(path);
    }

    pub fn insert_external(&mut self, path: Vec<String>) {
        self.externals.insert(path);
    }

    pub fn lookup_export(&self, path: &[String]) -> bool {
        self.exports.contains(path)
    }

    pub fn lookup_internal(&self, path: &[String]) -> bool {
        self.internals.contains(path)
    }

    pub fn lookup_external(&self, path: &[String]) -> bool {
        self.externals.contains(path)
    }

    /// Returns true if the path is resolvable from within this module (export, internal,
    /// or external declaration).
    pub fn lookup_any(&self, path: &[String]) -> bool {
        self.exports.contains(path) || self.internals.contains(path) || self.externals.contains(path)
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
    /// Each entry maps a variable name to a tuple of (type, was_declared_in_comptime)
    variable_scopes: Vec<HashMap<String, (TypeInfo, bool)>>,
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

    fn default_files_and_signatures() -> (HashMap<Vec<String>, FileDefinitions>, HashMap<Vec<String>, FunctionSignature>) {
        let mut files = HashMap::new();
        let mut function_signatures = HashMap::new();

        let mut compiler_file = FileDefinitions::new();

        compiler_file.insert_export(vec![String::from("compiler"), String::from("put_instruction")]);
        compiler_file.insert_export(vec![String::from("compiler"), String::from("use_variable")]);
        compiler_file.insert_export(vec![String::from("compiler"), String::from("set_variable")]);
        files.insert(vec![String::from("compiler")], compiler_file);

        let put_instruction = FunctionSignature {
            argument_types: vec![TypeInfo::String],
            return_type: TypeInfo::Unit,
        };
        function_signatures.insert(vec![String::from("compiler"), String::from("put_instruction")], put_instruction);

        let use_variable = FunctionSignature {
            argument_types: vec![TypeInfo::Generic { name: String::from("VariableHandle"), args: vec![TypeInfo::Custom { name: String::from("T")} ]}],
            return_type: TypeInfo::Unit,
        };
        function_signatures.insert(vec![String::from("compiler"), String::from("use_variable")], use_variable);
        let set_variable = FunctionSignature {
            argument_types: vec![
                TypeInfo::Generic { name: String::from("VariableHandle"), args: vec![TypeInfo::Custom { name: String::from("T")} ]},
                TypeInfo::Custom { name: String::from("T") },
            ],
            return_type: TypeInfo::Unit,
        };
        function_signatures.insert(vec![String::from("compiler"), String::from("set_variable")], set_variable);

        (files, function_signatures)
    }

    pub fn new() -> Self {
        let (files, function_signatures) = Self::default_files_and_signatures();

        Self {
            files,
            current_paths: HashSet::new(),
            current_comptime_paths: HashSet::new(),
            current_module: Vec::new(),
            function_signatures,
            // global scope with no comptime declarations
            variable_scopes: vec![HashMap::new()],
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

    /// Add a variable to the current scope.
    ///
    /// `is_comptime_decl` should be `true` when the variable is declared in a comptime
    /// context (either a comptime function or inside a comptime block).  This flag is
    /// used later during lookup so that a normal variable referenced inside a comptime
    /// block can be converted to a `VariableHandle<T>` type; comptime declarations are
    /// left alone.
    fn add_variable(&mut self, name: String, type_info: TypeInfo, is_comptime_decl: bool) {
        if let Some(current_scope) = self.variable_scopes.last_mut() {
            current_scope.insert(name, (type_info, is_comptime_decl));
        }
    }

    /// Look up a variable in the current scope chain (searching from innermost to
    /// outermost).  If `in_comptime` is true and the variable was *not* declared in a
    /// comptime context, the returned type will be wrapped in a
    /// `VariableHandle<...>` generic; this models crossing the comptime boundary.
    fn lookup_variable(&self, name: &str, in_comptime: bool) -> Option<TypeInfo> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some((type_info, is_comptime_decl)) = scope.get(name) {
                if in_comptime && !*is_comptime_decl {
                    // non-comptime variable accessed from a comptime context -> handle
                    return Some(TypeInfo::Generic {
                        name: "VariableHandle".to_string(),
                        args: vec![type_info.clone()],
                    });
                } else {
                    return Some(type_info.clone());
                }
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
    /// Infer the type of a numeric constant given an optional contextual expected type.
    ///
    /// Priority:
    ///   1. If an expected numeric type is provided and is compatible with the literal
    ///      (i.e. a float expected type won't be used for an integer literal), use it.
    ///   2. Otherwise fall back to `I32` for integers and `F64` for floats.
    fn infer_number_type(value: &str, expected: Option<&TypeInfo>) -> TypeInfo {
        let is_float_literal = value.contains('.');

        if let Some(hint) = expected {
            if hint.is_numeric() {
                // Don't coerce an integer literal into a float type — that would silently
                // change the value's semantics (e.g. `let x: f32 = 1` should stay `1`,
                // not become `1.0`).  We only accept the hint when the float-ness of the
                // literal and the hint agree.
                let hint_is_float = hint.is_float();
                if is_float_literal == hint_is_float {
                    return hint.clone();
                }
                // If the literal is an integer but the hint is a float, we still allow
                // it — this covers the common case of `let x: f32 = 0` or passing `1`
                // into an `f32` parameter.
                if !is_float_literal && hint_is_float {
                    return hint.clone();
                }
            }
        }

        // Default fallback
        if is_float_literal {
            TypeInfo::F64
        } else {
            TypeInfo::I32
        }
    }

    /// Given an already-resolved expression and a concrete type hint discovered *after*
    /// the fact (e.g. from the selected overload's signature), re-coerce bare numeric
    /// literals to the hint type.  Every other expression kind is returned unchanged.
    ///
    /// This is the second pass of the two-pass strategy: args are resolved first (so
    /// their natural types can drive overload selection), then this function fixes up
    /// any literals whose type the chosen signature constrains more precisely.
    fn recoerce_number_literal<'input>(
        expr: desugared_tree::Expression,
        hint: &TypeInfo,
        span: Span,
    ) -> desugared_tree::Expression {
        match expr {
            desugared_tree::Expression::ConstantNumber { value, .. } => {
                let better_type = Self::infer_number_type(&value, Some(hint));
                desugared_tree::Expression::ConstantNumber {
                    value,
                    r#type: Self::typeinfo_to_desugared_type(&better_type, span),
                    span,
                }
            }
            // Transparently unwrap parentheses
            desugared_tree::Expression::Parenthesized { expr: inner, span } => {
                desugared_tree::Expression::Parenthesized {
                    expr: Box::new(Self::recoerce_number_literal(*inner, hint, span)),
                    span,
                }
            }
            other => other,
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
            desugared_tree::Expression::ConstantBool { .. } => {
                Some(TypeInfo::Bool)
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

    fn typeinfo_to_desugared_type(info: &TypeInfo, span: Span) -> desugared_tree::Type {
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
                name: name.clone(),
                span,
            },
            TypeInfo::Generic { name, args } => {
                let desugared_args = args.iter()
                    .map(|arg| Self::typeinfo_to_desugared_type(arg, span))
                    .collect();
                desugared_tree::Type::Generic {
                    name: name.clone(),
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

    fn signature_to_desugared_type(sig: &FunctionSignature, span: Span) -> desugared_tree::Type {
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

    /// Resolve the file/module key that best matches a module path suffix.
    /// Extracted from the old inline closures in resolve_item/resolve_comptime_item
    /// so it isn't duplicated.
    fn find_module_key(module_path: &[String], files: &HashMap<Vec<String>, FileDefinitions>) -> Option<Vec<String>> {
        if files.contains_key(module_path) {
            return Some(module_path.to_vec());
        }
        for key in files.keys() {
            if key.len() < module_path.len() { continue; }
            let start = key.len() - module_path.len();
            let mut matched = true;
            for (i, seg) in module_path.iter().enumerate() {
                let key_seg = &key[start + i];
                if i + 1 == module_path.len() {
                    let key_base = key_seg.split('.').next().unwrap_or(key_seg);
                    if key_base != seg { matched = false; break; }
                } else {
                    if key_seg != seg { matched = false; break; }
                }
            }
            if matched {
                return Some(key.clone());
            }
        }
        None
    }

    /// Score a candidate path against a set of call-site argument types.
    ///
    /// Returns `None` if the candidate signature has the wrong arity (hard
    /// mismatch).  Otherwise returns a score where **higher is better**:
    ///   2 per argument that matches exactly,
    ///   1 per argument where the call-site type is a "default" literal type
    ///     (I32 / F64) that could be coerced into the parameter type,
    ///   0 when `arg_types` is empty (no type info available — any candidate
    ///     is equally valid).
    fn score_candidate(&self, path: &[String], arg_types: &[TypeInfo]) -> Option<usize> {
        if arg_types.is_empty() {
            return Some(0);
        }
        let sig = self.function_signatures.get(path)?;
        if sig.argument_types.len() != arg_types.len() {
            return None;
        }
        let default_int   = TypeInfo::I32;
        let default_float = TypeInfo::F64;
        let mut score = 0usize;
        for (param, arg) in sig.argument_types.iter().zip(arg_types.iter()) {
            if param == arg {
                score += 2;
            } else if (arg == &default_int || arg == &default_float) && param.is_numeric() {
                // The call-site type is a bare-literal default that could be coerced.
                score += 1;
            } else {
                // Hard type mismatch — this candidate cannot serve this call.
                return None;
            }
        }
        Some(score)
    }

    /// Collect every visible candidate path for `item` in normal (non-comptime) scope.
    fn candidates_for_item(&self, item: &str) -> Vec<Vec<String>> {
        let mut candidates: Vec<Vec<String>> = Vec::new();

        // Explicit module-qualified path (e.g. "math::add" or "math.add").
        if item.contains("::") || item.contains('.') {
            let segments: Vec<String> = if item.contains("::") {
                item.split("::").map(|s| s.to_string()).collect()
            } else {
                item.split('.').map(|s| s.to_string()).collect()
            };
            let module_path = &segments[..(segments.len() - 1)];

            if let Some(module_key) = Self::find_module_key(module_path, &self.files) {
                let mut full = module_key.clone();
                full.push(segments[segments.len() - 1].clone());
                if self.files.get(&module_key).map_or(false, |d| d.lookup_export(&full)) {
                    candidates.push(full);
                }
            }

            let mut local_path = self.current_module.clone();
            local_path.extend_from_slice(module_path);
            if let Some(module_key) = Self::find_module_key(&local_path, &self.files) {
                let mut full = module_key.clone();
                full.push(segments[segments.len() - 1].clone());
                if self.files.get(&module_key).map_or(false, |d| d.lookup_any(&full)) {
                    if !candidates.contains(&full) {
                        candidates.push(full);
                    }
                }
            }

            return candidates; // explicit path — no further ambiguity search needed
        }

        // Imported modules in scope.
        for path in self.generate_paths(item) {
            if self.files.get(&path[..(path.len() - 1)]).map_or(false, |d| d.lookup_export(&path)) {
                candidates.push(path);
            }
        }

        // Same module (exports, internals, externals).
        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if self.files.get(&local_path[..(local_path.len() - 1)]).map_or(false, |d| d.lookup_any(&local_path)) {
            if !candidates.contains(&local_path) {
                candidates.push(local_path);
            }
        }

        candidates
    }

    /// Collect every visible candidate path for `item` in comptime scope.
    fn candidates_for_comptime_item(&self, item: &str) -> Vec<Vec<String>> {
        let mut candidates: Vec<Vec<String>> = Vec::new();

        if item.contains("::") || item.contains('.') {
            let segments: Vec<String> = if item.contains("::") {
                item.split("::").map(|s| s.to_string()).collect()
            } else {
                item.split('.').map(|s| s.to_string()).collect()
            };
            let module_path = &segments[..(segments.len() - 1)];

            if let Some(module_key) = Self::find_module_key(module_path, &self.files) {
                let mut full = module_key.clone();
                full.push(segments[segments.len() - 1].clone());
                if self.files.get(&module_key).map_or(false, |d| d.lookup_export(&full) || d.lookup_external(&full)) {
                    candidates.push(full);
                }
            }

            let mut local_path = self.current_module.clone();
            local_path.extend_from_slice(module_path);
            if let Some(module_key) = Self::find_module_key(&local_path, &self.files) {
                let mut full = module_key.clone();
                full.push(segments[segments.len() - 1].clone());
                if self.files.get(&module_key).map_or(false, |d| d.lookup_any(&full)) {
                    if !candidates.contains(&full) {
                        candidates.push(full);
                    }
                }
            }

            return candidates;
        }

        for path in self.generate_comptime_paths(item) {
            if self.files.get(&path[..(path.len() - 1)]).map_or(false, |d| d.lookup_export(&path) || d.lookup_external(&path)) {
                candidates.push(path);
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if self.files.get(&local_path[..(local_path.len() - 1)]).map_or(false, |d| d.lookup_any(&local_path)) {
            if !candidates.contains(&local_path) {
                candidates.push(local_path);
            }
        }

        candidates
    }

    /// Resolve `item` to the best-matching candidate given the call-site arg types.
    /// When arg types are unknown or unavailable, falls back to returning the sole
    /// candidate (or `None` if there are none).
    fn resolve_item(&self, item: &str) -> Option<Vec<String>> {
        self.resolve_item_with_args(item, &[])
    }

    fn resolve_item_with_args(&self, item: &str, arg_types: &[TypeInfo]) -> Option<Vec<String>> {
        let candidates = self.candidates_for_item(item);
        self.pick_best_candidate(candidates, arg_types)
    }

    fn resolve_comptime_item(&self, item: &str) -> Option<Vec<String>> {
        self.resolve_comptime_item_with_args(item, &[])
    }

    fn resolve_comptime_item_with_args(&self, item: &str, arg_types: &[TypeInfo]) -> Option<Vec<String>> {
        let candidates = self.candidates_for_comptime_item(item);
        self.pick_best_candidate(candidates, arg_types)
    }

    /// Given a list of candidates and call-site arg types, return the one with the
    /// highest score.  Returns `None` only if the candidate list is empty.
    ///
    /// Scoring priority (highest wins):
    ///   1. Candidates that score against the arg types (exact or coercible matches).
    ///   2. If *no* candidate scores at all (e.g. the call site has incomplete type
    ///      info, or the function takes no arguments), fall back to the first
    ///      candidate rather than returning `None`.  This preserves the pre-scoring
    ///      behaviour for cases like comptime builtins where arg types may not yet
    ///      be fully resolved.
    fn pick_best_candidate(&self, candidates: Vec<Vec<String>>, arg_types: &[TypeInfo]) -> Option<Vec<String>> {
        if candidates.is_empty() {
            return None;
        }
        // No type info available — return the first (and hopefully only) candidate.
        if arg_types.is_empty() {
            return Some(candidates.into_iter().next().unwrap());
        }
        let mut best: Option<(usize, Vec<String>)> = None;
        let mut fallback: Option<Vec<String>> = None;
        for candidate in candidates {
            // Keep the first candidate as a fallback in case nothing scores.
            if fallback.is_none() {
                fallback = Some(candidate.clone());
            }
            if let Some(score) = self.score_candidate(&candidate, arg_types) {
                match &best {
                    None => best = Some((score, candidate)),
                    Some((best_score, _)) if score > *best_score => best = Some((score, candidate)),
                    _ => {}
                }
            }
        }
        // Use the scored winner if we got one; otherwise fall back to the first candidate
        // so that calls with incomplete type information still resolve.
        best.map(|(_, path)| path).or(fallback)
    }

    fn load_file<'input>(&mut self, (path, file): &(PathBuf, parse_tree::File<'input>)) {
        let segments: Vec<String> = Self::generate_segments(&path);

        let mut file_def = FileDefinitions::new();

        // register normal functions
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

        // Register extern-block functions using insert_external so the code generator
        // can distinguish them from ordinary definitions.  They live in the *module's*
        // namespace (same path segments as the file), NOT under the library name.
        for r#extern in file.extern_iter() {
            for function in &r#extern.functions {
                let arg_types: Vec<TypeInfo> = function.arguments.arguments.iter()
                    .map(|arg| Self::parse_tree_type_to_typeinfo(&arg.r#type))
                    .collect();
                let return_type = Self::parse_tree_type_to_typeinfo(&function.return_type);
                match &function.kind {
                    parse_tree::FunctionKind::Named { name, .. } => {
                        let mut func_path = segments.clone();
                        func_path.push(name.to_string());
                        // Always register as external regardless of the `pub` modifier;
                        // visibility for extern functions is controlled by whether the
                        // containing module is imported, not by the extern block itself.
                        file_def.insert_external(func_path.clone());
                        self.function_signatures.insert(func_path, FunctionSignature {
                            argument_types: arg_types.clone(),
                            return_type: return_type.clone(),
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
                        file_def.insert_external(func_path.clone());
                        self.function_signatures.insert(func_path, FunctionSignature {
                            argument_types: arg_types.clone(),
                            return_type: return_type.clone(),
                        });
                    }
                }
            }
        }

        self.files.insert(segments, file_def);
    }

    pub fn resolve<'input>(&mut self, files: Vec<(PathBuf, parse_tree::File<'input>)>) -> ResolverResult<Vec<desugared_tree::File>> {
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

        if !errors.is_empty() {
            return Err(ResolverError::Many(errors));
        }

        Ok(resolved_files)
    }

    fn resolve_file<'input>(&mut self, path: PathBuf, file: parse_tree::File<'input>) -> ResolverResult<desugared_tree::File> {
        let segments: Vec<String> = Self::generate_segments(&path);
        self.set_current_module(segments.clone());

        let parse_tree::File {
            top_level_statements,
            span,
        } = file;
        let mut functions = Vec::new();
        let mut externs_raw = Vec::new();
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
                parse_tree::TopLevelStatement::Extern(r#extern) => {
                    // Stash for desugaring into the externals list on the file node.
                    // Do NOT also push the contained functions into `functions` — they
                    // are resolved below via externs_raw to avoid double-resolution.
                    externs_raw.push(r#extern.clone());
                }
            }
        }

        let mut new_functions = Vec::with_capacity(functions.len());
        for function in functions {
            new_functions.push(self.resolve_function(&path, function)?);
        }
        let functions = new_functions;

        // Resolve extern-block functions separately and collect them into desugared
        // Extern nodes.  Each extern function ends up in the file's `externals` list
        // rather than its `functions` list so that downstream passes can treat them
        // differently (e.g. emit an `extern "C"` declaration instead of a definition).
        let mut new_externs = Vec::with_capacity(externs_raw.len());
        for r#extern in externs_raw {
            let mut desugared_funcs = Vec::with_capacity(r#extern.functions.len());
            for function in r#extern.functions {
                desugared_funcs.push(self.resolve_function(&path, function)?);
            }
            new_externs.push(desugared_tree::Extern {
                library: r#extern.library.to_string(),
                functions: desugared_funcs,
                span: r#extern.span,
            });
        }

        Ok(desugared_tree::File {
            file_path: path,
            functions,
            externals: new_externs,
            span
        })
    }

    fn resolve_function<'input>(&mut self, path: &PathBuf, function: parse_tree::Function<'input>) -> ResolverResult<desugared_tree::Function> {
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

        let type_args = type_args.into_iter().map(|x| x.to_string()).collect();

        // Push a scope for function arguments so they are visible inside the body.
        self.push_scope();
        for arg in &arguments.arguments {
            let type_info = Self::parse_tree_type_to_typeinfo(&arg.r#type);
            self.add_variable(arg.name.to_string(), type_info, comptime);
        }

        let body = self.resolve_block(path, body, comptime)?;

        self.pop_scope();

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

    fn resolve_block<'input>(&mut self, path: &PathBuf, block: parse_tree::Block<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Block> {
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

    fn resolve_statement<'input>(&mut self, path: &PathBuf, statement: parse_tree::Statement<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Statement> {
        let statement = match statement {
            parse_tree::Statement::Let { name, r#type, expr, span } => {
                let r#type = Self::translate_type(r#type);

                // Extract the declared type as a hint for numeric literal inference.
                // This covers `let x: u8 = 42` — without the hint the literal would
                // default to I32.
                let type_hint = Self::desugared_type_to_typeinfo(&r#type);
                let expr = self.resolve_expression(path, expr, in_comptime, type_hint.as_ref())?;

                // Track the variable's type in the current scope
                if let Some(type_info) = Self::desugared_type_to_typeinfo(&r#type) {
                    self.add_variable(name.to_string(), type_info, in_comptime);
                }

                desugared_tree::Statement::Let { name: name.to_string(), r#type, expr, span }
            }
            parse_tree::Statement::Assignment { target, expr, span } => {
                // For assignments, use the type of the target variable as the hint.
                let target_hint = match &target {
                    parse_tree::Expression::Variable { name, .. } => {
                        self.lookup_variable(name, in_comptime)
                    }
                    _ => None,
                };
                let target = self.resolve_expression(path, target, in_comptime, None)?;
                let expr = self.resolve_expression(path, expr, in_comptime, target_hint.as_ref())?;
                desugared_tree::Statement::Assignment { target, expr, span }
            }
            parse_tree::Statement::Expression { expr, span } => {
                let expr = self.resolve_expression(path, expr, in_comptime, None)?;
                desugared_tree::Statement::Expression { expr, span }
            }
            parse_tree::Statement::Comptime { block, span } => {
                let block = self.resolve_block(path, block, true)?;
                desugared_tree::Statement::Comptime { block, span }
            }
        };
        Ok(statement)
    }

    fn resolve_expression<'input>(
        &mut self,
        path: &PathBuf,
        expr: parse_tree::Expression<'input>,
        in_comptime: bool,
        // Contextual type hint propagated from the surrounding declaration or call site.
        // Used to pick the right concrete type for bare numeric literals.
        expected_type: Option<&TypeInfo>,
    ) -> ResolverResult<desugared_tree::Expression> {
        let expr = match expr {
            parse_tree::Expression::Return { value, span } => {
                let value = if let Some(value) = value {
                    Some(Box::new(self.resolve_expression(path, *value, in_comptime, None)?))
                } else {
                    None
                };
                desugared_tree::Expression::Return { value, span }
            }
            parse_tree::Expression::Parenthesized { expr, span } => {
                // Forward the hint through parentheses transparently.
                let expr = Box::new(self.resolve_expression(path, *expr, in_comptime, expected_type)?);
                desugared_tree::Expression::Parenthesized { expr, span }
            }
            parse_tree::Expression::Variable { name, span } => {
                // Use the tracked type from the Let statement if available, using scope lookup
                let r#type = self.lookup_variable(&name, in_comptime)
                    .map(|ti| Self::typeinfo_to_desugared_type(&ti, span))
                    .unwrap_or_else(|| desugared_tree::Type::Unit(span));
                desugared_tree::Expression::Variable { name: name.to_string(), r#type, span }
            }
            parse_tree::Expression::ConstantNumber { value, span } => {
                // Use the contextual hint when available so that, e.g., `let x: u8 = 5`
                // resolves the literal `5` as U8 rather than the default I32.
                let inferred_type = Self::infer_number_type(&value, expected_type);
                let r#type = Self::typeinfo_to_desugared_type(&inferred_type, span);
                desugared_tree::Expression::ConstantNumber { value: value.to_string(), r#type, span }
            }
            parse_tree::Expression::ConstantString { value, span } => {
                desugared_tree::Expression::ConstantString { value: value.to_string(), span }
            }
            parse_tree::Expression::ConstantBool { value, span } => {
                desugared_tree::Expression::ConstantBool { value, span }
            }
            parse_tree::Expression::FunctionCall { name, args, span } => {
                // Pass 1 — resolve args without a hint so their natural types drive
                // overload resolution.  e.g. `foo(x)` where `x: i64` must select
                // `foo(i64)` not `foo(i32)`.
                let mut resolved_args = Vec::with_capacity(args.len());
                let mut errors = Vec::new();
                for arg in args {
                    match self.resolve_expression(path, arg, in_comptime, None) {
                        Ok(res) => resolved_args.push(res),
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(ResolverError::Many(errors));
                }

                // Resolve the function path using the arg types so overloading works.
                // Collect arg types from the already-resolved arguments so the scorer
                // can pick the right overload when multiple candidates are in scope.
                let resolved_arg_types: Vec<TypeInfo> = resolved_args.iter()
                    .filter_map(Self::get_expression_type)
                    .collect();
                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(&name, &resolved_arg_types)
                } else {
                    self.resolve_item_with_args(&name, &resolved_arg_types)
                };
                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Pass 2 — re-coerce bare numeric literals whose type the selected
                // overload constrains more precisely.  e.g. `foo_u8(42)` should give
                // the literal type U8, not the default I32.
                let param_types: Vec<TypeInfo> = self.function_signatures
                    .get(&resolved_path)
                    .map(|sig| sig.argument_types.clone())
                    .unwrap_or_default();
                let args: Vec<_> = resolved_args.into_iter().enumerate().map(|(i, arg)| {
                    if let Some(hint) = param_types.get(i) {
                        Self::recoerce_number_literal(arg, hint, span)
                    } else {
                        arg
                    }
                }).collect();

                // Build return/function types, substituting any generics.
                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    let arg_types: Vec<_> = args.iter()
                        .filter_map(Self::get_expression_type)
                        .collect();
                    let generics = Self::match_generics(&sig.argument_types, &arg_types);
                    if let Some(generics) = generics {
                        let substituted_return = Self::substitute_generics(&sig.return_type, &generics);
                        let substituted_sig = FunctionSignature {
                            argument_types: sig.argument_types.iter()
                                .map(|t| Self::substitute_generics(t, &generics))
                                .collect(),
                            return_type: substituted_return.clone(),
                        };
                        (
                            Self::typeinfo_to_desugared_type(&substituted_return, span),
                            Self::signature_to_desugared_type(&substituted_sig, span),
                        )
                    } else {
                        (
                            Self::typeinfo_to_desugared_type(&sig.return_type, span),
                            Self::signature_to_desugared_type(sig, span),
                        )
                    }
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
                // Determine the operator name from arity (needed before arg resolution
                // for -negate vs -minus, but arity is known from the parse tree).
                let name = match operator {
                    parse_tree::Operator::Plus => "+",
                    parse_tree::Operator::Minus => {
                        if args.len() == 1 { "-negate" } else { "-minus" }
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

                // Pass 1 — resolve args first so their natural types drive overload
                // selection (e.g. `std::i32::+` vs `std::i64::+`).
                let mut resolved_args = Vec::with_capacity(args.len());
                let mut errors = Vec::new();
                for arg in args {
                    match self.resolve_expression(path, arg, in_comptime, None) {
                        Ok(res) => resolved_args.push(res),
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(ResolverError::Many(errors));
                }

                // Resolve the operator path using arg types for overload selection.
                let resolved_arg_types: Vec<TypeInfo> = resolved_args.iter()
                    .filter_map(Self::get_expression_type)
                    .collect();
                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(name, &resolved_arg_types)
                } else {
                    self.resolve_item_with_args(name, &resolved_arg_types)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                // Pass 2 — re-coerce bare number literals using the selected signature.
                let param_types: Vec<TypeInfo> = self.function_signatures
                    .get(&resolved_path)
                    .map(|sig| sig.argument_types.clone())
                    .unwrap_or_default();
                let args: Vec<_> = resolved_args.into_iter().enumerate().map(|(i, arg)| {
                    if let Some(hint) = param_types.get(i) {
                        Self::recoerce_number_literal(arg, hint, span)
                    } else {
                        arg
                    }
                }).collect();

                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(sig, span),
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
                // Resolve both sides without hints first so their natural types are known.
                let lhs_res = self.resolve_expression(path, *lhs, in_comptime, None);
                let rhs_res = self.resolve_expression(path, *rhs, in_comptime, None);

                let (mut lhs, mut rhs) = match (lhs_res, rhs_res) {
                    (Ok(lhs), Ok(rhs)) => (lhs, rhs),
                    (Err(err), Ok(_)) => return Err(err),
                    (Ok(_), Err(err)) => return Err(err),
                    (Err(err1), Err(err2)) => return Err(ResolverError::Many(vec![err1, err2])),
                };

                // Cross-coerce: if one side has a concrete numeric type and the other
                // is a bare literal that defaulted (I32 or F64), fix the literal so
                // both sides agree.  This is what makes `x + 5` correct when `x: i64`
                // — without this the wasm backend sees [i64, i32].
                {
                    let lhs_type = Self::get_expression_type(&lhs);
                    let rhs_type = Self::get_expression_type(&rhs);
                    if let (Some(lt), Some(rt)) = (&lhs_type, &rhs_type) {
                        if lt.is_numeric() && rt.is_numeric() && lt != rt {
                            // I32 and F64 are the "no hint" fallbacks from infer_number_type.
                            // If one side is a non-default type, use it to fix the other.
                            let default_int   = TypeInfo::I32;
                            let default_float = TypeInfo::F64;
                            if rt == &default_int || rt == &default_float {
                                rhs = Self::recoerce_number_literal(rhs, lt, span);
                            } else if lt == &default_int || lt == &default_float {
                                lhs = Self::recoerce_number_literal(lhs, rt, span);
                            }
                        }
                    }
                }

                // Re-read types after potential recoercion for overload resolution.
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

                // Resolve with arg types for overload selection.
                let arg_types_slice: &[TypeInfo] = if arg_types.len() == 2 { &arg_types } else { &[] };
                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(name, arg_types_slice)
                } else {
                    self.resolve_item_with_args(name, arg_types_slice)
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
                let value = self.resolve_expression(path, *value, in_comptime, None)?;
                let value_type = Self::get_expression_type(&value);
                let args = vec![value];

                let name = "!";

                // Build the argument types for overload resolution
                let arg_types: Vec<TypeInfo> = value_type.into_iter().collect();

                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(name, &arg_types)
                } else {
                    self.resolve_item_with_args(name, &arg_types)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span),
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
                let value = self.resolve_expression(path, *value, in_comptime, None)?;
                let value_type = Self::get_expression_type(&value);
                let args = vec![value];

                let name = "-negate";

                // Build the argument types for overload resolution
                let arg_types: Vec<TypeInfo> = value_type.into_iter().collect();

                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(name, &arg_types)
                } else {
                    self.resolve_item_with_args(name, &arg_types)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(&sig, span),
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

            parse_tree::Expression::DottedFunctionCall { base, name, args, span } => {
                // Determine the base string for the module path first so we can look up
                // the resolved path (and thus parameter types) before resolving args.
                fn expr_to_base_str(expr: &parse_tree::Expression<'_>) -> Option<String> {
                    match expr {
                        parse_tree::Expression::Variable { name, .. } => Some(name.to_string()),
                        parse_tree::Expression::FunctionCall { name, .. } => Some(name.to_string()),
                        parse_tree::Expression::DottedFunctionCall { base, name, .. } => {
                            if let Some(mut s) = expr_to_base_str(base) {
                                s.push_str("::");
                                s.push_str(&name.to_string());
                                Some(s)
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                }

                let base_str_opt = expr_to_base_str(base.as_ref());
                let Some(base_str) = base_str_opt else {
                    return Err(ResolverError::CannotInferType(path.clone(), span));
                };

                // Build the full item string like "module::item"
                let full_item = format!("{}::{}", base_str, name.to_string());

                // Pass 1 — resolve args first so their types drive overload selection.
                let mut resolved_args = Vec::with_capacity(args.len());
                let mut errors = Vec::new();
                for arg in args {
                    match self.resolve_expression(path, arg, in_comptime, None) {
                        Ok(res) => resolved_args.push(res),
                        Err(e) => errors.push(e),
                    }
                }
                if !errors.is_empty() {
                    return Err(ResolverError::Many(errors));
                }

                // Resolve the path now that we have concrete arg types.
                let resolved_arg_types: Vec<TypeInfo> = resolved_args.iter()
                    .filter_map(Self::get_expression_type)
                    .collect();
                let resolved_path = if in_comptime {
                    self.resolve_comptime_item_with_args(&full_item, &resolved_arg_types)
                } else {
                    self.resolve_item_with_args(&full_item, &resolved_arg_types)
                };

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(full_item, path.clone(), span))
                };

                // Pass 2 — re-coerce bare number literals using the selected signature.
                let param_types: Vec<TypeInfo> = self.function_signatures
                    .get(&resolved_path)
                    .map(|sig| sig.argument_types.clone())
                    .unwrap_or_default();
                let args: Vec<_> = resolved_args.into_iter().enumerate().map(|(i, arg)| {
                    if let Some(hint) = param_types.get(i) {
                        Self::recoerce_number_literal(arg, hint, span)
                    } else {
                        arg
                    }
                }).collect();

                let (return_type, function_type) = if let Some(sig) = self.function_signatures.get(&resolved_path) {
                    (
                        Self::typeinfo_to_desugared_type(&sig.return_type, span),
                        Self::signature_to_desugared_type(sig, span),
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
            },
        };
        Ok(expr)
    }

    fn resolve_if_expression<'input>(&mut self, path: &PathBuf, if_expr: parse_tree::IfExpression<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::IfExpression> {
        let parse_tree::IfExpression {
            condition,
            then_block,
            elifs,
            else_block,
            span
        } = if_expr;
        let mut errors = Vec::new();

        let condition = match self.resolve_expression(path, *condition, in_comptime, None) {
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
            let condition = match self.resolve_expression(path, condition, in_comptime, None) {
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
        let mut segs: Vec<String> = path.iter().map(|os_str| {
            os_str.to_string_lossy().to_string()
        }).collect();
        // Normalize last segment by removing .zerg extension if present
        if let Some(last) = segs.last_mut() {
            if last.ends_with(".zerg") {
                if let Some(stripped) = last.strip_suffix(".zerg") {
                    *last = stripped.to_string();
                }
            }
        }
        segs
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
                    name: name.to_string(),
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
            parse_tree::Type::Custom { name, span } => desugared_tree::Type::Custom { name: name.to_string(), span},
            parse_tree::Type::Generic { name, args, span} => {
                let args = args.into_iter().map(Self::translate_type).collect::<Vec<_>>();
                desugared_tree::Type::Generic { name: name.to_string(), args, span }
            }
            parse_tree::Type::Function { args, r#return, span } => {
                let args = args.into_iter().map(Self::translate_type).collect();
                let r#return = Box::new(Self::translate_type(*r#return));
                desugared_tree::Type::Function { args, r#return, span }
            }
        }
    }
}

// tests for variable handle transformation
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn variable_handle_in_comptime_lookup() {
        let mut resolver = FunctionResolver::new();

        // non-comptime variable should be wrapped when accessed inside comptime
        resolver.add_variable("x".to_string(), TypeInfo::U32, false);
        assert_eq!(resolver.lookup_variable("x", false), Some(TypeInfo::U32));
        assert_eq!(
            resolver.lookup_variable("x", true),
            Some(TypeInfo::Generic { name: "VariableHandle".to_string(), args: vec![TypeInfo::U32] }),
        );

        // a variable declared in a comptime context should not get wrapped
        resolver.push_scope();
        resolver.add_variable("y".to_string(), TypeInfo::I64, true);
        assert_eq!(resolver.lookup_variable("y", true), Some(TypeInfo::I64));
    }

    #[test]
    fn comptime_imported_builtin_resolves() {
        let mut resolver = FunctionResolver::new();
        // simulate having performed a `comptime import compiler;`
        resolver.add_current_comptime_path(vec!["compiler".to_string()]);

        // resolver should know about the builtin signatures we seeded in new()
        let path = resolver.resolve_comptime_item("use_variable");
        assert_eq!(path, Some(vec!["compiler".to_string(), "use_variable".to_string()]));

        let path2 = resolver.resolve_comptime_item("put_instruction");
        assert_eq!(path2, Some(vec!["compiler".to_string(), "put_instruction".to_string()]));
    }

    #[test]
    fn extern_function_resolves_in_same_module() {
        use std::collections::HashMap;

        let mut resolver = FunctionResolver::new();

        // Simulate a file at path ["mymodule"] that declares an extern function "printf"
        let mut file_def = FileDefinitions::new();
        file_def.insert_external(vec!["mymodule".to_string(), "printf".to_string()]);
        resolver.files.insert(vec!["mymodule".to_string()], file_def);
        resolver.function_signatures.insert(
            vec!["mymodule".to_string(), "printf".to_string()],
            FunctionSignature {
                argument_types: vec![TypeInfo::String],
                return_type: TypeInfo::Unit,
            },
        );

        // When the current module IS mymodule, lookup_any covers externals
        resolver.set_current_module(vec!["mymodule".to_string()]);
        let resolved = resolver.resolve_item("printf");
        assert_eq!(resolved, Some(vec!["mymodule".to_string(), "printf".to_string()]));
    }

    #[test]
    fn extern_function_not_visible_via_import() {
        let mut resolver = FunctionResolver::new();

        // Simulate a file at path ["libc"] that declares an extern function "malloc"
        let mut file_def = FileDefinitions::new();
        file_def.insert_external(vec!["libc".to_string(), "malloc".to_string()]);
        resolver.files.insert(vec!["libc".to_string()], file_def);
        resolver.function_signatures.insert(
            vec!["libc".to_string(), "malloc".to_string()],
            FunctionSignature {
                argument_types: vec![TypeInfo::U64],
                return_type: TypeInfo::Unit,
            },
        );

        // Importing ["libc"] puts it in scope; externals are visible to importers
        resolver.add_current_path(vec!["libc".to_string()]);
        resolver.set_current_module(vec!["mymodule".to_string()]);

        let resolved = resolver.resolve_item("malloc");
        assert_eq!(resolved, None);
    }

    #[test]
    fn number_inference_uses_declared_type() {
        // Integer literal with an explicit type annotation
        assert_eq!(TypeInfo::U8, FunctionResolver::infer_number_type("42", Some(&TypeInfo::U8)));
        assert_eq!(TypeInfo::I64, FunctionResolver::infer_number_type("100", Some(&TypeInfo::I64)));

        // Float literal with a float annotation
        assert_eq!(TypeInfo::F32, FunctionResolver::infer_number_type("3.14", Some(&TypeInfo::F32)));

        // Integer literal with a float annotation (common coercion: `let x: f32 = 0`)
        assert_eq!(TypeInfo::F32, FunctionResolver::infer_number_type("0", Some(&TypeInfo::F32)));

        // No hint falls back to defaults
        assert_eq!(TypeInfo::I32, FunctionResolver::infer_number_type("7", None));
        assert_eq!(TypeInfo::F64, FunctionResolver::infer_number_type("7.0", None));

        // Non-numeric hint is ignored, falls back to defaults
        assert_eq!(TypeInfo::I32, FunctionResolver::infer_number_type("7", Some(&TypeInfo::String)));
    }

    #[test]
    fn ambiguous_name_resolved_by_arg_types() {
        let mut resolver = FunctionResolver::new();

        // Two modules both export a function called "add" with different signatures.
        let mut mod_i32 = FileDefinitions::new();
        mod_i32.insert_export(vec!["math_i32".to_string(), "add".to_string()]);
        resolver.files.insert(vec!["math_i32".to_string()], mod_i32);
        resolver.function_signatures.insert(
            vec!["math_i32".to_string(), "add".to_string()],
            FunctionSignature { argument_types: vec![TypeInfo::I32, TypeInfo::I32], return_type: TypeInfo::I32 },
        );

        let mut mod_i64 = FileDefinitions::new();
        mod_i64.insert_export(vec!["math_i64".to_string(), "add".to_string()]);
        resolver.files.insert(vec!["math_i64".to_string()], mod_i64);
        resolver.function_signatures.insert(
            vec!["math_i64".to_string(), "add".to_string()],
            FunctionSignature { argument_types: vec![TypeInfo::I64, TypeInfo::I64], return_type: TypeInfo::I64 },
        );

        // Both modules are imported.
        resolver.add_current_path(vec!["math_i32".to_string()]);
        resolver.add_current_path(vec!["math_i64".to_string()]);
        resolver.set_current_module(vec!["mymodule".to_string()]);

        // With I64 arg types the i64 overload should win.
        let resolved = resolver.resolve_item_with_args("add", &[TypeInfo::I64, TypeInfo::I64]);
        assert_eq!(resolved, Some(vec!["math_i64".to_string(), "add".to_string()]));

        // With I32 arg types the i32 overload should win.
        let resolved = resolver.resolve_item_with_args("add", &[TypeInfo::I32, TypeInfo::I32]);
        assert_eq!(resolved, Some(vec!["math_i32".to_string(), "add".to_string()]));

        // With no type info, some candidate is returned (non-deterministic but non-None).
        let resolved = resolver.resolve_item_with_args("add", &[]);
        assert!(resolved.is_some());
    }

    #[test]
    fn binop_literal_cross_coercion() {
        // recoerce_number_literal should lift a default-typed literal to match a
        // concrete type discovered from the other operand.
        let span = Span::new(0, 0); // adjust if your Span type differs

        // I64 variable + bare `5` (defaults to I32) → both should become I64
        let lit_i32 = desugared_tree::Expression::ConstantNumber {
            value: String::from("5"),
            r#type: FunctionResolver::typeinfo_to_desugared_type(&TypeInfo::I32, span),
            span,
        };
        let result = FunctionResolver::recoerce_number_literal(lit_i32, &TypeInfo::I64, span);
        assert_eq!(
            FunctionResolver::get_expression_type(&result),
            Some(TypeInfo::I64),
            "literal should be recoerced from I32 to I64"
        );

        // A non-literal (variable) must not be changed.
        let var_expr = desugared_tree::Expression::Variable {
            name: String::from("x"),
            r#type: FunctionResolver::typeinfo_to_desugared_type(&TypeInfo::I64, span),
            span,
        };
        let unchanged = FunctionResolver::recoerce_number_literal(var_expr, &TypeInfo::U8, span);
        assert_eq!(
            FunctionResolver::get_expression_type(&unchanged),
            Some(TypeInfo::I64),
            "non-literal should be unchanged by recoerce"
        );
    }
}