use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use crate::{desugared_tree, parse_tree, OwnedPath, Span};

pub type ResolverResult<T> = Result<T, ResolverError>;

#[derive(Debug)]
pub enum ResolverError {
    Many(Vec<ResolverError>),
    UnknownFunction(String, PathBuf, Span),
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
        }
    }
}

impl std::error::Error for ResolverError {}

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
}

impl FunctionResolver {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            current_paths: HashSet::new(),
            current_comptime_paths: HashSet::new(),
            current_module: Vec::new(),
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
        let segments: Vec<String> = Self::generate_segements(&path);

        let mut file_def = FileDefinitions::new();

        for function in file.function_iter() {
            match &function.kind {
                parse_tree::FunctionKind::Named { name, .. } => {
                    let mut path = segments.clone();
                    path.push(name.to_string());
                    if function.public {
                        file_def.insert_export(path);
                    } else {
                        file_def.insert_internal(path);
                    }
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
                    let mut path = segments.clone();
                    path.push(name.to_string());
                    if function.public {
                        file_def.insert_export(path);
                    } else {
                        file_def.insert_internal(path);
                    }
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
        let segments: Vec<String> = Self::generate_segements(&path);
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
            arguments,
            return_type,
            body,
            span
        } = function;

        let body = self.resolve_block(path, body, comptime)?;

        let mut segments = Self::generate_segements(path);
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
            arguments,
            return_type,
            body,
            span,
        })
    }

    fn resolve_block<'input>(&mut self, path: &PathBuf, block: parse_tree::Block<'input>, in_comptime: bool) -> ResolverResult<desugared_tree::Block<'input>> {
        let parse_tree::Block { statements, span } = block;
        let mut new_statements = Vec::with_capacity(statements.len());
        let mut errors = Vec::new();
        for statement in statements {
            match self.resolve_statement(path, statement, in_comptime) {
                Ok(resolved_statement) => new_statements.push(resolved_statement),
                Err(e) => errors.push(e),
            }
        }

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
                desugared_tree::Expression::Variable { name, span }
            }
            parse_tree::Expression::ConstantNumber { value, span } => {
                desugared_tree::Expression::ConstantNumber { value, span }
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

                let resolved_path = action(self, name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
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

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
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

                let action = if in_comptime {
                    Self::resolve_item
                } else {
                    Self::resolve_comptime_item
                };

                let resolved_path = action(self, name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    span,
                }
            }
            parse_tree::Expression::Not { value, span } => {
                let value = self.resolve_expression(path, *value, in_comptime)?;
                let args = vec![value];

                let name = "!";

                let action = if in_comptime {
                    Self::resolve_item
                } else {
                    Self::resolve_comptime_item
                };

                let resolved_path = action(self, name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
                    span,
                }
            }
            parse_tree::Expression::Negation { value, span } => {
                let value = self.resolve_expression(path, *value, in_comptime)?;
                let args = vec![value];

                let name = "-negate";

                let action = if in_comptime {
                    Self::resolve_item
                } else {
                    Self::resolve_comptime_item
                };

                let resolved_path = action(self, name);

                let Some(resolved_path) = resolved_path else {
                    return Err(ResolverError::UnknownFunction(name.to_string(), path.clone(), span))
                };

                desugared_tree::Expression::FunctionCall {
                    name: OwnedPath::from(resolved_path),
                    args,
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

        Ok(desugared_tree::IfExpression {
            condition: Box::new(condition),
            then_block,
            elifs,
            else_block,
            span,
        })
    }
}

impl FunctionResolver {

    fn generate_segements(path: &PathBuf) -> Vec<String> {
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
            parse_tree::Type::String(span) => desugared_tree::Type::String(span),
            parse_tree::Type::Unit(span) => desugared_tree::Type::Unit(span),
            parse_tree::Type::Custom { name, span } => desugared_tree::Type::Custom { name, span},
            parse_tree::Type::Generic { name, args, span} => {
                let args = args.into_iter().map(Self::translate_type).collect::<Vec<_>>();
                desugared_tree::Type::Generic { name, args, span }
            }
        }
    }
}