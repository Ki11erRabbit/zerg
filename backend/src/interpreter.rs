mod interpreter_state;

use std::collections::HashMap;
use wasm_encoder::{Function, Instruction};
use ast::{desugared_tree, Span};
use ast::desugared_tree::{Block, Expression, IfExpression, Statement, Type};
use crate::{compiler, Compiler};
use crate::interpreter::interpreter_state::{InterpreterState, Value};

#[derive(Debug)]
pub enum InterpreterError {
    Many(Vec<InterpreterError>),
    MissMatchedTypes {
        expected: String,
        found: String,
        span: Span,
    }
}

#[derive(Debug, Copy, Clone)]
enum InternalFunction {
    /// Function takes in the name of the instruction as a string.
    PutInstruction,
    /// Takes in a variable handle and emits the instruction to use a local variable.
    UseVariable,
    /// Takes in a variable handle and a value and emits the instruction to set a local variable.
    SetVariable,
}

pub struct Interpreter {
    state: Vec<InterpreterState>,
    should_return: bool,
    internal_functions: HashMap<Vec<String>, InternalFunction>
}

impl<'input> Interpreter {
    pub fn new() -> Self {
        let mut internal_functions = HashMap::new();
        internal_functions.insert(vec![String::from("compiler"), String::from("put_instruction")], InternalFunction::PutInstruction);
        internal_functions.insert(vec![String::from("compiler"), String::from("use_variable")], InternalFunction::UseVariable);
        internal_functions.insert(vec![String::from("compiler"), String::from("set_variable")], InternalFunction::SetVariable);
        
        Self {
            state: Vec::new(),
            should_return: false,
            internal_functions
        }
    }

    fn push(&mut self) {
        self.state.last_mut().map(|state| state.push());
    }

    fn pop(&mut self) {
        self.state.last_mut().map(|state| state.pop());
    }

    fn push_state(&mut self) {
        self.state.push(InterpreterState::new());
    }

    fn pop_state(&mut self) {
        self.state.pop();
    }

    fn store(&mut self, name: &str, value: Value) {
        self.state.last_mut().map(|state| state.store(name, value));
    }

    fn get(&self, name: &str) -> Option<&Value> {
        self.state.last().map(|state| state.get(name)).flatten()
    }

    pub fn interpret_comptime_block(
        &mut self,
        compiler: &Compiler<'input>,
        function: &mut Function,
        block: Block<'input>,
    ) -> Result<(), InterpreterError> {
        let mut out = Value::Unit;
        self.push_state();
        self.push();
        for (name, location) in compiler.get_variables() {
            self.store(&name, Value::VariableHandle(location));
        }

        self.interpret_block(compiler, function, block, &mut out)?;
        self.pop_state();
        Ok(())
    }

    fn interpret_block(
        &mut self,
        compiler: &Compiler<'input>,
        function: &mut Function,
        block: Block<'input>,
        out: &mut Value,
    ) -> Result<Value, InterpreterError> {
        self.push();
        let mut out_value = Value::Unit;
        let statements_len = block.statements.len();
        for (i, stmt) in block.statements.into_iter().enumerate() {
            if self.should_return {
                return Ok(Value::Unit);
            }
            let value = self.interpret_statement(compiler, function, stmt, out)?;
            if i == statements_len - 1 {
                out_value = value;
            }
        }
        self.pop();
        Ok(out_value)
    }

    fn interpret_statement(
        &mut self,
        compiler: &Compiler<'input>,
        function: &mut Function,
        statement: Statement<'input>,
        out: &mut Value,
    ) -> Result<Value, InterpreterError> {
        match statement {
            Statement::Let { name, expr, .. } => {
                let value = self.interpret_expr(compiler, function, expr, out)?;
                self.store(&name, value);
            }
            Statement::Assignment { target, expr, .. } => {
                let value = self.interpret_expr(compiler, function, expr, out)?;
                match target {
                    Expression::Variable { name, .. } => {
                        self.store(&name, value);
                    }
                    _ => todo!("Non variable assignment")
                }
            }
            Statement::Expression { expr, .. } => {
                let value = self.interpret_expr(compiler, function, expr, out)?;
                return Ok(value);
            }
            Statement::Comptime { block, .. } => {
                self.interpret_block(compiler, function, block, out)?;
            }
        }
        Ok(Value::Unit)
    }

    fn interpret_expr(
        &mut self,
        compiler: &Compiler<'input>,
        function: &mut Function,
        expr: Expression<'input>,
        out: &mut Value,
    ) -> Result<Value, InterpreterError> {
        match expr {
            Expression::Variable { name, .. } => {
                let value = self.get(&name).unwrap();
                return Ok(value.clone());
            }
            Expression::ConstantNumber { value, r#type, ..} => {
                let value = match r#type {
                    Type::U8(_) => Value::U8(value.parse().unwrap()),
                    Type::I8(_) => Value::I8(value.parse().unwrap()),
                    Type::U16(_) => Value::U16(value.parse().unwrap()),
                    Type::I16(_) => Value::I16(value.parse().unwrap()),
                    Type::U32(_) => Value::U32(value.parse().unwrap()),
                    Type::I32(_) => Value::I32(value.parse().unwrap()),
                    Type::U64(_) => Value::U64(value.parse().unwrap()),
                    Type::I64(_) => Value::I64(value.parse().unwrap()),
                    Type::F32(_) => Value::F32(value.parse().unwrap()),
                    Type::F64(_) => Value::F64(value.parse().unwrap()),
                    _ => unreachable!()
                };
                return Ok(value);
            }
            Expression::ConstantString { value, .. } => {
                return Ok(Value::String(value.to_string()));
            }
            Expression::ConstantBool { value, .. } => {
                return Ok(Value::Bool(value));
            }
            Expression::Parenthesized { expr, .. } => {
                let value = self.interpret_expr(compiler, function, *expr, out)?;
                return Ok(value);
            }
            Expression::Return { value, .. } => {
                self.should_return = true;
                if let Some(value) = value {
                    let value = self.interpret_expr(compiler, function, *value, out)?;
                    *out = value;
                }
            }
            Expression::IfExpression(if_expr) => {
                let IfExpression {
                    condition,
                    then_block,
                    elifs,
                    else_block,
                    ..
                } = if_expr;

                let condition_value = self.interpret_expr(compiler, function, *condition, out)?;
                if matches!(condition_value, Value::Bool(true)) {
                    let out  = self.interpret_block(compiler, function, then_block, out)?;
                    return Ok(out)
                }
                for (condition, then_block) in elifs {
                    let condition_value = self.interpret_expr(compiler, function, condition, out)?;
                    if matches!(condition_value, Value::Bool(true)) {
                        let out  = self.interpret_block(compiler, function, then_block, out)?;
                        return Ok(out)
                    }
                }
                if let Some(else_block) = else_block {
                    let out = self.interpret_block(compiler, function, then_block, out)?;
                    return Ok(out)
                }
            }
            Expression::FunctionCall { name, args, span, .. } => {
                let args = args.into_iter()
                    .map(|val| self.interpret_expr(compiler, function, val, out))
                    .collect::<Result<Vec<_>, _>>()?;

                let mut path = name.to_vec_strings();
                if let Some(func) = self.internal_functions.get(&path) {
                    let value = self.interpret_internal_function(compiler, function, args, *func, span)?;
                    return Ok(value)
                }
                let name = path.pop().unwrap();
                let def = compiler.get_module_def(&path).unwrap();
                let comptime_function = def.get_comptime_definition(&name).unwrap();

                let out = self.interpret_function(compiler, function, comptime_function.function.clone(), args)?;
                return Ok(out)
            }
        }
        Ok(Value::Unit)
    }
    
    fn interpret_function(
        &mut self,
        compiler: &Compiler<'input>,
        function: &mut Function,
        comptime_function: desugared_tree::Function<'input>,
        parameters: Vec<Value>
    ) -> Result<Value, InterpreterError> {
        let desugared_tree::Function {
            arguments,
            body,
            ..
        } = comptime_function;
        
        let mut out = Value::Unit;
        self.push_state();

        self.push();

        for (argument, parameter) in arguments.iter().zip(parameters.into_iter()) {
            self.store(&argument.name, parameter);
        }
        self.push();

        match self.interpret_block(compiler, function, body, &mut out) {
            Ok(value) => {
                out = value;
            }
            Err(e) => {
                return Err(e);
            }
        }
        self.pop_state();

        Ok(out)
    }

    fn interpret_internal_function(
        &mut self,
        _compiler: &Compiler<'input>,
        function: &mut Function,
        parameters: Vec<Value>,
        internal_function: InternalFunction,
        span: Span
    ) -> Result<Value, InterpreterError> {
        match internal_function {
            InternalFunction::PutInstruction => {
                let value = match &parameters[0] {
                    Value::String(value) => value.to_string(),
                    x => {
                        return Err(InterpreterError::MissMatchedTypes {
                            expected: String::from("String"),
                            found: x.type_name(),
                            span,
                        })
                    }
                };
                match value.as_str() {
                    "i32.add" => {
                        function.instruction(&Instruction::I32Add);
                    }
                    "i32.sub" => {
                        function.instruction(&Instruction::I32Sub);
                    }
                    "i32.mul" => {
                        function.instruction(&Instruction::I32Mul);
                    }
                    "i32.div_s" => {
                        function.instruction(&Instruction::I32DivS);
                    }
                    "i32.rem_s" => {
                        function.instruction(&Instruction::I32RemS);
                    }
                    "i32.div_u" => {
                        function.instruction(&Instruction::I32DivU);
                    }
                    "i32.rem_u" => {
                        function.instruction(&Instruction::I32RemU);
                    }
                    "i32.eq" => {
                        function.instruction(&Instruction::I32Eq);
                    }
                    "i32.ne" => {
                        function.instruction(&Instruction::I32Ne);
                    }
                    "i32.lt_s" => {
                        function.instruction(&Instruction::I32LtS);
                    }
                    "i32.le_s" => {
                        function.instruction(&Instruction::I32LeS);
                    }
                    "i32.gt_s" => {
                        function.instruction(&Instruction::I32GtS);
                    }
                    "i32.ge_s" => {
                        function.instruction(&Instruction::I32GeS);
                    }
                    "i32.lt_u" => {
                        function.instruction(&Instruction::I32LtU);
                    }
                    "i32.le_u" => {
                        function.instruction(&Instruction::I32LeU);
                    }
                    "i32.gt_u" => {
                        function.instruction(&Instruction::I32GtU);
                    }
                    "i32.ge_u" => {
                        function.instruction(&Instruction::I32GeU);
                    }
                    "i64.add" => {
                        function.instruction(&Instruction::I64Add);
                    }
                    "i64.sub" => {
                        function.instruction(&Instruction::I64Sub);
                    }
                    "i64.mul" => {
                        function.instruction(&Instruction::I64Mul);
                    }
                    "i64.div_s" => {
                        function.instruction(&Instruction::I64DivS);
                    }
                    "i64.rem_s" => {
                        function.instruction(&Instruction::I64RemS);
                    }
                    "i64.div_u" => {
                        function.instruction(&Instruction::I64DivU);
                    }
                    "i64.rem_u" => {
                        function.instruction(&Instruction::I64RemU);
                    }
                    "i64.eq" => {
                        function.instruction(&Instruction::I64Eq);
                    }
                    "i64.ne" => {
                        function.instruction(&Instruction::I64Ne);
                    }
                    "i64.lt_s" => {
                        function.instruction(&Instruction::I64LtS);
                    }
                    "i64.le_s" => {
                        function.instruction(&Instruction::I64LeS);
                    }
                    "i64.gt_s" => {
                        function.instruction(&Instruction::I64GtS);
                    }
                    "i64.ge_s" => {
                        function.instruction(&Instruction::I64GeS);
                    }
                    "i64.lt_u" => {
                        function.instruction(&Instruction::I64LtU);
                    }
                    "i64.le_u" => {
                        function.instruction(&Instruction::I64LeU);
                    }
                    "i64.gt_u" => {
                        function.instruction(&Instruction::I64GtU);
                    }
                    "i64.ge_u" => {
                        function.instruction(&Instruction::I64GeU);
                    }
                    "f32.add" => {
                        function.instruction(&Instruction::F32Add);
                    }
                    "f32.sub" => {
                        function.instruction(&Instruction::F32Sub);
                    }
                    "f32.mul" => {
                        function.instruction(&Instruction::F32Mul);
                    }
                    "f32.div" => {
                        function.instruction(&Instruction::F32Div);
                    }
                    "f32.eq" => {
                        function.instruction(&Instruction::F32Eq);
                    }
                    "f32.ne" => {
                        function.instruction(&Instruction::F32Ne);
                    }
                    "f32.lt" => {
                        function.instruction(&Instruction::F32Lt);
                    }
                    "f32.le" => {
                        function.instruction(&Instruction::F32Le);
                    }
                    "f32.gt" => {
                        function.instruction(&Instruction::F32Gt);
                    }
                    "f32.ge" => {
                        function.instruction(&Instruction::F32Ge);
                    }
                    "f64.add" => {
                        function.instruction(&Instruction::F64Add);
                    }
                    "f64.sub" => {
                        function.instruction(&Instruction::F64Sub);
                    }
                    "f64.mul" => {
                        function.instruction(&Instruction::F64Mul);
                    }
                    "f64.div" => {
                        function.instruction(&Instruction::F64Div);
                    }
                    "f64.eq" => {
                        function.instruction(&Instruction::F64Eq);
                    }
                    "f64.ne" => {
                        function.instruction(&Instruction::F64Ne);
                    }
                    "f64.lt" => {
                        function.instruction(&Instruction::F64Lt);
                    }
                    "f64.le" => {
                        function.instruction(&Instruction::F64Le);
                    }
                    "f64.gt" => {
                        function.instruction(&Instruction::F64Gt);
                    }
                    "f64.ge" => {
                        function.instruction(&Instruction::F64Ge);
                    }
                    value => todo!("put instruction: {}", value),
                }
            }
            InternalFunction::UseVariable => {
                let index = match &parameters[0] {
                    Value::VariableHandle(location) => location.get_index(),
                    x => {
                        return Err(InterpreterError::MissMatchedTypes {
                            expected: String::from("VariableHandle<T>"),
                            found: x.type_name(),
                            span,
                        })
                    }
                };
                function.instruction(&Instruction::LocalGet(index));
            }
            InternalFunction::SetVariable => {
                let (index, r#type) = match &parameters[0] {
                    Value::VariableHandle(location) => (location.get_index(), location.get_type()),
                    x => {
                        return Err(InterpreterError::MissMatchedTypes {
                            expected: String::from("VariableHandle<T>"),
                            found: x.type_name(),
                            span,
                        })
                    }
                };
                match (r#type, &parameters[1]) {
                    (compiler::Type::Bool, Value::Bool(val)) => {
                        function.instruction(&Instruction::I32Const(*val as i32));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::U8, Value::U8(val)) => {
                        function.instruction(&Instruction::I32Const(*val as i32));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::I8, Value::I8(val)) => {
                        function.instruction(&Instruction::I32Const(*val as i32));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::U16, Value::U16(val)) => {
                        function.instruction(&Instruction::I32Const(*val as i32));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::I16, Value::I16(val)) => {
                        function.instruction(&Instruction::I32Const(*val as i32));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::U32, Value::U32(val)) => {
                        let val = i32::from_ne_bytes(val.to_ne_bytes());
                        function.instruction(&Instruction::I32Const(val));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::I32, Value::I32(val)) => {
                        function.instruction(&Instruction::I32Const(*val));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::U64, Value::U64(val)) => {
                        let val = i64::from_ne_bytes(val.to_ne_bytes());
                        function.instruction(&Instruction::I64Const(val));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::I64, Value::I64(val)) => {
                        function.instruction(&Instruction::I64Const(*val));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::F32, Value::F32(val)) => {
                        function.instruction(&Instruction::F32Const((*val).into()));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (compiler::Type::F64, Value::F64(val)) => {
                        function.instruction(&Instruction::F64Const((*val).into()));
                        function.instruction(&Instruction::LocalSet(index));
                    }
                    (ty, value) => {
                        return Err(InterpreterError::MissMatchedTypes {
                            expected: ty.name(),
                            found: value.type_name(),
                            span,
                        })
                    }
                }
            }
        }
        Ok(Value::Unit)
    }
}