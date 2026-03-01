mod interpreter_state;

use wasm_encoder::Function;
use ast::desugared_tree::{Block, Expression, IfExpression, Statement, Type};
use crate::Compiler;
use crate::interpreter::interpreter_state::{InterpreterState, Value};

#[derive(Debug)]
pub enum InterpreterError {
    Many(Vec<InterpreterError>),
}

pub struct Interpreter<'compiler, 'input> {
    compiler: &'compiler mut Compiler<'input>,
    state: Vec<InterpreterState>,
    should_return: bool,
}

impl<'compiler, 'input> Interpreter<'compiler, 'input> {
    pub fn new(compiler: &'compiler mut Compiler<'input>) -> Self {
        Self {
            compiler,
            state: Vec::new(),
            should_return: false,
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
        function: &mut Function,
        block: Block<'input>,
    ) -> Result<(), InterpreterError> {
        let mut out = Value::Unit;
        self.push_state();
        self.interpret_block(function, block, &mut out)?;
        self.pop_state();
        Ok(())
    }

    fn interpret_block(
        &mut self,
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
            let value = self.interpret_statement(function, stmt, out)?;
            if i == statements_len - 1 {
                out_value = value;
            }
        }
        self.pop();
        Ok(out_value)
    }

    fn interpret_statement(
        &mut self,
        function: &mut Function,
        statement: Statement<'input>,
        out: &mut Value,
    ) -> Result<Value, InterpreterError> {
        match statement {
            Statement::Let { name, expr, .. } => {
                let value = self.interpret_expr(function, expr, out)?;
                self.store(&name, value);
            }
            Statement::Assignment { target, expr, .. } => {
                let value = self.interpret_expr(function, expr, out)?;
                match target {
                    Expression::Variable { name, .. } => {
                        self.store(&name, value);
                    }
                    _ => todo!("Non variable assignment")
                }
            }
            Statement::Expression { expr, .. } => {
                let value = self.interpret_expr(function, expr, out)?;
                return Ok(value);
            }
            Statement::Comptime { block, .. } => {
                self.interpret_block(function, block, out)?;
            }
        }
        Ok(Value::Unit)
    }

    fn interpret_expr(
        &mut self,
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
            Expression::Parenthesized { expr, .. } => {
                let value = self.interpret_expr(function, *expr, out)?;
                return Ok(value);
            }
            Expression::Return { value, .. } => {
                self.should_return = true;
                if let Some(value) = value {
                    let value = self.interpret_expr(function, *value, out)?;
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

                let condition_value = self.interpret_expr(function, *condition, out)?;
                if matches!(condition_value, Value::Bool(true)) {
                    let out  = self.interpret_block(function, then_block, out)?;
                    return Ok(out)
                }
                for (condition, then_block) in elifs {
                    let condition_value = self.interpret_expr(function, condition, out)?;
                    if matches!(condition_value, Value::Bool(true)) {
                        let out  = self.interpret_block(function, then_block, out)?;
                        return Ok(out)
                    }
                }
                if let Some(else_block) = else_block {
                    let out = self.interpret_block(function, then_block, out)?;
                    return Ok(out)
                }
            }
            Expression::FunctionCall { name, args, .. } => {

            }
        }
        Ok(Value::Unit)
    }
}