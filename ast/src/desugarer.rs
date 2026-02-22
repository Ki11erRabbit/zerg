use crate::parse_tree::{Block, Expression, File, Function, IfExpression, Statement, TopLevelStatement};

pub fn desugar(file: File) -> File {
    let File {
        top_level_statements,
        span
    } = file;

    let mut new_top_level_statements = Vec::with_capacity(top_level_statements.len());
    for top_level_statement in top_level_statements.into_iter() {
        new_top_level_statements.push(desugar_top_level_statement(top_level_statement));
    }
    File::new(new_top_level_statements, span)
}


fn desugar_top_level_statement(top_level_statement: TopLevelStatement) -> TopLevelStatement {
    match top_level_statement {
        TopLevelStatement::Import(import) => TopLevelStatement::Import(import),
        TopLevelStatement::ComptimeImport(import) => TopLevelStatement::ComptimeImport(import),
        TopLevelStatement::Function(function) =>  {
            TopLevelStatement::Function(desugar_function(function))
        }
    }
}

fn desugar_function(function: Function) -> Function {
    let Function {
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

    let body = desugar_block(body);

    Function {
        public,
        comptime,
        inline,
        kind,
        type_args,
        arguments,
        return_type,
        body,
        span,
    }
}

fn desugar_block(block: Block) -> Block {
    let Block {
        statements,
        span
    } = block;

    let mut new_statements = Vec::with_capacity(statements.len());
    for statement in statements.into_iter() {
        new_statements.push(desugar_statement(statement));
    }

    Block::new(new_statements, span)
}

fn desugar_statement(statement: Statement) -> Statement {
    match statement {
        Statement::Comptime { block, span } => {
            Statement::Comptime { block: desugar_block(block), span }
        }
        Statement::Assignment { target, expr, span } => {
            let target = desugar_expression(target);
            let expr = desugar_expression(expr);
            Statement::Assignment { target, expr, span }
        }
        Statement::Expression { expr, span } => {
            let expr = desugar_expression(expr);
            Statement::Expression { expr, span }
        }
        Statement::Let { name, r#type, expr, span } => {
            let expr = desugar_expression(expr);
            Statement::Let { name, r#type, expr, span }
        }
    }
}

fn desugar_expression(expr: Expression) -> Expression {
    match expr {
        Expression::Return { value, span } => {
            let value = value.map(|expr| {
                Box::new(desugar_expression(*expr))
            });
            Expression::Return { value, span }
        }
        Expression::BinOp { lhs, rhs, op, span } => {
            let lhs = Box::new(desugar_expression(*lhs));
            let rhs = Box::new(desugar_expression(*rhs));
            Expression::BinOp { lhs, rhs, op, span }
        }
        Expression::Not { value, span } => {
            let value = Box::new(desugar_expression(*value));
            Expression::Not { value, span }
        }
        Expression::Negation { value, span } => {
            let value = Box::new(desugar_expression(*value));
            Expression::Negation { value, span }
        }
        Expression::Parenthesized { expr, span } => {
            let expr = Box::new(desugar_expression(*expr));
            Expression::Parenthesized { expr, span }
        }
        Expression::Variable { name, span } => Expression::Variable { name, span },
        Expression::ConstantNumber { value, span } => Expression::ConstantNumber { value, span },
        Expression::ConstantString { value, span } => Expression::ConstantString { value, span },
        Expression::FunctionCall { name, args, span } => {
            let args = args.into_iter().map(|expr| desugar_expression(expr)).collect();
            Expression::FunctionCall { name, args, span }
        }
        Expression::DottedFunctionCall { base, name, args, span } => {
            let base = desugar_expression(*base);
            let mut new_args = vec![base];
            new_args.extend(args.into_iter().map(|expr| desugar_expression(expr)));
            let args = new_args;
            Expression::FunctionCall { name, args, span }
        }
        Expression::OperatorFunctionCall { operator, args, span } => {
            let args = args.into_iter().map(|expr| desugar_expression(expr)).collect();
            Expression::OperatorFunctionCall { operator, args, span }
        }
        Expression::IfExpression(if_expr) => {
            Expression::IfExpression(desugar_if_expression(if_expr))
        }
    }
}

fn desugar_if_expression(if_expression: IfExpression) -> IfExpression {
    let IfExpression {
        condition,
        then_block,
        elifs,
        else_block,
        span
    } = if_expression;

    let condition = Box::new(desugar_expression(*condition));
    let then_block = desugar_block(then_block);
    let mut new_elifs = Vec::with_capacity(elifs.len());
    for (condition, block) in elifs.into_iter() {
        let condition = desugar_expression(condition);
        let block = desugar_block(block);
        new_elifs.push((condition, block));
    }
    let elifs = new_elifs;

    let else_block = else_block.map(|else_block| {
        desugar_block(else_block)
    });

    IfExpression {
        condition,
        then_block,
        elifs,
        else_block,
        span,
    }
}