use std::borrow::Cow;
use crate::{Path, Span};

#[derive(Debug, Clone)]
pub struct File<'input> {
    pub top_level_statements: Vec<TopLevelStatement<'input>>,
    pub span: Span,
}

impl<'input> File<'input> {
    pub fn new(top_level_statements: Vec<TopLevelStatement<'input>>, span: Span) -> Self {
        File { top_level_statements, span }
    }

    pub fn function_iter(&self) -> impl Iterator<Item = &Function<'input>> {
        self.top_level_statements.iter()
            .filter(|top_level_statement| {
                matches!(top_level_statement, TopLevelStatement::Function(_))
            })
            .map(|top_level_statement| match top_level_statement {
                TopLevelStatement::Function(function) => function,
                _ => unreachable!()
            })
    }
    
    pub fn extern_iter(&self) -> impl Iterator<Item = &Extern<'input>> {
        self.top_level_statements.iter()
            .filter(|top_level_statement| {
                matches!(top_level_statement, TopLevelStatement::Extern(_))
            })
            .map(|top_level_statement| match top_level_statement {
                TopLevelStatement::Extern(r#extern) => r#extern,
                _ => unreachable!()
            })
    }
}

#[derive(Debug, Clone)]
pub enum TopLevelStatement<'input> {
    Import(Path<'input>),
    ComptimeImport(Path<'input>),
    Function(Function<'input>),
    Extern(Extern<'input>),
}

#[derive(Debug, Clone)]
pub struct Extern<'input> {
    pub library: Cow<'input, str>,
    pub functions: Vec<Function<'input>>,
    pub span: Span
}

impl<'input> Extern<'input> {
    pub fn new(library: Cow<'input, str>, functions: Vec<Function<'input>>, span: Span) -> Self {
        Extern { library, functions, span }
    }
}

#[derive(Debug, Clone)]
pub struct Function<'input> {
    pub public: bool,
    pub comptime: bool,
    pub inline: bool,
    pub kind: FunctionKind<'input>,
    pub type_args: Vec<Cow<'input, str>>,
    pub arguments: FunctionArguments<'input>,
    pub return_type: Type<'input>,
    pub body: Block<'input>,
    pub span: Span,
}

impl<'input> Function<'input> {
    pub fn new_comptime(
        public: bool,
        kind: FunctionKind<'input>,
        type_args: Vec<Cow<'input, str>>,
        arguments: FunctionArguments<'input>,
        return_type: Type<'input>,
        body: Block<'input>,
        span: Span,
    ) -> Self {
        Self {
            public,
            comptime: true,
            inline: false,
            kind,
            type_args,
            arguments,
            return_type,
            body,
            span,
        }
    }

    pub fn new_inline(
        public: bool,
        kind: FunctionKind<'input>,
        type_args: Vec<Cow<'input, str>>,
        arguments: FunctionArguments<'input>,
        return_type: Type<'input>,
        body: Block<'input>,
        span: Span,
    ) -> Self {
        Self {
            public,
            comptime: false,
            inline: true,
            kind,
            type_args,
            arguments,
            return_type,
            body,
            span,
        }
    }

    pub fn new_function(
        public: bool,
        kind: FunctionKind<'input>,
        type_args: Vec<Cow<'input, str>>,
        arguments: FunctionArguments<'input>,
        return_type: Type<'input>,
        body: Block<'input>,
        span: Span,
    ) -> Self {
        Self {
            public,
            comptime: false,
            inline: false,
            kind,
            type_args,
            arguments,
            return_type,
            body,
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionKind<'input> {
    Named {
        name: Cow<'input, str>,
        span: Span,
    },
    Operator {
        operator: Operator,
        span: Span,
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArguments<'input> {
    pub arguments: Vec<FunctionArgument<'input>>,
    pub span: Span,
}

impl<'input> FunctionArguments<'input> {
    pub fn new(arguments: Vec<FunctionArgument<'input>>, span: Span) -> Self {
        FunctionArguments { arguments, span }
    }

    pub fn len(&self) -> usize {
        self.arguments.len()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArgument<'input> {
    pub lazy: bool,
    pub name: Cow<'input, str>,
    pub r#type: Type<'input>,
    pub span: Span,
}

impl<'input> FunctionArgument<'input> {
    pub fn new(lazy: bool, name: Cow<'input, str>, r#type: Type<'input>, span: Span) -> Self {
        FunctionArgument { lazy, name, r#type, span }
    }
}

#[derive(Debug, Clone)]
pub enum Type<'input> {
    U8(Span),
    I8(Span),
    U16(Span),
    I16(Span),
    U32(Span),
    I32(Span),
    U64(Span),
    I64(Span),
    F32(Span),
    F64(Span),
    Bool(Span),
    String(Span),
    Unit(Span),
    Generic {
        name: Cow<'input, str>,
        args: Vec<Type<'input>>,
        span: Span,
    },
    Custom {
        name: Cow<'input, str>,
        span: Span,
    },
    Function {
        args: Vec<Type<'input>>,
        r#return: Box<Type<'input>>,
        span: Span,
    }
}

#[derive(Debug, Clone)]
pub struct Block<'input> {
    pub statements: Vec<Statement<'input>>,
    pub span: Span,
}

impl<'input> Block<'input> {
    pub fn new(statements: Vec<Statement<'input>>, span: Span) -> Self {
        Block { statements, span }
    }
}

#[derive(Debug, Clone)]
pub enum Statement<'input> {
    Let {
        name: Cow<'input, str>,
        r#type: Type<'input>,
        expr: Expression<'input>,
        span: Span,
    },
    Assignment {
        target: Expression<'input>,
        expr: Expression<'input>,
        span: Span,
    },
    Expression {
        expr: Expression<'input>,
        span: Span,
    },
    Comptime {
        block: Block<'input>,
        span: Span,
    }
}

#[derive(Debug, Clone)]
pub enum Expression<'input> {
    Return {
        value: Option<Box<Expression<'input>>>,
        span: Span,
    },
    BinOp {
        lhs: Box<Expression<'input>>,
        rhs: Box<Expression<'input>>,
        op: Operator,
        span: Span,
    },
    Not {
        value: Box<Expression<'input>>,
        span: Span,
    },
    Negation {
        value: Box<Expression<'input>>,
        span: Span,
    },
    Parenthesized {
        expr: Box<Expression<'input>>,
        span: Span,
    },
    Variable {
        name: Cow<'input, str>,
        span: Span,
    },
    ConstantNumber {
        value: Cow<'input, str>,
        span: Span,
    },
    ConstantString {
        value: Cow<'input, str>,
        span: Span,
    },
    ConstantBool {
        value: bool,
        span: Span,
    },
    FunctionCall {
        name: Cow<'input, str>,
        args: Vec<Expression<'input>>,
        span: Span,
    },
    DottedFunctionCall {
        base: Box<Expression<'input>>,
        name: Cow<'input, str>,
        args: Vec<Expression<'input>>,
        span: Span,
    },
    OperatorFunctionCall {
        operator: Operator,
        args: Vec<Expression<'input>>,
        span: Span,
    },
    IfExpression(IfExpression<'input>),
}

#[derive(Debug, Clone)]
pub struct IfExpression<'input> {
    pub condition: Box<Expression<'input>>,
    pub then_block: Block<'input>,
    pub elifs: Vec<(Expression<'input>, Block<'input>)>,
    pub else_block: Option<Block<'input>>,
    pub span: Span,
}

impl<'input> IfExpression<'input> {
    pub fn new(
        condition: Box<Expression<'input>>,
        then_block: Block<'input>,
        elifs: Vec<(Expression<'input>, Block<'input>)>,
        else_block: Option<Block<'input>>,
        span: Span,
    ) -> Self {
        IfExpression { condition, then_block, elifs, else_block, span }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    GreaterThanEquals,
    LessThanEquals,
    Or,
    And,
    Not,
}