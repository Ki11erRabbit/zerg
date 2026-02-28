use std::borrow::Cow;
use std::path::PathBuf;
use crate::{OwnedPath, Path, Span};

#[derive(Debug, Clone)]
pub struct File<'input> {
    pub file_path: PathBuf,
    pub functions: Vec<Function<'input>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function<'input> {
    pub public: bool,
    pub comptime: bool,
    pub inline: bool,
    pub path: OwnedPath,
    pub type_args: Vec<Cow<'input, str>>,
    pub arguments: FunctionArguments<'input>,
    pub return_type: Type<'input>,
    pub body: Block<'input>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionArguments<'input> {
    pub arguments: Vec<FunctionArgument<'input>>,
    pub span: Span,
}

impl<'input> FunctionArguments<'input> {
    pub fn iter(&self) -> impl Iterator<Item = &FunctionArgument<'input>> {
        self.arguments.iter()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArgument<'input> {
    pub lazy: bool,
    pub name: Cow<'input, str>,
    pub r#type: Type<'input>,
    pub span: Span,
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
    Parenthesized {
        expr: Box<Expression<'input>>,
        span: Span,
    },
    Variable {
        name: Cow<'input, str>,
        r#type: Type<'input>,
        span: Span,
    },
    ConstantNumber {
        value: Cow<'input, str>,
        r#type: Type<'input>,
        span: Span,
    },
    ConstantString {
        value: Cow<'input, str>,
        span: Span,
    },
    FunctionCall {
        name: OwnedPath,
        args: Vec<Expression<'input>>,
        return_type: Type<'input>,
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
    pub return_type: Type<'input>,
    pub span: Span,
}

impl<'input> IfExpression<'input> {
    pub fn new(
        condition: Box<Expression<'input>>,
        then_block: Block<'input>,
        elifs: Vec<(Expression<'input>, Block<'input>)>,
        else_block: Option<Block<'input>>,
        return_type: Type<'input>,
        span: Span,
    ) -> Self {
        IfExpression { condition, then_block, elifs, else_block, return_type, span }
    }
}
