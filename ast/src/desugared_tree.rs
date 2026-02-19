use crate::{Path, Span};

#[derive(Debug, Clone)]
pub struct File<'input> {
    pub functions: Vec<Function<'input>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function<'input> {
    pub public: bool,
    pub comptime: bool,
    pub inline: bool,
    pub path: Path<'input>,
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

#[derive(Debug, Clone)]
pub struct FunctionArgument<'input> {
    pub lazy: bool,
    pub name: &'input str,
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
    String(Span),
    Void(Span),
    Generic {
        name: &'input str,
        args: Vec<Type<'input>>,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct Block<'input> {
    pub statements: Vec<Statement<'input>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement<'input> {
    Let {
        name: &'input str,
        r#type: Type<'input>,
        expr: Expression<'input>,
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
        name: &'input str,
        span: Span,
    },
    ConstantNumber {
        value: &'input str,
        span: Span,
    },
    ConstantString {
        value: &'input str,
        span: Span,
    },
    FunctionCall {
        name: Path<'input>,
        args: Vec<Expression<'input>>,
        span: Span,
    },
}
