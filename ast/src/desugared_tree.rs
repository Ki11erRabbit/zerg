use std::borrow::Cow;
use std::path::PathBuf;
use crate::{OwnedPath, Span};

#[derive(Debug, Clone)]
pub struct File {
    pub file_path: PathBuf,
    pub functions: Vec<Function>,
    pub externals: Vec<Extern>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Extern {
    pub library: String,
    pub functions: Vec<Function>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub public: bool,
    pub comptime: bool,
    pub inline: bool,
    pub path: OwnedPath,
    pub type_args: Vec<String>,
    pub arguments: FunctionArguments,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct FunctionArguments {
    pub arguments: Vec<FunctionArgument>,
    pub span: Span,
}

impl FunctionArguments {
    pub fn iter(&self) -> impl Iterator<Item = &FunctionArgument> {
        self.arguments.iter()
    }
    
    pub fn len(&self) -> usize {
        self.arguments.len()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionArgument {
    pub lazy: bool,
    pub name: String,
    pub r#type: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Type {
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
        name: String,
        args: Vec<Type>,
        span: Span,
    },
    Custom {
        name: String,
        span: Span,
    },
    Function {
        args: Vec<Type>,
        r#return: Box<Type>,
        span: Span,
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let {
        name: String,
        r#type: Type,
        expr: Expression,
        span: Span,
    },
    Assignment {
        target: Expression,
        expr: Expression,
        span: Span,
    },
    Expression {
        expr: Expression,
        span: Span,
    },
    Comptime {
        block: Block,
        span: Span,
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    Return {
        value: Option<Box<Expression>>,
        span: Span,
    },
    Parenthesized {
        expr: Box<Expression>,
        span: Span,
    },
    Variable {
        name: String,
        r#type: Type,
        span: Span,
    },
    ConstantNumber {
        value: String,
        r#type: Type,
        span: Span,
    },
    ConstantString {
        value: String,
        span: Span,
    },
    ConstantBool {
        value: bool,
        span: Span,
    },
    FunctionCall {
        name: OwnedPath,
        args: Vec<Expression>,
        return_type: Type,
        function_type: Type,
        span: Span,
    },
    IfExpression(IfExpression),
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_block: Block,
    pub elifs: Vec<(Expression, Block)>,
    pub else_block: Option<Block>,
    pub return_type: Type,
    pub span: Span,
}

impl IfExpression {
    pub fn new(
        condition: Box<Expression>,
        then_block: Block,
        elifs: Vec<(Expression, Block)>,
        else_block: Option<Block>,
        return_type: Type,
        span: Span,
    ) -> Self {
        IfExpression { condition, then_block, elifs, else_block, return_type, span }
    }
}
