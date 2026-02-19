use crate::{Path, Span};

pub struct File<'input> {
    pub imports: Vec<Path<'input>>,
    pub functions: Vec<Function<'input>>,
    pub span: Span,
}

pub struct Function<'input> {
    pub public: bool,
    pub comptime: bool,
    pub inline: bool,
    pub kind: FunctionKind<'input>,
    pub arguments: FunctionArguments<'input>,
    pub return_type: Type<'input>,
    pub body: Block<'input>,
    pub span: Span,
}

pub enum FunctionKind<'input> {
    Named {
        name: &'input str,
        span: Span,
    },
    Operator {
        operator: Operator,
        span: Span,
    }
}

pub struct FunctionArguments<'input> {
    pub arguments: Vec<FunctionArgument<'input>>,
    pub span: Span,
}

pub struct FunctionArgument<'input> {
    pub name: &'input str,
    pub r#type: Type<'input>,
    pub span: Span,
}

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

pub struct Block<'input> {
    pub statements: Vec<Statement<'input>>,
    pub span: Span,
}

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
        statements: Vec<Statement<'input>>,
        span: Span,
    }
}

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
    }
}

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
}