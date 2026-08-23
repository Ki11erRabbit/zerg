



pub struct Module {
    pub imports: Vec<Import>,
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
}


pub struct Import {
    pub path: Vec<String>,
}

pub struct FunctionDef {
    pub name: String,
    pub arguments: Vec<FunctionArg>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

pub struct FunctionArg {
    name: String,
    r#type: Type,
}

pub struct Statement {
    pub kind: StatementKind,
}

pub enum StatementKind {
    Expression(Expression),
    Let {
        name: String,
        r#type: Type,
        expr: Expression,
    },
    Assignment {
        target: Expression,
        src: Expression,
    },

}
