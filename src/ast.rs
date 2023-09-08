#[derive(Debug, Clone)]
pub struct Program {
    pub declarations: Vec<VarDecl>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub names: Vec<String>,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Simple(String),
    Array {
        size: usize,
        ty: Box<Type>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    If {
        condition: Condition,
        then_statements: Vec<Statement>,
        else_statements: Vec<Statement>,
    },
    While {
        condition: Condition,
        statements: Vec<Statement>,
    },
    Read(Designator),
    Write(Expression),
    Assignment {
        designator: Designator,
        expression: Expression,
    },
}

#[derive(Debug, Clone)]
pub struct Condition {
    pub left: Expression,
    pub operator: ConditionOperator,
    pub right: Expression,
}

#[derive(Debug, Clone)]
pub enum ConditionOperator {
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum Designator {
    Variable(String),
    Array {
        designator: Box<Designator>,
        index: Box<Expression>,
        __id: usize,
    },
}

#[derive(Debug, Clone)]
pub enum Expression {
    Constant(i32),
    Designator(Designator),
    Unary {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Div,
    Mod,
}
