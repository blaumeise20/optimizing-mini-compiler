use std::collections::HashMap;

use crate::ast;

/// A collection of "items" in a specific scope. An item can be either a
/// variable or a type definition. Every scope can also have an optional parent
/// scope.
#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub items: HashMap<String, (Type, ItemKind)>,
    pub stack: Vec<(String, Type, usize)>,
    current_stack_offset: usize,
    pub __designators: HashMap<usize, usize>,
}

impl Scope {
    pub fn new(parent: Scope) -> Self {
        Self {
            parent: Some(Box::new(parent)),
            items: HashMap::new(),
            stack: vec![],
            current_stack_offset: 0,
            __designators: HashMap::new(),
        }
    }

    pub fn new_empty() -> Self {
        Self {
            parent: None,
            items: HashMap::new(),
            stack: vec![],
            current_stack_offset: 0,
            __designators: HashMap::new(),
        }
    }

    fn put_stack(&mut self, name: String, ty: Type) {
        self.current_stack_offset += ty.size();
        self.stack.push((name, ty, self.current_stack_offset));
    }

    pub fn create_universe() -> Self {
        let mut universe = Self::new_empty();
        universe.items.insert("INTEGER".to_string(), (Type::Integer, ItemKind::Type));
        universe
    }

    pub fn get(&self, name: &str) -> Option<&(Type, ItemKind)> {
        self.items.get(name).or_else(|| self.parent.as_ref().and_then(|parent| parent.get(name)))
    }
}

/// Type of an item stored inside a scope.
#[derive(Debug, Clone)]
pub enum ItemKind {
    Variable,
    Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Integer,
    Array {
        size: usize,
        ty: Box<Type>,
    },
    Unresolved(String),
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Integer => 4,
            Type::Array { size, ty } => size * ty.size(),
            Type::Unresolved(_) => panic!("cannot get size of unresolved type"),
        }
    }

    pub fn is(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Integer, Type::Integer) => true,
            (Type::Array { size: size1, ty: ty1 }, Type::Array { size: size2, ty: ty2 }) => size1 == size2 && ty1.is(ty2),
            (Type::Unresolved(_), _) => true,
            (_, Type::Unresolved(_)) => true,
            _ => false,
        }
    }
}

pub fn typecheck_program(program: &ast::Program) -> Result<Scope, Vec<String>> {
    let mut checker = Checker::new();
    checker.check_program(program);
    if checker.errors.is_empty() { Ok(checker.scope) }
    else { Err(checker.errors) }
}


struct Checker {
    scope: Scope,
    errors: Vec<String>,
}

macro report($self:ident $($arg:tt)*) {
    $self.errors.push(format!($($arg)*));
}

impl Checker {
    fn new() -> Self {
        Self {
            scope: Scope::create_universe(),
            errors: vec![],
        }
    }

    fn check_program(&mut self, program: &ast::Program) {
        self.scope = Scope::new(self.scope.clone());

        // load variables into scope
        for decl in &program.declarations {
            for name in &decl.names {
                if self.scope.get(name).is_some() {
                    report!(self "'{name}' already declared");
                }
                else {
                    let ty = self.check_type(&decl.ty);
                    if let Type::Array { .. } = ty {
                        self.scope.put_stack(name.clone(), ty.clone());
                    }
                    self.scope.items.insert(name.clone(), (ty, ItemKind::Variable));
                }
            }
        }

        // check statements
        for stmt in &program.statements {
            self.check_statement(stmt);
        }
    }

    fn check_type(&mut self, ty: &ast::Type) -> Type {
        match ty {
            ast::Type::Simple(name) => {
                match self.scope.get(name) {
                    Some((ty, ItemKind::Type)) => ty.clone(),
                    Some(_) => {
                        report!(self "'{name}' is not a type");
                        Type::Unresolved(name.clone())
                    },
                    None => {
                        report!(self "unknown type '{name}'");
                        Type::Unresolved(name.clone())
                    },
                }
            },
            ast::Type::Array { size, ty } => {
                Type::Array {
                    size: *size,
                    ty: Box::new(self.check_type(ty)),
                }
            },
        }
    }

    fn check_statement(&mut self, stmt: &ast::Statement) {
        match stmt {
            ast::Statement::If {
                condition,
                then_statements,
                else_statements,
            } => {
                self.check_condition(condition);
                for stmt in then_statements {
                    self.check_statement(stmt);
                }
                for stmt in else_statements {
                    self.check_statement(stmt);
                }
            },
            ast::Statement::While {
                condition,
                statements,
            } => {
                self.check_condition(condition);
                for stmt in statements {
                    self.check_statement(stmt);
                }
            },
            ast::Statement::Read(designator) => {
                let ty = self.check_designator(designator);
                if !ty.is(&Type::Integer) {
                    report!(self "cannot read into non-integer");
                }
            },
            ast::Statement::Write(expression) => {
                let ty = self.check_expression(expression);
                if !ty.is(&Type::Integer) {
                    report!(self "cannot write non-integer");
                }
            },
            ast::Statement::Assignment {
                designator,
                expression,
            } => {
                let ty_designator = self.check_designator(designator);
                let ty_expression = self.check_expression(expression);
                if !ty_designator.is(&Type::Integer) {
                    report!(self "cannot assign to non-integer");
                }
                else if !ty_expression.is(&Type::Integer) {
                    report!(self "cannot assign non-integer");
                }
            },
        }
    }

    fn check_condition(&mut self, condition: &ast::Condition) {
        let ty_left = self.check_expression(&condition.left);
        let ty_right = self.check_expression(&condition.right);
        if !ty_left.is(&Type::Integer) || !ty_right.is(&Type::Integer) {
            report!(self "cannot compare non-integers");
        }
    }

    fn check_designator(&mut self, designator: &ast::Designator) -> Type {
        match designator {
            ast::Designator::Variable(name) => {
                match self.scope.get(name) {
                    Some((ty, ItemKind::Variable)) => ty.clone(),
                    Some(_) => {
                        report!(self "'{name}' is not a variable");
                        Type::Unresolved(name.into())
                    },
                    None => {
                        report!(self "unknown variable '{name}'");
                        Type::Unresolved(name.into())
                    },
                }
            },
            ast::Designator::Array {
                box designator,
                box index,
                __id,
            } => {
                let ty_designator = self.check_designator(designator);
                let ty_index = self.check_expression(index);
                if !ty_index.is(&Type::Integer) {
                    report!(self "cannot index with non-integer");
                }
                match ty_designator {
                    Type::Array { ty, .. } => {
                        self.scope.__designators.insert(*__id, ty.size());
                        *ty
                    },
                    Type::Unresolved(_) => Type::Unresolved("".to_string()),
                    _ => {
                        report!(self "cannot index non-array");
                        Type::Unresolved("".to_string())
                    },
                }
            },
        }
    }

    fn check_expression(&mut self, expression: &ast::Expression) -> Type {
        match expression {
            ast::Expression::Constant(_) => Type::Integer,
            ast::Expression::Designator(designator) => self.check_designator(designator),
            ast::Expression::Binary {
                box left,
                box right,
                ..
            } => {
                let ty_left = self.check_expression(left);
                let ty_right = self.check_expression(right);
                if !ty_left.is(&Type::Integer) || !ty_right.is(&Type::Integer) {
                    report!(self "cannot operate on non-integers");
                }
                Type::Integer
            },
            ast::Expression::Unary {
                operator,
                box operand,
            } => {
                let ty_operand = self.check_expression(operand);
                if !ty_operand.is(&Type::Integer) {
                    report!(self "cannot operate on non-integers");
                }
                match operator {
                    ast::UnaryOperator::Negate => Type::Integer,
                }
            },
        }
    }
}
