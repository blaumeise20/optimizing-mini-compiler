use std::{vec::IntoIter, mem};

use crate::{lexer::{Token, TokenKind::{self, *}}, ast};

/// A recursive descent parser for the Mini programming language.
pub struct Parser<'a> {
    tokens: IntoIter<Token<'a>>,
    cur_token: Token<'a>,
    pub errors: Vec<String>,
    __designator_id: usize,
}

impl<'a> Parser<'a> {
    /// Creates a new parser from a list of tokens.
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        let mut iter = tokens.into_iter();
        let cur_token = iter.next().unwrap();
        Parser {
            tokens: iter,
            cur_token,
            errors: vec![],
            __designator_id: 0,
        }
    }

    fn token(&self) -> TokenKind {
        self.cur_token.kind
    }

    fn next(&mut self) -> Token<'a> {
        match self.tokens.next() {
            Some(mut token) => {
                mem::swap(&mut token, &mut self.cur_token);
                token
            },
            None => self.cur_token.clone(),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Token<'a> {
        if self.token() != kind {
            self.errors.push(format!("Expected {:?}, got {:?}", kind, self.token()));
            Token {
                kind,
                text: "",
            }
        }
        else {
            self.next()
        }
    }

    fn consume(&mut self, kind: TokenKind) -> bool {
        if self.token() == kind {
            self.next();
            true
        }
        else {
            false
        }
    }

    /// Parses the tokens into an AST.
    pub fn parse(&mut self) -> ast::Program {
        let program = self.parse_program();
        self.expect(TokenKind::EOF);
        program
    }

    /// Mini = "PROGRAM" { VarDecl } "BEGIN" StatSeq "END" ".";
    fn parse_program(&mut self) -> ast::Program {
        self.expect(K_Program);
        let mut declarations = vec![];
        while self.token() == K_Var {
            declarations.append(&mut self.parse_var_decl());
        }
        self.expect(K_Begin);
        let statements = self.parse_stat_seq();
        self.expect(K_End);
        self.expect(Dot);
        ast::Program {
            declarations,
            statements,
        }
    }

    /// VarDecl = "VAR" { IdListDecl ";" };
    fn parse_var_decl(&mut self) -> Vec<ast::VarDecl> {
        self.expect(K_Var);
        let mut declarations = vec![];
        while self.token() == Identifier {
            declarations.push(self.parse_id_list_decl());
            self.expect(Semicolon);
        }
        declarations
    }

    /// IdListDecl = ident { "," ident } ":" Type;
    fn parse_id_list_decl(&mut self) -> ast::VarDecl {
        let mut names = vec![];
        loop {
            names.push(self.expect(Identifier).text.into());
            if !self.consume(Comma) {
                break;
            }
        }
        self.expect(Colon);
        let ty = self.parse_type();
        ast::VarDecl {
            names,
            ty,
        }
    }

    /// Type = ident | "ARRAY" number "OF" Type;
    fn parse_type(&mut self) -> ast::Type {
        if self.consume(K_Array) {
            let size = self.expect(Number).text.parse().unwrap();
            self.expect(K_Of);
            let ty = self.parse_type();
            ast::Type::Array {
                size,
                ty: Box::new(ty),
            }
        }
        else {
            ast::Type::Simple(self.expect(Identifier).text.into())
        }
    }

    /// StatSeq = Statement { ";" Statement };
    fn parse_stat_seq(&mut self) -> Vec<ast::Statement> {
        let mut statements = vec![];
        loop {
            statements.push(self.parse_statement());
            if !self.consume(Semicolon) {
                break;
            }
        }
        statements
    }

    /// Statement = If | While | Read | Write | Assignment;
    fn parse_statement(&mut self) -> ast::Statement {
        match self.token() {
            K_If => self.parse_if(),
            K_While => self.parse_while(),
            K_Read => self.parse_read(),
            K_Write => self.parse_write(),
            _ => self.parse_assignment(),
        }
    }

    /// If = "IF" Condition "THEN" StatSeq { "ELSIF" Condition "THEN" StatSeq } ["ELSE" StatSeq] "END";
    fn parse_if(&mut self) -> ast::Statement {
        self.expect(K_If);
        let condition = self.parse_condition();
        self.expect(K_Then);
        let then_statements = self.parse_stat_seq();

        let else_statements = self.parse_if_else();

        self.expect(K_End);

        ast::Statement::If {
            condition,
            then_statements,
            else_statements,
        }
    }

    fn parse_if_else(&mut self) -> Vec<ast::Statement> {
        if self.consume(K_Elsif) {
            let condition = self.parse_condition();
            self.expect(K_Then);
            let then_statements = self.parse_stat_seq();
            let else_statements = self.parse_if_else();
            vec![ast::Statement::If {
                condition,
                then_statements,
                else_statements,
            }]
        }
        else if self.consume(K_Else) {
            self.parse_stat_seq()
        }
        else {
            vec![]
        }
    }

    /// While = "WHILE" Condition "DO" StatSeq "END";
    fn parse_while(&mut self) -> ast::Statement {
        self.expect(K_While);
        let condition = self.parse_condition();
        self.expect(K_Do);
        let statements = self.parse_stat_seq();
        self.expect(K_End);
        ast::Statement::While {
            condition,
            statements,
        }
    }

    /// Read = "READ" Designator;
    fn parse_read(&mut self) -> ast::Statement {
        self.expect(K_Read);
        let designator = self.parse_designator();
        ast::Statement::Read(designator)
    }

    /// Write = "WRITE" Expression;
    fn parse_write(&mut self) -> ast::Statement {
        self.expect(K_Write);
        let expression = self.parse_expression();
        ast::Statement::Write(expression)
    }

    /// Assignment = Designator ":=" Expression;
    fn parse_assignment(&mut self) -> ast::Statement {
        let designator = self.parse_designator();
        self.expect(ColonEquals);
        let expression = self.parse_expression();
        ast::Statement::Assignment {
            designator,
            expression,
        }
    }

    /// Condition = Expression ("=" | "#" | "<" | ">" | ">=" | "<=") Expression;
    fn parse_condition(&mut self) -> ast::Condition {
        let left = self.parse_expression();
        let operator = match self.token() {
            Equals => ast::ConditionOperator::Equal,
            Hash => ast::ConditionOperator::NotEqual,
            LessThan => ast::ConditionOperator::LessThan,
            GreaterThan => ast::ConditionOperator::GreaterThan,
            LessThanEquals => ast::ConditionOperator::LessThanOrEqual,
            GreaterThanEquals => ast::ConditionOperator::GreaterThanOrEqual,
            _ => {
                self.errors.push(format!("Expected relop, got {:?}", self.token()));
                ast::ConditionOperator::Equal
            },
        };
        self.next();
        let right = self.parse_expression();
        ast::Condition {
            left,
            operator,
            right,
        }
    }

    /// Designator = ident { "[" Expression "]" };
    fn parse_designator(&mut self) -> ast::Designator {
        let mut designator = ast::Designator::Variable(self.expect(Identifier).text.into());
        while self.consume(LBracket) {
            let index = self.parse_expression();
            self.expect(RBracket);
            designator = ast::Designator::Array {
                designator: Box::new(designator),
                index: Box::new(index),
                __id: self.__designator_id,
            };
            self.__designator_id += 1;
        }
        designator
    }

    /// Expression = ["+" | "-"] Term { ("+" | "-") Term };
    fn parse_expression(&mut self) -> ast::Expression {
        let neg = self.token() == Minus;
        let _ = self.consume(Minus) || self.consume(Plus);
        let mut expr = self.parse_term();
        if neg {
            expr = ast::Expression::Unary {
                operator: ast::UnaryOperator::Negate,
                operand: Box::new(expr),
            };
        }
        while self.token() == Plus || self.token() == Minus {
            let operator = match self.token() {
                Plus => ast::BinaryOperator::Plus,
                Minus => ast::BinaryOperator::Minus,
                _ => unreachable!(),
            };
            self.next();
            let term = self.parse_term();
            expr = ast::Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(term),
            };
        }
        expr
    }

    /// Term = Factor { ("*" | "/" | "%") Factor };
    fn parse_term(&mut self) -> ast::Expression {
        let mut term = self.parse_factor();
        while self.token() == Star || self.token() == Slash || self.token() == Percent {
            let operator = match self.token() {
                Star => ast::BinaryOperator::Times,
                Slash => ast::BinaryOperator::Div,
                Percent => ast::BinaryOperator::Mod,
                _ => unreachable!(),
            };
            self.next();
            let factor = self.parse_factor();
            term = ast::Expression::Binary {
                left: Box::new(term),
                operator,
                right: Box::new(factor),
            };
        }
        term
    }

    /// Factor = Designator | number | "(" Expression ")";
    fn parse_factor(&mut self) -> ast::Expression {
        match self.token() {
            Identifier => ast::Expression::Designator(self.parse_designator()),
            Number => ast::Expression::Constant(self.expect(Number).text.parse().unwrap()),
            LParen => {
                self.next();
                let expr = self.parse_expression();
                self.expect(RParen);
                expr
            }
            _ => {
                self.errors.push(format!("Expected factor, got {:?}", self.token()));
                ast::Expression::Constant(0)
            },
        }
    }
}
