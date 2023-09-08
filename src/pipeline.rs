use std::{fs::File, io::Read, process};

use crate::{ast, parser::Parser, lexer::Lexer, types::{typecheck_program, self}, cfg, optimizer};

pub struct State {
    pub code: String,
    pub ast: Option<ast::Program>,
    pub scope: Option<types::Scope>,
    pub cfg: Option<cfg::Graph>,
}

pub fn state() -> State {
    State {
        code: String::new(),
        ast: None,
        scope: None,
        cfg: None,
    }
}

pub fn read(state: &mut State, path: &str) {
    File::open(path)
        .unwrap()
        .read_to_string(&mut state.code)
        .unwrap();
}

pub fn parse(state: &mut State) {
    let mut lexer = Lexer::new(&state.code);
    let tokens = lexer.lex();
    println!("{} lexer errors", lexer.errors);
    let mut parser = Parser::new(tokens);
    state.ast = Some(parser.parse());
    println!("{} parser errors", parser.errors.len());
    for error in &parser.errors {
        println!("  {}", error);
    }
    if !parser.errors.is_empty() {
        process::exit(1);
    }
}

pub fn typecheck(state: &mut State) {
    let result = typecheck_program(state.ast.as_ref().unwrap());
    match result {
        Ok(result) => {
            state.scope = Some(result);
        },
        Err(errors) => {
            println!("{} type errors", errors.len());
            for error in errors {
                println!("  {}", error);
            }
            process::exit(1);
        },
    }
}

pub fn build_cfg(state: &mut State) {
    state.cfg = Some(cfg::build_cfg(state.ast.as_ref().unwrap().clone(), state.scope.as_ref().unwrap()));
}

pub fn optimize(state: &mut State) {
    let cfg = state.cfg.as_mut().unwrap();
    optimizer::optimize(cfg);
}
