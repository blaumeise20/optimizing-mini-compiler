#![feature(let_chains)]
#![feature(decl_macro)]
#![feature(box_patterns)]
#![deny(clippy::useless_attribute)]

use std::env::args;

use cli::{Arguments, CliOptionType};

mod parser;
mod pipeline;
mod ast;
mod cli;
mod lexer;
mod types;
mod cfg;
mod optimizer;
mod memory;

static mut VERBOSE: bool = false;

macro log($($arg:tt)*) {
    if unsafe { VERBOSE } {
        eprintln!($($arg)*);
    }
}

fn main() {
    let args = Arguments::new(
        "optimizing-mini-compiler",
        "An optimizing compiler for the Mini programming language, written as the project for Advanced Compiler Construction at JKU, Linz, Austria.",
    )
        .option("file", "The file to compile.", true)
        .flag("output", "File to write compile results to. Defaults to stdout.", Some('o'), CliOptionType::String { default: Some("") })
        .flag("verbose", "Prints more debug information.", Some('v'), CliOptionType::Boolean)
        .parse(args().skip(1));

    unsafe { VERBOSE = args.get("verbose") != ""; }

    let mut state = pipeline::state();
    log!("Reading file");
    pipeline::read(&mut state, args.get("file"));
    log!("Parsing file contents");
    pipeline::parse(&mut state);
    log!("Typechecking");
    pipeline::typecheck(&mut state);
    log!("Building CFG");
    pipeline::build_cfg(&mut state);
    log!("Optimizing CFG");
    pipeline::optimize(&mut state);
    log!();

    let graph_string = state.cfg.unwrap().to_dot();
    if args.get("output") == "" {
        println!("{graph_string}");
    }
    else {
        std::fs::write(args.get("output"), graph_string).unwrap();
    }
}
