#![feature(rustc_private, register_tool)]

extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_type_ir;

pub mod callbacks;
pub(crate) mod check;
pub(crate) mod ctx;
pub mod error;
pub mod spec;
pub(crate) mod util;

#[derive(Debug, Clone)]
pub struct Options {
    pub should_output: bool,
    pub output_file: Option<OutputFile>,
    pub in_cargo: bool,
}

#[derive(Debug, Clone)]
pub enum OutputFile {
    File(String),
    Stdout,
}
