#![feature(rustc_private, register_tool)]

extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_interface;

pub mod callbacks;

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
