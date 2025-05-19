#![feature(rustc_private, register_tool)]

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_ast_pretty;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_hir_pretty;
extern crate rustc_index;
extern crate rustc_interface;
extern crate rustc_macros;
extern crate rustc_middle;
extern crate rustc_serialize;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_target;
extern crate rustc_type_ir;

pub mod callbacks;
pub(crate) mod check;
pub mod ctx;
pub mod error;
pub mod locset;
pub mod spec;
pub mod suppress_borrowck;
pub mod term;
pub mod util;

/// Options for RML.
#[derive(Debug, Clone)]
pub struct Options {
    /// Whether to generate output.
    ///
    /// TODO: Currently unused.
    pub should_output: bool,
    /// Where to write output if any.
    pub output_file: Option<OutputFile>,
    /// Whether RML is run from `cargo rml`.
    pub in_cargo: bool,
    /// Whether to write expanded (i.e., expanded macros) to `stdout`.
    pub print_expanded: bool,
    /// Whether to print the [Debug] output of the collected specifications.
    pub print_specs_debug: bool,
    /// Whether to pretty print the output.
    pub pretty_print: bool,
}

/// Where to write the output.
#[derive(Debug, Clone)]
pub enum OutputFile {
    /// Write to a file at a path.
    File(String),
    /// Standard output.
    Stdout,
}
