#![feature(rustc_private, register_tool)]

use rustc_middle::ty::TyCtxt;

extern crate rustc_abi;
extern crate rustc_ast;
extern crate rustc_ast_ir;
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
pub mod hir;
pub mod locset;
pub mod spec;
pub mod suppress_borrowck;
pub mod term;
pub mod util;

/// Allows translating from `T` to `Self`, where `T` is a HIR structure. Since
/// some structures reference bodies, we require access to the HIR.
pub trait FromHir<'hir, T>
where
    T: Sized,
{
    /// Translate from `value` to `Self`, where `T` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `hir`.
    fn from_hir(value: T, tcx: TyCtxt<'hir>) -> Self;
}

/// Allows translating from `Self` to `T`, where `Self` is a HIR structure.
/// Since some structures reference bodies, we require access to the HIR.
///
/// **Do not implement this directly.** Use [FromHir] instead.
pub trait HirInto<'hir, T>
where
    T: Sized,
{
    /// Translate from `self` to `T`, where `self` is a HIR structure. Since
    /// some structures reference bodies, we require access to the HIR via
    /// `hir`.
    fn hir_into(self, tcx: TyCtxt<'hir>) -> T;
}

impl<'hir, T, U> HirInto<'hir, U> for T
where
    U: FromHir<'hir, T>,
{
    fn hir_into(self, tcx: TyCtxt<'hir>) -> U {
        U::from_hir(self, tcx)
    }
}

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
