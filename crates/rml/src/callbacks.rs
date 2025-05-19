//! Rust compiler callbacks for RML.

use std::{cell::RefCell, fs, thread_local};

use rustc_ast_pretty::pprust::PpAnn;
use rustc_driver::{Callbacks, Compilation};
use rustc_interface::{interface::Compiler, Config};
use rustc_middle::ty::TyCtxt;

use crate::{
    ctx::RmlCtxt, spec::SpecMap, suppress_borrowck::suppress_borrowck, Options, OutputFile,
};

thread_local! {
    static RML_CTXT: RefCell<Option<RmlCtxt<'static>>> = RefCell::new(None);
}

/// Extracts the RML specs.
pub struct ExtractSpec {
    opts: Options,
}

impl ExtractSpec {
    pub fn new(opts: Options) -> Self {
        Self { opts }
    }
}

struct NoAnn;

impl PpAnn for NoAnn {}

impl Callbacks for ExtractSpec {
    fn config(&mut self, config: &mut Config) {
        config.override_queries = Some(|_, providers| {
            providers.mir_built = |tcx, did| {
                let mir = (rustc_interface::DEFAULT_QUERY_PROVIDERS.mir_built)(tcx, did);
                let mut mir = mir.steal();
                suppress_borrowck(tcx, did.to_def_id(), &mut mir);
                tcx.alloc_steal_mir(mir)
            }
        });
    }

    fn after_expansion<'tcx>(&mut self, c: &Compiler, tcx: TyCtxt<'tcx>) -> Compilation {
        c.sess.dcx().abort_if_errors();

        if self.opts.print_expanded {
            // based on the implementation of rustc_driver::pretty::print_after_parsing
            let sess = &c.sess;
            let krate = &tcx.resolver_for_lowering().borrow().1;
            let src_name = sess.io.input.source_name();
            let src = sess
                .source_map()
                .get_source_file(&src_name)
                .expect("get_source_file")
                .src
                .as_ref()
                .expect("src")
                .to_string();
            print!(
                "{}",
                rustc_ast_pretty::pprust::print_crate(
                    sess.source_map(),
                    krate,
                    src_name,
                    src,
                    &NoAnn,
                    false,
                    sess.edition(),
                    &sess.psess.attr_id_generator,
                )
            );
        }

        let mut rcx = RmlCtxt::new(tcx, self.opts.clone());
        rcx.validate();
        unsafe { store_rcx(rcx) };

        c.sess.dcx().abort_if_errors();

        Compilation::Continue
    }

    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &rustc_interface::interface::Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> Compilation {
        let rcx = unsafe { retrieve_rcx(tcx) };
        let specs = rcx.get_specs();
        if self.opts.print_specs_debug {
            println!("== Function/method specs ==");
            for spec in specs.fn_specs.values() {
                println!("{spec:#?}");
            }
            println!("== Struct invariants ==");
            for invs in specs.struct_invs.values() {
                println!("{invs:#?}");
            }
            println!("== Enum invariants ==");
            for invs in specs.enum_invs.values() {
                println!("{invs:#?}");
            }
            println!("== Trait invariants ==");
            for invs in specs.trait_invs.values() {
                println!("{invs:#?}");
            }
            println!("== Loop specs ==");
            for spec in specs.loop_specs.values() {
                println!("{spec:#?}");
            }
        }

        if let Some(of) = &self.opts.output_file {
            output_specs(&specs, of, self.opts.pretty_print);
        }

        Compilation::Continue
    }
}

/// Stores the [`RmlCtxt`] between compiler phases so we can access both AST and
/// HIR where necessary.
///
/// # Safety
///
/// We extend the lifetime of `rcx` to `'static` but this is safe as long as we
/// only retrieve the context (using [`retrieve_rcx`]) with the compiler's
/// `tcx`, which has the correct lifetime `'tcx`.
pub unsafe fn store_rcx(rcx: RmlCtxt<'_>) {
    RML_CTXT.with(|ctx| {
        let rcx = unsafe { std::mem::transmute(rcx) };
        ctx.borrow_mut().replace(rcx)
    });
}

/// Retrieves the stored [`RmlCtxt`] stored in the last compiler phase so we can
/// access both AST and HIR where necessary. Leaves an empty [Option] in its
/// place.
///
/// # Safety
///
/// We constrain the lifetime of `rcx` from `'static` to `'tcx` but this is safe
/// because the context originally had the lifetime `'tcx` when we stored it.
pub unsafe fn retrieve_rcx(_tcx: TyCtxt<'_>) -> RmlCtxt<'_> {
    let rcx: RmlCtxt<'static> = RML_CTXT.with(|ctx| ctx.replace(None).unwrap());
    unsafe { std::mem::transmute(rcx) }
}

/// Write specs to output. Possibly pretty printed.
fn output_specs(specs: &SpecMap, out_file: &OutputFile, pretty_print: bool) {
    let specs = specs.serializable();
    let json = if pretty_print {
        serde_json::to_string_pretty(&specs)
    } else {
        serde_json::to_string(&specs)
    }
    .expect("expected no serialization errors");

    match out_file {
        OutputFile::File(file) => {
            fs::write(file, json).unwrap();
        }
        OutputFile::Stdout => println!("{json}"),
    }
}
