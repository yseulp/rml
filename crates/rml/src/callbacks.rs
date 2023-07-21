use rustc_driver::{Callbacks, Compilation};
use rustc_interface::{interface::Compiler, Config, Queries};
use rustc_middle::ty::TyCtxt;

use std::{cell::RefCell, thread_local};

use crate::{ctx::RmlCtxt, Options};

thread_local! {
    static RML_CTXT: RefCell<Option<RmlCtxt<'static>>> = RefCell::new(None);
}

pub struct ExtractSpec {
    opts: Options,
}

impl ExtractSpec {
    pub fn new(opts: Options) -> Self {
        Self { opts }
    }
}

impl Callbacks for ExtractSpec {
    fn config(&mut self, config: &mut Config) {
        config.override_queries = Some(|_, providers, _| {
            providers.mir_built = |tcx, did| {
                let mir = (rustc_interface::DEFAULT_QUERY_PROVIDERS.mir_built)(tcx, did);
                let mir = mir.steal();
                tcx.alloc_steal_mir(mir)
            }
        });
    }

    fn after_expansion<'tcx>(&mut self, c: &Compiler, queries: &'tcx Queries<'tcx>) -> Compilation {
        c.session().abort_if_errors();

        // based on the implementation of rustc_driver::pretty::print_after_parsing
        queries.global_ctxt().unwrap().enter(|tcx| {
            let sess = c.session();
            let krate = &tcx.resolver_for_lowering(()).borrow().1;
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
                    &rustc_ast_pretty::pprust::state::NoAnn,
                    false,
                    sess.edition(),
                    &sess.parse_sess.attr_id_generator,
                )
            );
        });

        queries.global_ctxt().unwrap().enter(|tcx| {
            let mut rcx = RmlCtxt::new(tcx, self.opts.clone());
            rcx.validate();
            unsafe { store_rcx(rcx) };
        });

        c.session().abort_if_errors();

        Compilation::Continue
    }

    fn after_analysis<'tcx>(
        &mut self,
        _handler: &rustc_session::EarlyErrorHandler,
        _compiler: &rustc_interface::interface::Compiler,
        queries: &'tcx Queries<'tcx>,
    ) -> Compilation {
        queries.global_ctxt().unwrap().enter(|tcx| {
            let rcx = unsafe { retrieve_rcx(tcx) };
            let specs = rcx.get_specs();
            for spec in specs.0.values() {
                println!("{:#?}", spec)
            }
        });

        Compilation::Continue
    }
}

pub unsafe fn store_rcx<'tcx>(rcx: RmlCtxt<'tcx>) {
    RML_CTXT.with(|ctx| {
        let rcx = unsafe { std::mem::transmute(rcx) };
        ctx.borrow_mut().replace(rcx)
    });
}

pub unsafe fn retrieve_rcx<'tcx>(_tcx: TyCtxt<'tcx>) -> RmlCtxt<'tcx> {
    let rcx: RmlCtxt<'static> = RML_CTXT.with(|ctx| ctx.replace(None).unwrap());
    unsafe { std::mem::transmute(rcx) }
}
