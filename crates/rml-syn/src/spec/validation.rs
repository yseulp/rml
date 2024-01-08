use proc_macro2::Span;
use syn::spanned::Spanned;

use crate::{
    visit::{visit_term, Visit, Visitable},
    Spec,
};

pub fn check_final(spec: &Spec) -> Option<Span> {
    let mut c = FinalChecker::default();
    spec.visit(&mut c);
    c.final_outside_demands
}

/// Checks that [final terms] only occur in `demands` spec parts.
///
/// [final terms]: [crate::TermFinal]
#[derive(Default)]
struct FinalChecker {
    final_outside_demands: Option<Span>,
}

impl<'ast> syn::visit::Visit<'ast> for FinalChecker {}

impl<'ast> Visit<'ast> for FinalChecker {
    fn visit_term(&mut self, t: &'ast crate::Term) {
        if self.final_outside_demands.is_some() {
            // Already found error; return early.
            return;
        }
        visit_term(self, t)
    }

    fn visit_spec(&mut self, t: &'ast Spec) {
        for p in &t.pre_conds {
            self.visit_term(p);
        }
        if let Some(l) = &t.modifies {
            self.visit_loc_set(l);
        }
        if let Some(var) = &t.variant {
            self.visit_term(var);
        }
        if let Some(Some(d)) = &t.diverges {
            self.visit_term(d);
        }
        for p in &t.post_conds {
            self.visit_term(p);
        }
    }

    fn visit_term_final(&mut self, t: &'ast crate::TermFinal) {
        self.final_outside_demands = Some(t.span());
    }
}
