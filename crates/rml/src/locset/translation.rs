use rustc_hir::{Expr, ExprKind, Path, PathSegment, QPath};
use rustc_middle::hir::map::Map;
use rustc_span::Symbol;

use super::{LocSet, LocSetKind};
use crate::term::{
    translation::{FromHir, HirInto},
    Term, TermQPath,
};

impl<'hir> FromHir<'hir, &'hir Expr<'hir>> for LocSet {
    fn from_hir(value: &'hir Expr<'hir>, hir: Map<'hir>) -> Self {
        Self {
            hir_id: value.hir_id.into(),
            span: value.span.into(),
            kind: value.kind.hir_into(hir),
        }
    }
}

impl<'hir> FromHir<'hir, ExprKind<'hir>> for LocSetKind {
    fn from_hir(value: ExprKind<'hir>, hir: Map<'hir>) -> Self {
        match value {
            ExprKind::Call(
                Expr {
                    kind: ExprKind::Path(QPath::Resolved(None, Path { segments, .. })),
                    ..
                },
                args,
            ) => {
                let f = match get_locset_fn(segments) {
                    Some(f) => f,
                    None => panic!("Expected loc set fn"),
                };

                match f {
                    LocSetFn::Field => {
                        let (base, field) = match &args[0].kind {
                            ExprKind::Field(base, field) => (base, field),
                            _ => panic!(),
                        };
                        let term: Term = (*base).hir_into(hir);
                        LocSetKind::Field(term.into(), (*field).into())
                    }
                    LocSetFn::AllFields => {
                        LocSetKind::FieldWildcard(Box::new((&args[0]).hir_into(hir)))
                    }
                    LocSetFn::Index => LocSetKind::Index(
                        Box::new((&args[0]).hir_into(hir)),
                        Box::new((&args[1]).hir_into(hir)),
                    ),
                    LocSetFn::Path => {
                        let path: TermQPath = match &args[0].kind {
                            ExprKind::Path(q) => q.hir_into(hir),
                            _ => panic!(),
                        };
                        LocSetKind::Path(path)
                    }
                    LocSetFn::Empty => LocSetKind::Nothing,
                    LocSetFn::Union => {
                        let left: LocSet = (&args[0]).hir_into(hir);
                        let right: LocSet = (&args[1]).hir_into(hir);
                        let locsets: Vec<LocSet> = vec![left, right]
                            .into_iter()
                            .flat_map(|l| match l.kind {
                                LocSetKind::Group(ls) => ls,
                                k => vec![LocSet {
                                    hir_id: l.hir_id,
                                    span: l.span,
                                    kind: k,
                                }],
                            })
                            .collect();

                        LocSetKind::Group(locsets)
                    }
                }
            }
            _ => unreachable!("Cannot translate the expression to a loc set"),
        }
    }
}

enum LocSetFn {
    Field,
    AllFields,
    Index,
    Path,
    Empty,
    Union,
}

fn get_locset_fn(segments: &[PathSegment]) -> Option<LocSetFn> {
    if !(segments.len() == 5
        && segments[0].ident.name == Symbol::intern("{{root}}")
        && segments[1].ident.name == Symbol::intern("rml_contracts")
        && segments[2].ident.name == Symbol::intern("logic")
        && segments[3].ident.name == Symbol::intern("Logic"))
    {
        return None;
    }

    let f = match segments[4].ident.as_str() {
        "field" => LocSetFn::Field,
        "all_fields" => LocSetFn::AllFields,
        "index" => LocSetFn::Index,
        "path" => LocSetFn::Path,
        "empty" => LocSetFn::Empty,
        "union" => LocSetFn::Union,
        n => panic!("Unknown function {n}"),
    };

    Some(f)
}
