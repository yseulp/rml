#![feature(stmt_expr_attributes)]
#![feature(proc_macro_hygiene)]
#![allow(clippy::all)]
#![allow(dead_code)]

extern crate rml_contracts;
use rml_contracts::*;

#[spec { name = "normal",
    requires(a + b <= i32::MAX),
    requires(a + b >= i32::MIN),
    ensures(result == a + b)
}]
fn add(a: i32, b: i32) -> i32 {
    let mut x = ghost! {1};
    ghost! {
        *x = 2;
    };
    a + 1
}

fn main() {}
