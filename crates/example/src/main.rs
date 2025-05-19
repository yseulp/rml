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
#[strictly_pure]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[spec(name = "b is not zero", requires(b != 0), ensures(result == a / b))]
#[spec(name = "b is zero", requires(b == 0), panics)]
#[pure]
fn div(a: u64, b: u64) -> u64 {
    a / b
}

#[allow(clippy::needless_range_loop)]
#[spec {
    name = "main",
    ensures(exists(| i: usize, j: usize | result[i] == 0)),
    modifies(v[..])
}]
fn all_zero(mut v: Vec<u64>) -> Vec<u64> {
    for i in 0..v.len() {
        v[i] = 0;
    }
    v
}

#[spec(requires(a + b <= i32::MAX), requires(a + b >= i32::MIN), ensures(result == a + b))]
#[logic]
fn no_name(a: i32, b: i32) -> i32 {
    // rec(5);
    a + b
}

#[spec(name = "anchor", requires(n == 0), ensures(result == 0), ensures(no_name(1, 1) != 0))]
#[spec(name = "step", requires(n > 0), variant(n), ensures(result == 0))]
fn rec(n: u128) -> u128 {
    if n == 0 {
        0
    } else {
        rec(n - 1)
    }
}

#[spec(requires(n >= 0), ensures(result == n))]
#[allow(unused)]
fn simple_loop(n: i32) -> i32 {
    let mut i = 0;

    #[invariant(0 <= i)]
    #[invariant(i <= n)]
    #[modifies(nothing)]
    #[variant(n - i)]
    while i < n {
        i += 1;
    }

    i
}

#[invariant(self.0 < 10)]
#[derive(Debug)]
struct Digit(pub u32);

#[invariant(match self {
    Self::Digit(u) => u < 10,
    Self::TenToNineteen(u) => 10 <= u && u < 20
})]
#[derive(Debug)]
enum Number {
    Digit(u32),
    TenToNineteen(u32),
}

#[invariant(self.m() < 10)]
trait T {
    #[strictly_pure]
    fn m(&self) -> u32;
}

// #[extern_spec(std::mem)]
// impl<T> Option<T> {
// #[spec {
// name = "name",
// requires(true),
// ensures[result == match self { Some(_) => true, _ => false }]
// }]
// fn is_some(&self) -> bool;
// }

pub fn main() {
    #[cfg(rml)]
    println!("RML enabled");
    #[cfg(not(rml))]
    println!("RML disabled");
    println!("{}", add(2, 8));
    println!("{}", div(8, 4));
    println!("{:?}", all_zero(vec![1, 2, 3, 4]));
    println!("{}", div(4, 0));
    // println!("{}", no_name(1, 1));
    println!("{}", rec(100));
    println!("{:?}", Digit(8));
    println!("{:?}", Number::TenToNineteen(15));
    println!("{:?}", Number::Digit(5));
}
