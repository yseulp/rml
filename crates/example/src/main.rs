extern crate rml_contracts;

use rml_contracts::*;

#[spec("normal", requires(a + b <= i32::MAX), requires(a + b >= i32::MIN), ensures(result == a + b))]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[spec("b is not zero", requires(b != 0), ensures(result == a / b))]
#[spec("b is zero", requires(b == 0), panics)]
fn div(a: u64, b: u64) -> u64 {
    a / b
}

#[spec("main", ensures(exists(| i: usize, j: usize | result[i] == 0)))]
fn all_zero(mut v: Vec<u64>) -> Vec<u64> {
    for i in 0..v.len() {
        v[i] = 0;
    }
    v
}

#[spec(requires(a + b <= i32::MAX), requires(a + b >= i32::MIN), ensures(result == a + b))]
fn _no_name(a: i32, b: i32) -> i32 {
    a + b
}

pub fn main() {
    #[cfg(rml)]
    println!("RML enabled");
    println!("{}", add(2, 8));
    println!("{}", div(8, 4));
    println!("{:?}", all_zero(vec![1, 2, 3, 4]));
    println!("{}", div(4, 0));
}
