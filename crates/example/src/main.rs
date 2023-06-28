extern crate rml_proc;

use rml_proc::spec;

#[spec("normal", requires(a + b <= i32::MAX), requires(a + b >= i32::MIN), ensures(result == a + b))]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[spec("b is not zero", requires(b != 0), ensures(result == a / b))]
#[spec("b is zero", requires(b == 0), panics)]
fn div(a: u64, b: u64) -> u64 {
    a / b
}

pub fn main() {
    println!("{}", add(2, 8));
    println!("{}", div(8, 4));
    println!("{}", div(4, 0));
}
