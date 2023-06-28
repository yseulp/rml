extern crate rml_proc;

use rml_proc::spec;

#[spec("normal", requires(a + b <= i32::MAX), requires(a + b >= i32::MIN), ensures(result == a + b))]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn main() {
    println!("{}", add(2, 8));
}
