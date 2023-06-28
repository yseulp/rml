extern crate rml_proc;

use rml_proc::spec;

#[spec("normal", requires(), panics)]
fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn main() {
    println!("{}", add(2, 8));
}
