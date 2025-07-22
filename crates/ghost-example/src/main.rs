#[spec { name = "normal",
    requires(a + b <= i32::MAX),
    requires(a + b >= i32::MIN),
    ensures(result == a + b)
}]
#[strictly_pure]
fn add(a: i32) -> i32 {
    let x = ghost! {1};
    ghost! {
        let y = x;
        proof_assert!(y==1)
    }
    a + 1
}
