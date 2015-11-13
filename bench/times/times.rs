fn add_five(x: i64) -> i64 {
    x + 5
}

fn times(n: i64, f: fn(i64) -> i64, x: i64) -> i64 {
    let mut val = x;
    for _ in (0 .. n) {
        val = f(val)
    }
    val
}

fn main() {
    println!("{}", times(100000, add_five, 0));
}
