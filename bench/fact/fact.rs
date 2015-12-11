fn fact(n: u64) -> u64 {
    (1 .. n + 1).fold(1, |acc, n| acc * n)
}

fn main() {
    for _ in 0 .. 620 {
        print!("{}", fact(12));
    }
    println!("");
}
