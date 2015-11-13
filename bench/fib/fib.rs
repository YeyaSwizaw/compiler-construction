fn fib(n: u64) -> u64 {
    fn fib_rec(n: u64, acc1: u64, acc2: u64) -> u64 {
        if n == 0 {
            acc2
        } else {
            fib_rec(n - 1, acc1 + acc2, acc1)
        }
    }

    fib_rec(n, 0, 1)
}

fn main() {
    println!("{}", fib(47));
}
