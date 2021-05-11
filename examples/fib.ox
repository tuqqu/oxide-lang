/// Recursive function calls to compute n-th
/// fibonacci sequence number

fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }

    return fib(n - 2) + fib(n - 1);
}

fn main() {
    for i in 0..=10 {
        println(fib(i) as str);
    }
}