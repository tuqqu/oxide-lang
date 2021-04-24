/// recursive function calls to compute n-th
/// fibonacci sequence number

fn fib(n: int) -> int {
    if n <= 1 {
        return n;
    }

    return fib(n - 2) + fib(n - 1);
}

for let mut i = 0; i < 10; i += 1 {
    println(fib(i));
}