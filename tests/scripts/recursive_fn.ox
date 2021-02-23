fn decr(x: num) {
    println(x);

    if x <= 0 {
        return;
    }

    decr(x - 1);
}

decr(10);

/// fibonacci sequence
fn fib(n: num) -> num {
    if n <= 1 {
        return n;
    }

    return fib(n - 2) + fib(n - 1);
}

for let mut i = 0; i < 15; i = i + 1 {
    println(fib(i));
}
