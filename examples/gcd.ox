/// Compute the greatest common divisor
/// of two integers using Euclid s algorithm.
fn gcd(mut n: int, mut m: int) -> int {
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }

    return n;
}

fn main() {
    println(gcd(15, 5));     // 5
    println(gcd(12, 1456));  // 4
    println(gcd(8, 321));    // 1
}

