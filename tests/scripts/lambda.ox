/// lambda returns closure
/// which captures the internal value i
/// each call to the closure increments the captured value
let x = fn () -> fn {
    let mut i = 0;

    fn count() {
        i = i + 1;

        println(i);
    }

    return count;
};

let counter = x();

counter();
counter();
counter();

let y: fn = x;
let counter_2: fn = y();

counter_2();

(fn (x: str) -> nil {
    println(x);
})("hello");

const D = 100;

let y = fn () {
    println(D);
};

y();

let g = fn (x: any, y: num, b: bool) -> str {
    return "string" + x as str;
};

let r: str = g(45, 67.87, false);
println(r);