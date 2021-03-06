/// lambda returns closure
/// which captures the internal value i
/// each call to the closure increments the captured value
let x = fn () -> fn() {
    let mut i = 0;

    fn count() {
        i = i + 1;

        dbg(i);
    }

    return count;
};

let counter = x();

counter();
counter();
counter();

let y: fn() -> fn() = x;
let counter_2: fn() = y();

counter_2();

(fn (x: str) -> nil {
    dbg(x);
})("hello");

const D = 100;

let y = fn () {
    dbg(D);
};

y();

let g = fn (x: any, y: num, b: bool) -> str {
    return "string" + x as str;
};

let r: str = g(45, 67.87, false);
dbg(r);