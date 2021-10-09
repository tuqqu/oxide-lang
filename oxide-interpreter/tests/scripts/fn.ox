/// argless
fn func_x() {
    dbg("hello");
}

func_x();

/// one argument
fn func_y(x: int) {
    dbg(x);
}
const D = 10;

func_y(D);

/// multiargument
/// inner shadowing
/// inner const declaring
/// inner function declaration
fn func_z(x: any, mut y: num, z: bool, e: float, i: any) {
    let x = 100;
    dbg(x); // shadowed

    y += 10; // mutable
    dbg(y);
    
    const D = "hello";
    dbg(D);
    
    fn inner_one(s: str) {
        dbg(s + " world");
    }
    
    inner_one(D);
}

func_z("hi", 9, true, 45.56, nil);

/// return value
fn func_a(a: int) -> int {
    return a + 10;
}

dbg(func_a(10));

/// accepts closure of type fn(int) and calls it
fn func_b(closure: fn(int) -> int) {
    closure(10);
}

fn closure(x: int) -> int {
    return x + 100;
}

let value = func_b(closure);

dbg(value);

/// inner loop
fn func_c(mut x: int) {
    while x > 10 {
        dbg(x);
        x = x - 1;

        if x == 5 {
            break;
        }
    }
}

func_c(15);

let mut r = 100;
/// unpure, changes value outside
fn func_d() {
    r = 200;
}

func_d();

dbg(r);