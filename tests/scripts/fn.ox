/// argless
fn func_x() {
    println("hello");
}

func_x();

/// one argument
fn func_y(x: any) {
    println(x);
}
const D = 10;

func_y(D);

/// multiargument
/// inner shadowing
/// inner const declaring
/// inner function declaration
fn func_z(x: any, mut y: num, z: bool, e: float, i: any) {
    let x = 100;
    println(x); // shadowed

    y += 10; // mutable
    println(y);
    
    const D = "hello";
    println(D);
    
    fn inner_one(s: str) {
        println(s + " world");
    }
    
    inner_one(D);
}

func_z("hi", 9, true, 45.56, nil);

/// return value
fn func_a(a: num) -> num {
    return a + 10;
}

println(func_a(10));

/// accepts closure and calls it
fn func_b(closure: func) {
    closure(10);
}

fn closure(x: num) -> num {
    return x + 100;
}

let value = func_b(closure);

println(value);

/// inner loop
fn func_c(mut x: num) {
    while x > 10 {
        println(x);
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

println(r);