let mut x: int = 0;
println(x);
println(typeof(x));

let x;
println(typeof(x));

let w: float = 0.1;
println(w);
println(typeof(w));

let mut y: str = "string";
println(y);
println(typeof(y));

let y: num = 100;
println(y);
println(typeof(y));

let y: nil = nil;
println(y);
println(typeof(y));

let mut b: bool = true;
b = false;
println(b);
println(typeof(b));

fn function() {}
let f: fn() = function;
println(f);
println(typeof(f));

let f: fn() = fn () {};
println(f);
println(typeof(f));

let f2: fn() = f;
println(f2);
println(typeof(f2));

let v: vec = vec[];
println(v);
println(typeof(v));

let v: vec<any> = vec[1, true, nil];
println(v);
println(typeof(v));

let v: vec<int> = vec[1, 2, 3];
println(v);
println(typeof(v));

let v: vec<int> = vec<int>[1, 2, 3];
println(v);
println(typeof(v));

let v: vec<str> = vec["s", "t", "r"];
println(v);
println(typeof(v));

struct S {}
let s: S = S {};
println(s);
println(typeof(s));

let v: vec<S> = vec[s, s, S {}];
println(v);
println(typeof(v));

let v: vec<S> = vec<S>[s, s, S {}];
println(v);
println(typeof(v));

enum E { A, B }
let mut e: E = E::A;
println(e);
println(typeof(e));

e = E::B;
println(e);

let v: vec<E> = vec[e, E::B, E::B];
println(v);
println(typeof(v));

let v: vec<E> = vec<E>[E::A, e, e];
println(v);
println(typeof(v));

let f: fn() = fn () {};
let v: vec<fn()> = vec[fn () {}, fn () {}, f, f];
println(v);
println(typeof(v));

let v: vec<fn()> = vec<fn()>[fn () {}, fn () {}, f, f];
println(v);
println(typeof(v));

let mut func: fn(fn(fn(), fn(int))) -> int;
func = fn (f: fn(fn(), fn(int))) -> int {
    return 1;
};

fn function_x(f: fn(fn(), fn(int))) -> int {
    return 1;
}

func = function_x;
println(func);
println(typeof(func));
