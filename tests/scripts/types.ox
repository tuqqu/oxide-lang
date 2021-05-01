let mut x: int = 0;
dbg(x);
dbg(typeof(x));

let w: float = 0.1;
dbg(w);
dbg(typeof(w));

let mut y: str = "string";
dbg(y);
dbg(typeof(y));

let y: num = 100;
dbg(y);
dbg(typeof(y));

let y: nil = nil;
dbg(y);
dbg(typeof(y));

let mut b: bool = true;
b = false;
dbg(b);
dbg(typeof(b));

fn function() {}
let f: fn() = function;
dbg(f);
dbg(typeof(f));

let f: fn() = fn () {};
dbg(f);
dbg(typeof(f));

let f2: fn() = f;
dbg(f2);
dbg(typeof(f2));

let v: vec = vec[];
dbg(v);
dbg(typeof(v));

let v: vec<any> = vec[1, true, nil];
dbg(v);
dbg(typeof(v));

let v: vec<int> = vec[1, 2, 3];
dbg(v);
dbg(typeof(v));

let v: vec<int> = vec<int>[1, 2, 3];
dbg(v);
dbg(typeof(v));

let v: vec<str> = vec["s", "t", "r"];
dbg(v);
dbg(typeof(v));

struct S {}
let s: S = S {};
dbg(s);
dbg(typeof(s));

let v: vec<S> = vec[s, s, S {}];
dbg(v);
dbg(typeof(v));

let v: vec<S> = vec<S>[s, s, S {}];
dbg(v);
dbg(typeof(v));

enum E { A, B }
let mut e: E = E::A;
dbg(e);
dbg(typeof(e));

e = E::B;
dbg(e);

let v: vec<E> = vec[e, E::B, E::B];
dbg(v);
dbg(typeof(v));

let v: vec<E> = vec<E>[E::A, e, e];
dbg(v);
dbg(typeof(v));

let f: fn() = fn () {};
let v: vec<fn()> = vec[fn () {}, fn () {}, f, f];
dbg(v);
dbg(typeof(v));

let v: vec<fn()> = vec<fn()>[fn () {}, fn () {}, f, f];
dbg(v);
dbg(typeof(v));

let mut func: fn(fn(fn(), fn(int))) -> int;
func = fn (f: fn(fn(), fn(int))) -> int {
    return 1;
};

fn function_x(f: fn(fn(), fn(int))) -> int {
    return 1;
}

func = function_x;
dbg(func);
dbg(typeof(func));
