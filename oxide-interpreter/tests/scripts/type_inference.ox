let mut x = 0;
dbg(typeof(x));

let x;
x = "string";
dbg(typeof(x));

let w = 0.1;
dbg(typeof(w));

let mut y = "string";
dbg(typeof(y));

let y = 100;
dbg(typeof(y));

let y = nil;
dbg(typeof(y));

let mut b = true;
dbg(typeof(b));

fn function() {}
let f = function;
dbg(typeof(f));

let f = fn () {};
dbg(typeof(f));

let f2 = f;
dbg(typeof(f2));

let v = vec[];
dbg(typeof(v));

let v = vec[1, true, nil];
dbg(typeof(v));

let v = vec[1, 2, 3];
dbg(typeof(v));

let v = vec<int>[1, 2, 3];
dbg(typeof(v));

let v = vec["s", "t", "r"];
dbg(typeof(v));

struct S {};
let s = S {};
dbg(typeof(s));

let v = vec[s, s, S {}];
dbg(typeof(v));

let v = vec<S>[s, s, S {}];
dbg(typeof(v));

enum E { A, B };
let mut e: E = E::A;
dbg(typeof(e));

let v = vec[e, E::B, E::B];
dbg(typeof(v));

let v = vec<E>[E::A, e, e];
dbg(typeof(v));

let f: fn() = fn () {};
let v = vec[fn () {}, fn () {}, f, f];
dbg(typeof(v));

let v = vec<fn()>[fn () {}, fn () {}, f, f];
dbg(typeof(v));

let v;
v = vec[nil, nil];
dbg(typeof(v));

