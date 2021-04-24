let mut x = 0;
println(typeof(x));

let x;
println(typeof(x));
x = "string";
println(typeof(x));

let w = 0.1;
println(typeof(w));

let mut y = "string";
println(typeof(y));

let y = 100;
println(typeof(y));

let y = nil;
println(typeof(y));

let mut b = true;
println(typeof(b));

fn function() {}
let f = function;
println(typeof(f));

let f = fn () {};
println(typeof(f));

let f2 = f;
println(typeof(f2));

let v = vec[];
println(typeof(v));

let v = vec[1, true, nil];
println(typeof(v));

let v = vec[1, 2, 3];
println(typeof(v));

let v = vec<int>[1, 2, 3];
println(typeof(v));

let v = vec["s", "t", "r"];
println(typeof(v));

struct S {};
let s = S {};
println(typeof(s));

let v = vec[s, s, S {}];
println(typeof(v));

let v = vec<S>[s, s, S {}];
println(typeof(v));

enum E { A, B };
let mut e: E = E::A;
println(typeof(e));

let v = vec[e, E::B, E::B];
println(typeof(v));

let v = vec<E>[E::A, e, e];
println(typeof(v));

let f: fn() = fn () {};
let v = vec[fn () {}, fn () {}, f, f];
println(typeof(v));

let v = vec<fn()>[fn () {}, fn () {}, f, f];
println(typeof(v));

let v;
println(typeof(v));
v = vec[nil, nil];
println(typeof(v));

