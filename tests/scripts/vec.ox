let v = vec[];

println(v);

let v = vec[1, 2, 3];

println(v[0]);

let p = v.pop();

println(p);
println(v);

v.push(10);
println(v);

println(v.len());

let v: vec = vec[];
println(v);
println(typeof(v));

let v: vec<int> = vec<int>[1, 2, 3];
println(v);
println(typeof(v));

let v: vec<vec<bool>> = vec[
    vec<bool>[true, false],
    vec<bool>[true, false],
];
println(v);
println(typeof(v));

let v = vec[
    vec<bool>[true],
    vec<bool>[true, false],
];
println(v);
println(typeof(v));

let v = vec[vec[vec[vec[]]]];
v[0][0][0].push("hello");
println(v);
println(typeof(v));

let v = vec<int>[1, 2, 3];
fn change_fist_value(a: vec<int>) {
    a[0] = 100;
}
change_fist_value(v);
println(v);
println(typeof(v));

struct S {}
let v: vec<S> = vec[S {}, S {}];
println(v);
println(typeof(v));

let v = vec<S>[];
println(v);
println(typeof(v));

struct D {
    pub d: int
}
let v = vec[D { d: 1 }, D { d: 2 }];
v[0] = D { d: 10 };
println(v[0]);
v[0].d = 100;
println(v[0]);
println(typeof(v));

enum E { A, B }
let v = vec<E>[];
println(v);
println(typeof(v));

let v: vec<E> = vec[E::A];
println(v);
println(typeof(v));

let v = vec[E::A, E::B];
v[0] = E::B;
println(v[0]);
println(typeof(v));

let v = vec[E::A, E::B];
v[0] = E::B;
println(v[0]);
println(typeof(v));

let v: vec<fn> = vec<fn>[];
println(v);
println(typeof(v));

let v = vec[fn () {}];
println(v);
println(typeof(v));
