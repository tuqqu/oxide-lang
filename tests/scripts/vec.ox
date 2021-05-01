let v = vec[];

dbg(v);

let v = vec[1, 2, 3];

dbg(v[0]);

let p = v.pop();

dbg(p);
dbg(v);

v.push(10);
dbg(v);

dbg(v.len());

let v: vec = vec[];
dbg(v);
dbg(typeof(v));

let v: vec<int> = vec<int>[1, 2, 3];
dbg(v);
dbg(typeof(v));

let v: vec<vec<bool>> = vec[
    vec<bool>[true, false],
    vec<bool>[true, false],
];
dbg(v);
dbg(typeof(v));

let v = vec[
    vec<bool>[true],
    vec<bool>[true, false],
];
dbg(v);
dbg(typeof(v));

let v = vec[vec[vec[vec[]]]];
v[0][0][0].push("hello");
dbg(v);
dbg(typeof(v));

let v = vec<int>[1, 2, 3];
fn change_fist_value(a: vec<int>) {
    a[0] = 100;
}
change_fist_value(v);
dbg(v);
dbg(typeof(v));

struct S {}
let v: vec<S> = vec[S {}, S {}];
dbg(v);
dbg(typeof(v));

let v = vec<S>[];
dbg(v);
dbg(typeof(v));

struct D {
    pub d: int
}
let v = vec[D { d: 1 }, D { d: 2 }];
v[0] = D { d: 10 };
dbg(v[0]);
v[0].d = 100;
dbg(v[0]);
dbg(typeof(v));

enum E { A, B }
let v = vec<E>[];
dbg(v);
dbg(typeof(v));

let v: vec<E> = vec[E::A];
dbg(v);
dbg(typeof(v));

let v = vec[E::A, E::B];
v[0] = E::B;
dbg(v[0]);
dbg(typeof(v));

let v = vec[E::A, E::B];
v[0] = E::B;
dbg(v[0]);
dbg(typeof(v));

let v: vec<fn()> = vec<fn()>[];
dbg(v);
dbg(typeof(v));

let v = vec[fn () {}];
dbg(v);
dbg(typeof(v));
