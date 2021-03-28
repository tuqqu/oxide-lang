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

struct D {};
let v: vec<D> = vec[D {}, D{}];
println(v);
println(typeof(v));

let v = vec<int>[1, 2, 3];
fn change_fist_value(a: vec<int>) {
    a[0] = 100;
}
change_fist_value(v);
println(v);
println(typeof(v));
