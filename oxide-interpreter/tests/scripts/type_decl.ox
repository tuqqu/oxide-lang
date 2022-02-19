type X = int;
dbg(X);

type Y = int|nil|vec<X>;
dbg(Y);

let x: X = 12;

fn foo_x(x: X) -> X {
    let x: X = 4 + x;
    return x;
}

println(foo_x(10) as str);

let y: Y = vec[4,5,6];
let y: Y = 9;
let y: Y = nil;

fn foo_y(y: Y) -> Y {
    dbg(y);
    return y;
}

foo_y(10);
foo_y(vec[1,2]);
let y = foo_y(nil);
dbg(y);
