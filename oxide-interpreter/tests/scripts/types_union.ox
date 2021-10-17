let mut x: int | nil = 0;
dbg(x);
dbg(typeof(x));

let mut w: float | int = 0.1;
dbg(w);
dbg(typeof(w));
w = 9;
dbg(w);
dbg(typeof(w));

let mut y: str | int | nil | bool = "string";
dbg(y);
dbg(typeof(y));
y = true;
dbg(y);
dbg(typeof(y));
y = nil;
dbg(y);
dbg(typeof(y));
y = 98;
dbg(y);
dbg(typeof(y));

let y: int | str = 100;
dbg(y);
dbg(typeof(y));

enum E { A, B }
enum X { A, B }
let mut e: E | X = E::A;
dbg(e);
dbg(typeof(e));

e = X::A;
dbg(e);
dbg(typeof(e));

struct Y {}
struct Z {}

fn f(x: Y | Z | nil) -> Y | Z | nil | int {
    if x == nil {
        println("nil passed");

        return 56;
    } else if typeof(x) == "Y" {
        println("Y passed");

        return Z {};
    } else if typeof(x) == "Z" {
        println("Z passed");

        return Y {};
    }

    return nil;
}

let x = f(Y{});
dbg(x);
dbg(typeof(x));
let x = f(Z{});
dbg(x);
dbg(typeof(x));
let x = f(nil);
dbg(x);
dbg(typeof(x));



impl Y {
    pub fn f(self, other: Self | Z) -> vec<str> | Self {
        if typeof(x) == "Z" {
            println("Z passed");
            return vec["hello"];
        }

        return Self {};
    }
}

let y = Y {};
dbg(y.f(y));
dbg(y.f(Z{}));