let mut x = 1;
let y;
let z;
let a;
let mut b = 20;
let mut c = 30;

{
    let x = 10;
    y = x;

    {
        a = x;
        let mut x = 100;
        x = x - 1;
        {
            c = b;
        }
        {
            z = x;
        }
    }
    b = b - 1;
    let b = 100;
}

dbg(x);
dbg(y);
dbg(z);
dbg(a);
dbg(b);
dbg(c);
