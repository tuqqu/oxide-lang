let mut x = 10;
if true {
    x += 1;
}

dbg(x);

let mut y = 10;
if y == 10 {
    let y = y + 1;
}

dbg(y);

let mut z = 10;
if z == (5 + 5) && 45 != 9 || "hello" == "he" + "l" + "lo" {
    z += 1;
}

dbg(z);

let mut a = 10;
if "hello" != "he" + "l" + "lo" {
    a += 1;
}

dbg(a);

let mut b = 10;
if false {
    b = b + 1;
} else {
    b = b - 1;
}

dbg(b);

let mut c = 10;
if false {
    c = c + 1;
} else {
    if true {
        c = 100;
    } else {
        c = -10;
    }
}

dbg(c);

let mut d = 10;
if false {
    d = 20;
} else if false {
    d = 30;
}

dbg(d);

let mut e = 10;
if false {
    e = 20;
} else if true {
    e = 30;
} else {
    e = 40;
}

dbg(e);

let mut f = 10;
if false {
    f = 20;
} else if true && true && !false {
    f = 30;
} else if true {
    f = 40;
}

dbg(f);

let mut j = 10;
if j == 10 {
    j = 20;
} else if true && true && !false {
    j = 30;
}

dbg(j);

let mut i = 10;
if i == 11 {
    i = 20;
} else if i == 12 {
    i = 30;
} else {
    i = 13;
}

dbg(i);
