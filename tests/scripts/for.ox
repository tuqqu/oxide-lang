let mut x;
for let mut x0 = 0; x0 < 10; x0 = x0 + 1 {
    x0 = x0 + 1;
    x = x0;
}

dbg(x);

let mut y = 0;
for ; y < 10; y = y + 2 {
    y += 1;
}

dbg(y);

let mut z;
for z = 0; z < 10; z = z + 1 {
}

dbg(z);

let mut a;
for let mut a0 = 0; true; a0 = a0 + 1 {
    if a0 > 5 {
        break;
    }
    a = a0;
}

dbg(a);

let mut b;
for let mut b0 = 0; b0 < 10;  {
    if b0 == 6 {
        b0 = b0 + 1;
        continue;
    }
    b0 = b0 + 2;
    b = b0;
}

dbg(b);

let mut c = 0;
for ;; {
    c = c + 1;
    if c > 5 {
        break;
    }
}

dbg(c);

let mut d = 0;
for ;; {
    for ;; {
        if d > 5 {
            break;
        }
        d = d + 1;
    }

    d = d + 1;
    if d > 10 {
        break;
    }
}

dbg(d);
