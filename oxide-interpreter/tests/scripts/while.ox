let mut x = 0;

while true {
    x += 1;

    if x == 5 {
        break;
    }
}

dbg(x);

let mut y = 0;

while y < 10 {
    if y == 4 {
        y += 1;
        continue;
    }
    y = y + 2;
}

dbg(y);

let mut z = 0;

while z < 1000 {
    while z < 7 {
        z += 1;
        if z > 5 {
            break;
        }
    }

    z = z + 2;

    if z > 15 {
        break;
    }
}

dbg(z);

let mut a = 0;
const D = 2;

while a < 10 {
    a = a + D;
}

dbg(a);
