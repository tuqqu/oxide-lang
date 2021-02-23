let mut x = 0;

loop {
    x += 1;

    if x == 5 {
        break;
    }
}

println(x);

let mut y = 0;

loop {
    if y == 4 {
        y += 1;
        continue;
    }
    y = y + 2;

    if y > 10 {
        break;
    }
}

println(y);

let mut z = 0;

loop {
    loop {
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

println(z);
