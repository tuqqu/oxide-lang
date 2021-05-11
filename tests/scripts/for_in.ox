for x in vec[10, 20, 30] {
    dbg(x);
}

for name in vec["John", "Johann", "Jane"] {
    dbg(name);
}

let values = vec[44, 55, 66];
for value in values {
    dbg(value);
}

struct X {
    pub vals: vec<str>
}

let x = X { vals: vec["a", "b", "c", "d"] };
for x in x.vals {
    dbg(x);
}

for x in x.vals {
    if x == "a" {
        continue;
    }
    dbg(x);
}

dbg(x); // value "x" must not be overwritten by the for in loops

for x in x.vals {
    if x == "c" {
        break;
    }
    dbg(x);
}

let i = true;
for i in vec[100, 200, 300, 400] {
    dbg(i);
    if i == 200 {
        for i in vec["hello", "hi", "good day"] {
            dbg(i);
            if i == "hi" {
                loop {
                    break;
                }
                break;
            }
        }
    }
}
dbg(i);

for i, x in vec[10, 20, 30] {
    dbg(i);
    dbg(x);
}

for i, name in vec["John", "Johann", "Jane"] {
    dbg(i as str + ": " + name);
}

let values = vec[44, 55, 66];
for pos, value in values {
    dbg(pos + value);
}

let x = X { vals: vec["a", "b", "c", "d"] };
for i, x in x.vals {
    dbg(x);
}

for i, x in x.vals {
    if i == 2 {
        continue;
    }
    dbg(i as str + x as str);
}

dbg(x); // value "x" must not be overwritten by the for in loops

for i, x in x.vals {
    if i == 1 {
        break;
    }
    dbg(x);
}

let i = true;
for pos, i in vec[100, 200, 300, 400] {
    dbg(i);
    if i == 200 {
        for pos, i in vec["hello", "hi", "good day"] {
            dbg(i);
            if pos == 1 {
                loop {
                    break;
                }
                break;
            }
        }
    }
}
dbg(i);

// ranges

let mut sum = 0;
for x in 0..10 {
    sum += x;
}

dbg(sum);

let mut sum = 0;
for i, x in 0..10 {
    sum += x;
    sum += i;
}

dbg(sum);

let mut sum = 0;
for i, x in 1..=15 {
    sum += x;
    sum += i;
}

dbg(sum);

let mut sum = 0;
for i, x in 1..=15 {
    sum += x;
    sum += i;
}

dbg(sum);



