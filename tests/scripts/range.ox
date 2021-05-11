let r = 0..10;
dbg(r);

let r: vec<int> = 1..3;
dbg(r);

let r = 0..=10;
dbg(r);

let r: vec<int> = 1..=3;
dbg(r);

const D = 5;
let z = 9;

let r = D..=z;
dbg(r);

let r = z..=z;
dbg(r);

let r = z..=D;
dbg(r);