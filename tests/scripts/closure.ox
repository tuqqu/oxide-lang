/// function returns closure
/// which captures the internal value i
/// each call to the closure increments the captured value
fn create_counter(start_from: num) -> fn {
    let mut i = start_from;

    fn count() {
        i = i + 1;

        println(i);
    }

    return count;
}

let counter = create_counter(10);

counter();
counter();
counter();

let anon_create_counter: fn = fn (mut i: num) -> fn {
    return fn () {
        i += 1;
        println(i);
    };
};

let counter2 = anon_create_counter(15);

counter2();
counter2();
counter2();