enum TimeUnit {
    Seconds,
    Minutes,
    Hours,
    Days,
    Months,
    Years,
}

let secs: TimeUnit = TimeUnit::Seconds;

println(secs);
println(secs == TimeUnit::Seconds);
println(secs != TimeUnit::Minutes);

let mins = TimeUnit::Minutes;

println(secs == mins);

let mut secs_2 = TimeUnit::Seconds;

println(secs == secs_2);

let secs_3 = secs_2;

println(typeof(secs_3));

secs_2 = TimeUnit::Hours;

println(secs_3);

fn plural(time: TimeUnit) -> str {
    return match time {
        TimeUnit::Seconds => "seconds",
        TimeUnit::Minutes => "minutes",
        TimeUnit::Hours => "hours",
        TimeUnit::Days => "days",
        TimeUnit::Months => "months",
        TimeUnit::Years => "years"
    };
}

println(plural(secs));
println(plural(mins));

enum Ordering {
    Less,
    Equal,
    Greater
}

let all_orders: vec<Ordering> = vec<Ordering>[Ordering::Less, Ordering::Equal];
all_orders.push(Ordering::Greater);

println(all_orders);

struct TestStruct {
    pub ordering: Ordering,
    pub time_units: vec<TimeUnit>
}

impl TestStruct {
    pub fn get_ordering(self) -> Ordering {
        return self.ordering;
    }
}

let test = TestStruct {
    ordering: Ordering::Less,
    time_units: vec[TimeUnit::Hours, TimeUnit::Months]
};

println(test.get_ordering());

test.ordering = Ordering::Equal;

println(test.ordering);