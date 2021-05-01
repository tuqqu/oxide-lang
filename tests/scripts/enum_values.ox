enum TimeUnit {
    Seconds,
    Minutes,
    Hours,
    Days,
    Months,
    Years,
}

let secs: TimeUnit = TimeUnit::Seconds;

dbg(secs);
dbg(secs == TimeUnit::Seconds);
dbg(secs != TimeUnit::Minutes);

let mins = TimeUnit::Minutes;

dbg(secs == mins);

let mut secs_2 = TimeUnit::Seconds;

dbg(secs == secs_2);

let secs_3 = secs_2;

dbg(typeof(secs_3));

secs_2 = TimeUnit::Hours;

dbg(secs_3);

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

dbg(plural(secs));
dbg(plural(mins));

enum Ordering {
    Less,
    Equal,
    Greater
}

let all_orders: vec<Ordering> = vec<Ordering>[Ordering::Less, Ordering::Equal];
all_orders.push(Ordering::Greater);

dbg(all_orders);

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

dbg(test.get_ordering());

test.ordering = Ordering::Equal;

dbg(test.ordering);