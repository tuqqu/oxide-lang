enum TimeUnit {
    Seconds,
    Minutes,
    Hours,
    Days,
    Months,
    Years,
}

impl TimeUnit {
    pub const SOME_VALUE = "value";

    const PRIVATE_VALUE = 100;

    pub fn default_value() -> Self {
        return Self::Months;
    }

    pub fn plural(time: Self) -> str {
        return match time {
            Self::Seconds => "seconds",
            Self::Minutes => "minutes",
            Self::Hours => "hours",
            Self::Days => "days",
            Self::Months => "months",
            Self::Years => "years"
        };
    }

    pub fn single(time: TimeUnit) -> str {
         return match time {
             TimeUnit::Seconds => "second",
             Self::Minutes => "minute",
             TimeUnit::Hours => "hour",
             TimeUnit::Days => "day",
             TimeUnit::Months => "month",
             TimeUnit::Years => "year"
         };
    }

    pub fn public_method(x: int) -> int {
        return (Self::private_method() + Self::PRIVATE_VALUE) * x;
    }

    fn private_method() -> int {
        let x = 500;
        return x;
    }
}

let secs: TimeUnit = TimeUnit::Seconds;
let secs_str = TimeUnit::plural(secs);
let hours_str = TimeUnit::plural(TimeUnit::Hours);

dbg(secs_str);
dbg(hours_str);

let sec_str = TimeUnit::single(secs);
let hour_str = TimeUnit::single(TimeUnit::Hours);

dbg(sec_str);
dbg(hour_str);

let value = TimeUnit::SOME_VALUE;

dbg(value);

let def = TimeUnit::default_value();

dbg(def);

let public_method_value = TimeUnit::public_method(2);

dbg(public_method_value);
