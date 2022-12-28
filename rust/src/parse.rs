pub use anyhow::format_err;
pub use nom::{
    types::{CompleteByteSlice as Bytes, CompleteStr as Str},
    *,
};

macro_rules! chain {
    ($i:expr, $($arg:tt)*) => {
        do_parse!($i, $($arg)* >> (()))
    };
}

#[macro_export]
macro_rules! uint {
    ($i:expr, $t:ty) => {
        flat_map!($i, digit, parse_to!($t))
    };
}

macro_rules! int {
    ($i:expr, $t:ty) => {
        flat_map!($i, recognize!(chain!(opt!(alt!(char!('-') | char!('+'))) >> digit)), parse_to!($t))
    };
}

#[macro_export]
macro_rules! nom {
    ($result:expr) => {
        match $result {
            Ok((_rest, value)) => Ok(value),
            Err(err) => Err(format_err!("{}", err)),
        }
    };
}
