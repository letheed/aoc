macro_rules! with_context {
    ($context:expr, $e:expr) => {{
        use failure::ResultExt;

        $e.with_context(|err| format!("{}: {}", $context, err))
    }};
}

macro_rules! read_to_string {
    ($path:expr) => {
        with_context!($path, std::fs::read_to_string($path))
    };
}

macro_rules! answer {
    () => {
        return Ok($crate::Answers::None);
    };
    ($res:expr) => {
        return Ok($crate::Answers::One($res.to_string()));
    };
    ($res1:expr, $res2:expr) => {
        return Ok($crate::Answers::Two($res1.to_string(), $res2.to_string()));
    };
}
