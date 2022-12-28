macro_rules! read_to_string {
    ($path:expr) => {{
        use anyhow::Context;

        std::fs::read_to_string($path).with_context(|| format!("{}", $path))
    }};
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
