use crate::Result;

pub trait OkOrFail {
    type Ok;

    fn ok_or_fail(self, msg: &'static str) -> Result<Self::Ok>;
}

impl<T> OkOrFail for Option<T> {
    type Ok = T;

    fn ok_or_fail(self, msg: &'static str) -> Result<Self::Ok> {
        self.ok_or_else(|| failure::err_msg(msg))
    }
}
