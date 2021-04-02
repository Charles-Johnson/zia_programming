pub trait MapErrVariant {
    type Error;
    fn map_err_variant(
        self,
        err_variant: &Self::Error,
        on_err: impl FnMut() -> Self,
    ) -> Self;
}

impl<T, E: PartialEq> MapErrVariant for Result<T, E> {
    type Error = E;

    fn map_err_variant(
        mut self,
        err_variant: &Self::Error,
        mut on_err: impl FnMut() -> Self,
    ) -> Self {
        if let Err(e) = &self {
            if e == err_variant {
                self = on_err();
            }
        }
        self
    }
}
