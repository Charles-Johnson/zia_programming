pub trait AndAlso<T, U> {
    fn and_also<'a, 'b>(
        &'a self,
        other: &'b Option<U>,
    ) -> Option<(&'a T, &'b U)>;
    fn and_also_move(self, other: Option<U>) -> Option<(T, U)>;
}

impl<T, U> AndAlso<T, U> for Option<T> {
    fn and_also<'a, 'b>(
        &'a self,
        other: &'b Option<U>,
    ) -> Option<(&'a T, &'b U)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }

    fn and_also_move(self, other: Option<U>) -> Option<(T, U)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }
}
