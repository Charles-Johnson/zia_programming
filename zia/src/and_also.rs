pub trait AndAlso<T> {
    fn and_also<'a, 'b>(&'a self, other: &'b Self) -> Option<(&'a T, &'b T)>;
}

impl<T> AndAlso<T> for Option<T> {
    fn and_also<'a, 'b>(&'a self, other: &'b Self) -> Option<(&'a T, &'b T)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }
}
