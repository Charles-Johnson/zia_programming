pub trait AndAlso<T> {
    fn and_also<'a, 'b>(&'a self, other: &'b Self) -> Option<(&'a T, &'b T)>;
    fn and_also_mut<'a, 'b>(
        &'a mut self,
        other: &'b mut Self,
    ) -> Option<(&'a mut T, &'b mut T)>;
    fn and_also_move(self, other: Self) -> Option<(T, T)>;
}

impl<T> AndAlso<T> for Option<T> {
    fn and_also<'a, 'b>(&'a self, other: &'b Self) -> Option<(&'a T, &'b T)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }

    fn and_also_mut<'a, 'b>(
        &'a mut self,
        other: &'b mut Self,
    ) -> Option<(&'a mut T, &'b mut T)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }

    fn and_also_move(self, other: Self) -> Option<(T, T)> {
        if let (Some(x), Some(y)) = (self, other) {
            Some((x, y))
        } else {
            None
        }
    }
}
