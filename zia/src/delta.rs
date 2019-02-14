pub trait Delta {
    type Delta;
    fn apply(&mut self, Self::Delta);
}