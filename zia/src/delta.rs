pub trait Delta {
    type Delta;
    fn apply(&mut self, &Self::Delta);
    fn apply_all(&mut self, deltas: &Vec<Self::Delta>) {
        for delta in deltas {
            self.apply(delta);
        }
    }
}