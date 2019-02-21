use slog;

pub trait Logger {
    fn logger(&mut self) -> &mut slog::Logger;
}