use std::fmt::Display;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl Associativity {
    pub fn display_joint_left(
        self,
        leftleft: impl Display,
        leftright: impl Display,
    ) -> String {
        match &self {
            Self::Left => format!("{leftleft} {leftright}"),
            Self::Right => format!("({leftleft} {leftright})"),
        }
    }

    pub fn display_joint_right(
        self,
        rightleft: impl Display,
        rightright: impl Display,
    ) -> String {
        match &self {
            Self::Left => format!("({rightleft} {rightright})"),
            Self::Right => format!("{rightleft} {rightright}"),
        }
    }

    #[must_use]
    pub fn slice_tokens<'a, T>(
        &self,
        tokens: &'a [T],
        prev_lp_index: Option<usize>,
        lp_index: usize,
    ) -> &'a [T] {
        match &self {
            Self::Left => prev_lp_index
                .map_or_else(|| &tokens[..lp_index], |i| &tokens[i..lp_index]),
            Self::Right => prev_lp_index
                .map_or_else(|| &tokens[lp_index..], |i| &tokens[lp_index..i]),
        }
    }
}
