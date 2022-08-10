use std::fmt::Display;

#[derive(Copy, Clone, Debug, PartialEq)]
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
            Self::Left => format!("{} {}", leftleft, leftright),
            Self::Right => format!("({} {})", leftleft, leftright),
        }
    }

    pub fn display_joint_right(
        self,
        rightleft: impl Display,
        rightright: impl Display,
    ) -> String {
        match &self {
            Self::Left => format!("({} {})", rightleft, rightright),
            Self::Right => format!("{} {}", rightleft, rightright),
        }
    }

    pub fn slice_tokens<'a>(
        &self,
        tokens: &'a [String],
        prev_lp_index: Option<usize>,
        lp_index: usize,
    ) -> &'a [String] {
        match &self {
            Self::Left => match prev_lp_index {
                Some(i) => &tokens[i..lp_index],
                None => &tokens[..lp_index],
            },
            Self::Right => match prev_lp_index {
                Some(i) => &tokens[lp_index..i],
                None => &tokens[lp_index..],
            },
        }
    }
}
