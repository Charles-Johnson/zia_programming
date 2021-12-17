pub struct Lexeme {
    pub text: String,
    pub category: Category,
}

#[derive(Debug, PartialEq)]
pub enum Category {
    AbstractConcept,
    ConcreteConcept,
    NewConcept,
    Whitespace,
    OpeningParenthesis {
        closing_position: Option<usize>,
    },
    ClosingParenthesis {
        opening_position: Option<usize>,
    },
}
