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
        closing_position: Option<()>,
    },
    ClosingParenthesis {
        opening_position: Option<()>,
    },
}
