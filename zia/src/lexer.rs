pub struct Lexeme {
    pub text: String,
    pub category: Category,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Category {
    Concept(ConceptKind),
    Whitespace,
    OpeningParenthesis {
        closing_position: Option<usize>,
    },
    ClosingParenthesis {
        opening_position: Option<usize>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConceptKind {
    Concrete,
    Abstract,
    New,
    Variable,
}
