use crate::mixed_concept::ConceptId;

pub struct Lexeme<CI: ConceptId> {
    pub text: String,
    pub category: Category<CI>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Category<CI: ConceptId> {
    Concept(ConceptKind<CI>),
    Whitespace,
    OpeningParenthesis {
        closing_position: Option<usize>,
    },
    ClosingParenthesis {
        opening_position: Option<usize>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum ConceptKind<CI: ConceptId> {
    Concrete {
        id: CI,
    },
    Abstract {
        id: CI,
    },
    New,
    Variable,
}
