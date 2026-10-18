use crate::{concepts::ConcreteConceptType, mixed_concept::ConceptId};

#[derive(Debug)]
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
        concrete_type: ConcreteConceptType,
    },
    Abstract {
        id: CI,
    },
    New,
    Variable,
}
