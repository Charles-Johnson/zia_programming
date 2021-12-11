pub struct Lexeme {
    pub text: String,
    pub category: Category,
}

#[derive(Debug, PartialEq)]
pub enum Category {
    ConcreteConcept,
    NewConcept,
    Whitespace
}
