use proptest::prelude::*;
use zia::{multi_threaded::NEW_CONTEXT, LexemeCategory};

proptest! {
    #[test]
    fn lexer_preserves_text(command in "\\PC*") {
        let cont = NEW_CONTEXT.clone();
        let reconstructed_text = cont.lex(&command)
            .into_iter()
            .fold(String::new(), |reconstructed_text, lexeme| reconstructed_text + &lexeme.text);
        assert_eq!(reconstructed_text, command);
    }
}

#[test]
fn lexer_indentifies_new_concept() {
    let cont = NEW_CONTEXT.clone();
    let lexemes = cont.lex("new_concept");
    assert_eq!(lexemes.len(), 1);
    assert_eq!(lexemes.first().unwrap().category, LexemeCategory::NewConcept);
}

proptest! {
    #[test]
    fn lexer_indentifies_whitespace(w in r"\s") {
        let cont = NEW_CONTEXT.clone();
        let lexemes = cont.lex(&format!("new_concept{}another_new_concept", w));
        assert_eq!(lexemes.len(), 3);
        assert_eq!(lexemes[0].text, "new_concept");
        assert_eq!(lexemes[0].category, LexemeCategory::NewConcept);

        assert_eq!(lexemes[1].text, w);
        assert_eq!(lexemes[1].category, LexemeCategory::Whitespace);

        assert_eq!(lexemes[2].text, "another_new_concept");
        assert_eq!(lexemes[2].category, LexemeCategory::NewConcept);
    }
}

#[test]
fn lexer_indentifies_concrete_concept() {
    let cont = NEW_CONTEXT.clone();
    let lexemes = cont.lex("let");
    assert_eq!(lexemes.len(), 1);
    assert_eq!(lexemes.first().unwrap().category, LexemeCategory::ConcreteConcept);
}

#[test]
fn lexer_indentifies_abstract_concept() {
    let cont = NEW_CONTEXT.clone();
    let lexemes = cont.lex("and");
    assert_eq!(lexemes.len(), 1);
    assert_eq!(lexemes.first().unwrap().category, LexemeCategory::AbstractConcept);
}

#[test]
fn lexer_indentifies_opening_parenthesis() {
    let cont = NEW_CONTEXT.clone();
    let lexemes = cont.lex("(");
    assert_eq!(lexemes.len(), 1);
    assert_eq!(lexemes.first().unwrap().category, LexemeCategory::OpeningParenthesis{closing_position: None, ..});
}