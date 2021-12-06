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
fn lexer_indentifies_new_concepts() {
    let cont = NEW_CONTEXT.clone();
    let lexemes = cont.lex("new_concept");
    assert_eq!(lexemes.len(), 1);
    assert_eq!(lexemes.first().unwrap().category, LexemeCategory::NewConcept);
}
