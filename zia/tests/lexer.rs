use zia::multi_threaded::NEW_CONTEXT;
use proptest::prelude::*;

proptest!{
    #[test]
    fn lexer_preserves_text(command in "\\PC*") {
        let cont = NEW_CONTEXT.clone();
        let reconstructed_text = cont.lex(&command)
            .into_iter()
            .fold(String::new(), |reconstructed_text, lexeme| reconstructed_text + &lexeme.text);
        assert_eq!(reconstructed_text, command);
    }
}

