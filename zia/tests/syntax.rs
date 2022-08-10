//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019 Charles Johnson
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program. If not, see <http://www.gnu.org/licenses/>.
extern crate zia;
#[macro_use]
extern crate proptest;

use zia::{multi_threaded::NEW_CONTEXT, ZiaError, Associativity};

#[test]
fn empty_parentheses() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("()"), ZiaError::EmptyParentheses.to_string());
}
#[test]
fn ambiguous_expression() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let assoc c -> left"), "");
    assert_eq!(
        cont.execute("a b c"),
        ZiaError::DifferentAssociativityAmongstLowestPrecendenceTokens{associativity: Associativity::Left, other_associativity: Associativity::Right, token: "c".to_string()}.to_string()
    );
}
proptest! {
    // No input should crash the interpreter
    #[test]
    fn random_input(a in "\\PC*") {
        let mut cont = NEW_CONTEXT.clone();
        cont.execute(&a);
    }
}
