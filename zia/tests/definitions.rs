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

use zia::{ZiaError, NEW_CONTEXT};

// The label of a new symbol should reduce to the string of the symbol.
#[test]
fn fresh_symbol() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("label_of a"), "'a'");
}

// The label of the expansion of a previously used concept which is composed of a pair of previously used concepts should reduce to the string representation of the pair.
// The interpreter should not accept a definition where the lefthand side is not a symbol
#[test]
fn pair_on_the_left() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let (a b) := c"),
        ZiaError::BadComposition.to_string()
    );
}

// Refactoring a symbol that was never previously used with another symbol that has never been used has no effect so the interpreter should tell you if this is unnecessary.
#[test]
fn fresh_refactor() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let a := b"),
        ZiaError::RedundantRefactor.to_string()
    );
}

// Should not be able to refactor a used symbol to another used symbol
#[test]
fn bad_refactor() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a := b c"), "");
    assert_eq!(
        cont.execute("let b := a"),
        ZiaError::CompositionCollision.to_string()
    );
}

// Should not be able to define a used symbol in terms of a pair of symbols that have already been defined.
#[test]
fn defining_used_symbol_as_used_pair() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a := b c"), "");
    assert_eq!(cont.execute("let f := d e"), "");
    assert_eq!(
        cont.execute("let d := b c"),
        ZiaError::CompositionCollision.to_string()
    );
}

// Cannot define a symbol in terms of that symbol
#[test]
fn definition_loop() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let a := a b"),
        ZiaError::InfiniteComposition.to_string()
    );
}

// Cannot define a symbol in terms of that symbol
#[test]
fn nested_definition_loop() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let a := (a b) b"),
        ZiaError::InfiniteComposition.to_string()
    );
}

// Cannot define a concept in terms of concepts defined in terms of the former concept.
#[test]
fn chained_definitions_loop() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let c := a b"), "");
    assert_eq!(
        cont.execute("let a := c b"),
        ZiaError::InfiniteComposition.to_string()
    );
}

// If a concept's definition that doesn't exist tries to get removed than the interpreter should let the user know
#[test]
fn redundantly_remove_definition() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a := b c"), "");
    assert_eq!(
        cont.execute("let b := b"),
        ZiaError::RedundantCompositionRemoval.to_string()
    );
}

// If the definition has already been specified in the same way, the interpreter will let the user know
#[test]
fn redundancy() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a := b c"), "");
    assert_eq!(
        cont.execute("let a := b c"),
        ZiaError::RedundantComposition.to_string()
    );
}

// Concrete concepts should not be able to be expanded.
#[test]
fn setting_definition_of_concrete() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let label_of := a b"),
        ZiaError::SettingCompositionOfConcrete.to_string()
    );
}
