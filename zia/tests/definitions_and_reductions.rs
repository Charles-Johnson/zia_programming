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

#[test]
fn indirect_reduction() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("let d e -> f"), "");
    assert_eq!(cont.execute("let g := c f"), "");
    assert_eq!(cont.execute("(a b) d e"), "g");
}

// The interpreter should not allow a definition of a concept in terms of concepts that may reduce to the former concept.
#[test]
fn sneeky_infinite_reduction_chain() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let c d -> a"), "");
    assert_eq!(
        cont.execute("let a := b c d"),
        ZiaError::InfiniteDefinition.to_string()
    );
}

// A reduction defined for a concept which is used to compose labelled concepts should not be accepted by the interpreter.
#[test]
fn reducing_part_of_a_labelled_concept() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a := b c d"), "");
    assert_eq!(cont.execute("let c d -> e"), "");
    assert_eq!(cont.execute("a"), cont.execute("b e"));
}
