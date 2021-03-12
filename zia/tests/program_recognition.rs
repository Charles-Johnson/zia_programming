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

use zia::NEW_CONTEXT;

// A previously unused symbol cannot reduce
#[test]
fn fresh_symbol_is_not_a_program() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("a"), "a");
}
// A pair of previously unused symbols cannot reduce
#[test]
fn fresh_pair_does_not_reduce() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("a b"), "a b");
}
// An expression of previously unused symbols containing a nested pair cannot reduce
#[test]
fn fresh_nested_pair_does_not_reduce() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("a b c"), "a b c");
}
// A previously used symbol cannot reduce unless it is a reducible concepts.
#[test]
fn used_symbol_does_not_reduce() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("c"), "c");
}
// A pair of previously used symbols cannot reduce unless their concepts are composed of any reducible concepts.
#[test]
fn used_symbol_in_a_pair_does_not_reduce() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("a c"), "a c");
}
// An expression with a nested pair composed of previously used symbols cannot reduce unless their concepts are composed of any reducible concepts.
#[test]
fn used_symbol_in_a_nested_pair_does_not_reduce() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("a b c"), "a b c");
}
