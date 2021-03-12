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

// A pair of symbols reduces to another symbol
#[test]
fn pair_to_symbol() {
    let mut cont = NEW_CONTEXT.clone();
    cont.execute("let a b -> c");
    assert_eq!(cont.execute("a b"), "c");
}
// Checking whether two reduction rules can be correctly chained together for an expression with a nested pair.
#[test]
fn nested_pair_to_symbol() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("let a c -> b"), "");
    assert_eq!(cont.execute("a a b"), "b");
}
// A concept should not be able to reduce to a concept whose normal form is the former concept.
#[test]
fn cycle() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c d"), "");
    assert_eq!(
        cont.execute("let c d -> a b"),
        ZiaError::CyclicReduction.to_string()
    );
}
// If only a symbol is enclosed in parentheses, then this should be the same as if the parentheses weren't there
#[test]
fn trivial_parentheses() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("(a)"), "a");
}
// A concept should not be able to reduce to something composed of that concept.
#[test]
fn infinite_expansion() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(
        cont.execute("let a b -> c a b"),
        ZiaError::ExpandingReduction.to_string()
    );
}
// A concept that used to doubly reduce but whose first reduction no longer reduces should doubly reduce to its first reduction.
#[test]
fn broken_end_chain() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c d"), "");
    assert_eq!(cont.execute("let c d -> e"), "");
    assert_eq!(cont.execute("let c d -> c d"), "");
    assert_eq!(cont.execute(&"a b"), "c d");
}
// A concept that used to triply reduce but whose second reduction no longer reduces should triply reduce to its second reduction.
#[test]
fn broken_middle_chain() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c d"), "");
    assert_eq!(cont.execute("let c d -> e f"), "");
    assert_eq!(cont.execute("let e f -> g"), "");
    assert_eq!(cont.execute("let e f -> e f"), "");
    assert_eq!(cont.execute("a b"), "e f");
}
// Checking that reduction rules can be changed correctly
#[test]
fn change_reduction_rule() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(cont.execute("let a b -> d"), "");
    assert_eq!(cont.execute("a b"), "d");
}
// It is not necessarily redundant to make a concept reduce to its normal form.
#[test]
fn leapfrog_reduction_rule() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c d"), "");
    assert_eq!(cont.execute("let c d -> e"), "");
    assert_eq!(cont.execute("let a b -> e"), "");
}
// It is redundant to specify a reduction rule that is already true
#[test]
fn redundancy() {
    let mut cont = NEW_CONTEXT.clone();
    assert_eq!(cont.execute("let a b -> c"), "");
    assert_eq!(
        cont.execute("let a b -> c"),
        ZiaError::RedundantReduction.to_string()
    );
    assert_eq!(
        cont.execute("let c d -> c d"),
        ZiaError::RedundantReduction.to_string()
    );
}
