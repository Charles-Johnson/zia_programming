/*  Library for the Zia programming language.
    Copyright (C) 2018  Charles Johnson

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program. If not, see <http://www.gnu.org/licenses/>.
*/
extern crate zia;

use zia::{Context, ContextMaker, Execute, ZiaError};

#[test]
fn symbol_to_symbol() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("(label_of (a ->)) ->"), "b");
}
#[test]
fn pair_to_symbol() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let ((not true) (-> false))"), "");
    assert_eq!(cont.execute("(label_of ((not true) ->)) ->"), "false");
}
#[test]
fn nested_pairs_to_symbol() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let ((not true) (-> false))"), "");
    assert_eq!(cont.execute("let ((not false) (-> true))"), "");
    assert_eq!(cont.execute("(label_of ((not(not true))->)) -> "), "not false");
}
#[test]
fn chain() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("let (b (-> c))"), "");
    assert_eq!(cont.execute("(label_of (a ->)) ->"), "b");
}
#[test]
fn cycle() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(
        cont.execute("let (b (-> a))"),
        ZiaError::CyclicReduction.to_string()
    );
    assert_eq!(cont.execute("(label_of (b ->))->"), "b");
}
#[test]
fn trivial_parentheses() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("(label_of((a) ->))->"), "a");
}
#[test]
fn remove_reduction() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let ((b c) (-> a))"), "");
    assert_eq!(cont.execute("let ((b c) (-> (b c)))"), "");
    assert_eq!(cont.execute("(label_of((b c) ->))->"), "b c");
}
#[test]
fn infinite_expansion() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (b (-> (a b)))"),
        ZiaError::ExpandingReduction.to_string()
    );
}
#[test]
fn broken_end_chain() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("let (b (-> c))"), "");
    assert_eq!(cont.execute("let (b (-> b))"), "");
    assert_eq!(cont.execute("(label_of (a ->)) ->"), "b");
}
#[test]
fn broken_middle_chain() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("let (b (-> c))"), "");
    assert_eq!(cont.execute("let (c (-> d))"), "");
    assert_eq!(cont.execute("let (b (-> b))"), "");
    assert_eq!(cont.execute("(label_of (a ->)) ->"), "b");
}
#[test]
fn change_reduction_rule() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("let (a (-> c))"), "");
    assert_eq!(cont.execute("(label_of (a ->)) ->"), "c");
}
#[test]
fn leapfrog_reduction_rule() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(cont.execute("let (b (-> c))"), "");
    assert_eq!(cont.execute("let (a (-> c))"), "");
}
#[test]
fn redundancy() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (-> b))"), "");
    assert_eq!(
        cont.execute("let (a (-> b))"),
        ZiaError::RedundantReduction.to_string()
    );
    assert_eq!(
        cont.execute("let (b (-> b))"),
        ZiaError::RedundantReduction.to_string()
    );
}
#[test]
fn reducing_concrete() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (-> (-> a))"),
        ZiaError::ConcreteReduction.to_string()
    );
}
