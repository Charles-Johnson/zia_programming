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
#[macro_use]
extern crate proptest;
#[macro_use]
extern crate test_zia;
extern crate zia;

// Needed for assume_abstract macro which is needed for let_reduction macro
use test_zia::CONCRETE_SYMBOLS;
use zia::{Context, ContextMaker, Execute, ZiaError};

// Common pattern in tests where a pair of symbols reduces to another symbol
macro_rules! reduce_pair {
	($cont:ident, $a:ident, $b:ident, $c:ident) => (
		assume_symbols!($a, $b, $c);
		let reduction = format!("let (({} {}) (-> {}))", $a, $b, $c);
		prop_assert_eq!($cont.execute(&reduction), "");
	)
}

proptest! {
	// If a symbol reduces to another symbol then the label of the reduction of that symbol must be a string of that other symbol
	#[test]
	fn symbol_to_symbol(a in "\\PC*", b in "\\PC*") {
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let print = format!("(label_of ({} ->)) ->", a);
		prop_assert_eq!(cont.execute(&print), b);
	}
	// If a pair of symbols reduces to another symbol then the label of the reduction of that pair must be a string of that other symbol
	#[test]
	fn pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		reduce_pair!(cont, a, b, c);
		let print = format!("(label_of (({} {}) ->)) ->", a, b);
		prop_assert_eq!(cont.execute(&print), c);
	}
	// Checking whether two reduction rules can be correctly chained together for an expression with a nested pair
	#[test]
	fn nested_pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		assume_symbols!(a, b, c);
		let mut cont = Context::new();
		reduce_pair!(cont, a, b, c);
		reduce_pair!(cont, a, c, b);
		let print = format!("(label_of ((({}({} {}))->)->)) -> ", a, a, b);
		assert_eq!(cont.execute(&print), b);
	}
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
