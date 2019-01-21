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

// Common pattern in tests where a pair of symbols reduces to another symbol. If `a == b` the tests that use this macro fail as if they forgot the reduction rule.
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
		prop_assume!(a != b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let print = format!("(label_of ({} ->)) ->", a);
		prop_assert_eq!(cont.execute(&print), b);
	}
	// If a pair of symbols reduces to another symbol then the label of the reduction of that pair must be a string of that other symbol. Error! : if `a == b` then the interpreter can't reduce `a b` to `c`.
	#[test]
	fn pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		reduce_pair!(cont, a, b, c);
		let print = format!("(label_of (({} {}) ->)) ->", a, b);
		prop_assert_eq!(cont.execute(&print), c);
	}
	// Checking whether two reduction rules can be correctly chained together for an expression with a nested pair. Error! : if `a == b` then the interpreter can't twice reduce `a (a b)` to `b`. 
	#[test]
	fn nested_pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		reduce_pair!(cont, a, b, c);
		reduce_pair!(cont, a, c, b);
		let print = format!("(label_of ((({}({} {}))->)->)) -> ", a, a, b);
		prop_assert_eq!(cont.execute(&print), b);
	}
	// Checking whether the label of the reduction of the reduction of a concept reduces correctly 
	#[test]
	fn chain(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		// to prevent rendundant reductions and looping reductions
		prop_assume!(a != b && b != c && c != a);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, b, c);
		let print = format!("(label_of (({} ->)->)) ->", a);
		prop_assert_eq!(cont.execute(&print), c);
	}
	// A concept should not be able to reduce to a concept whose normal form is the former concept.
	#[test]
	fn cycle(a in "\\PC*", b in "\\PC*") {
		// to prevent redundant reduction
		prop_assume!(a != b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let reduction = format!("let ({} (-> {}))", b, a);
		prop_assert_eq!(
		    cont.execute(&reduction),
		    ZiaError::CyclicReduction.to_string()
		);
		let print = format!("(label_of ({} ->)) ->", b);
		prop_assert_eq!(cont.execute(&print), b);
	}
	// If only a symbol is enclosed in parentheses, then this should be the same as if the parentheses weren't there
	#[test]
	fn trivial_parentheses(a in "\\PC*") {
		let mut cont = Context::new();
		assume_symbol!(a);
		let print = format!("(label_of(({}) ->))->", a);
		prop_assert_eq!(cont.execute(&print), a);
	}
	// Checking that reduction rules can be removed. Error! : if `a == b` then the interpreter thinks a reduction rule already exists.
	#[test]
	fn remove_reduction(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		reduce_pair!(cont, b, c, a);
		let reduction = format!("let (({} {}) (-> ({} {})))", b, c, b, c);
		prop_assert_eq!(cont.execute(&reduction), "");
		let print = format!("(label_of(({} {}) ->))->", b, c);
		let result = format!("{} {}", b, c);
		prop_assert_eq!(cont.execute(&print), result);
	}
	// A concept should not be able to reduce to something composed of that concept.
	#[test]
	fn infinite_expansion(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		let reduction = format!("let ({} (-> ({} {})))", b, a, b);
		let mut cont = Context::new();
		assert_eq!(
		    cont.execute(&reduction),
		    ZiaError::ExpandingReduction.to_string()
		);
	}
	// A concept that used to doubly reduce but whose first reduction no longer reduces should doubly reduce to its first reduction. 
	#[test]
	fn broken_end_chain(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		// to prevent rendundant reductions and looping reductions
		prop_assume!(a != b && b != c && c != a);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, b, c);
		let_reduction!(cont, b, b);
		let print = format!("(label_of (({} ->)->)) ->", a);
		assert_eq!(cont.execute(&print), b);
	}
	// A concept that used to triply reduce but whose first reduction no longer reduces should triply reduce to its first reduction. 
	#[test]
	fn broken_middle_chain(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
		// to prevent rendundant reductions and looping reductions
		prop_assume!(a != b && b != c && c != d && c != a && d != a && d != b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, b, c);
		let_reduction!(cont, c, d);
		let_reduction!(cont, b, b);
		let print = format!("(label_of ((({} ->)->)->)) ->", a);
		assert_eq!(cont.execute(&print), b);
	}
	// Checking that reduction rules can be changed correctly
	#[test]
	fn change_reduction_rule(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		// to prevent rendundant reductions
		prop_assume!(a != b && b != c);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, a, c);
		let print = format!("(label_of ({} ->)) ->", a);
		assert_eq!(cont.execute(&print), c);
	}
	// It is not necessarily redundant to make a concept reduce to its normal form.  
	#[test]
	fn leapfrog_reduction_rule(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		// `a`, `b` and `c` all need to be unique to prevent redundant reductions and looping reductions.
		prop_assume!(a != b && b != c && c != a);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, b, c);
		let_reduction!(cont, a, c);
	}
	// It is redundant to specify a reduction rule that is already true
	#[test]
	fn redundancy(a in "\\PC*", b in "\\PC*") {
		prop_assume!(a != b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let reduction = format!("let ({} (-> {}))", a, b);
		assert_eq!(
		    cont.execute(&reduction),
		    ZiaError::RedundantReduction.to_string()
		);
		let remove_reduction = format!("let ({} (-> {}))", b, b);
		assert_eq!(
		    cont.execute(&remove_reduction),
		    ZiaError::RedundantReduction.to_string()
		);
	}
	// Concrete concepts should be allowed to reduce to anything
	#[test]
	fn reducing_concrete(a in "\\PC*", c in "label_of|:=|->|let") {
		let mut cont = Context::new();
		assume_symbol!(a);
		let reduction = format!("let ({} (-> {}))", c, a);
		assert_eq!(
		    cont.execute(&reduction),
		    ZiaError::ConcreteReduction.to_string()
		);
	}
}
