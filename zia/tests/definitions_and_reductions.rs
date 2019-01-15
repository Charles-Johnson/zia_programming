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

// Needed for assume_abstract macro which is needed for let_definition macro
use test_zia::CONCRETE_SYMBOLS;
use zia::{Context, ContextMaker, Execute, ZiaError};

macro_rules! let_reduction {
	($cont:ident, $a:ident, $b:ident) => (
		assume_symbols!($a, $b);
		assume_abstract!($a);
		let reduction = format!("let ({} (-> {}))", $a, $b);
		prop_assert_eq!($cont.execute(&reduction), "");
	)
}

proptest! {
	// If the components of a concept reduce to something that has its own symbol, then the label of the reduction of the original concept should be the latter symbol. The original concept should not be able to reduce to something else because its composition defines its reduction.
	#[test]
	fn indirect_reduction(
		a in "\\PC*",
		b in "\\PC*",
		c in "\\PC*",
		d in "\\PC*",
		e in "\\PC*",
		f in "\\PC*",
		g in "\\PC*",
	) {
		assume_symbol!(g);
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let_reduction!(cont, b, d);
		let_reduction!(cont, c, e);
		let print_reduction = format!("(label_of ({} ->)) ->", a);
		let result = format!("{} {}", d, e);
		assert_eq!(cont.execute(&print_reduction), result);
		let_definition!(cont, f, d, e);
		assert_eq!(cont.execute(&print_reduction), f);
		let reduction = format!("let ({} (-> {}))", a, g);
		assert_eq!(
		    cont.execute(&reduction),
		    ZiaError::MultipleReductionPaths.to_string()
		);
	}
}
#[test]
fn sneeky_infinite_reduction_chain() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (c (-> a))"), "");
    assert_eq!(
        cont.execute("let (a (:= (c b)))"),
        ZiaError::ExpandingReduction.to_string()
    );
}
#[test]
fn reducing_nested_definition() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b (c d))))"), "");
    assert_eq!(cont.execute("let (c (-> e))"), "");
    assert_eq!(
        cont.execute("let (a (-> f))"),
        ZiaError::MultipleReductionPaths.to_string()
    );
}
