/*  Library for the Zia programming language.
    Copyright (C) 2018 to 2019 Charles Johnson

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

proptest! {
	// A previously unused symbol cannot be interpreted as a program
	#[test]
	fn fresh_symbol_is_not_a_program(a in "\\PC*") {
		assume_abstract!(a);
		assume_symbol!(a);
		let mut cont = Context::new();
		assert_eq!(cont.execute(&a), ZiaError::NotAProgram.to_string());
	}
	// A pair of previously unused symbols cannot be interpreted as a program
	#[test]
	fn fresh_pair_is_not_a_program(a in "\\PC*", b in "\\PC*") {
		assume_abstract!(a);
		assume_abstract!(b);
		assume_symbols!(a, b);
		let mut cont = Context::new();
		let command = format!("{} {}", a, b);
		assert_eq!(cont.execute(&command), ZiaError::NotAProgram.to_string());
	}
	// An expression of previously unused symbols containing a nested pair cannot be interpreted as a program
	#[test]
	fn fresh_nested_pair_is_not_a_program(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		assume_abstract!(a);
		assume_abstract!(b);
		assume_abstract!(c);
		assume_symbols!(a, b, c);
		let mut cont = Context::new();
		let command = format!("{} ({} {})", a, b, c);
		assert_eq!(cont.execute(&command), ZiaError::NotAProgram.to_string());
	}
	// A previously used symbol cannot be interpreted as a program unless the concept is composed of any concrete concepts.
	#[test]
	fn used_symbol_is_not_a_program(a in "\\PC*", b in "\\PC*") {
		prop_assume!(a != b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		assert_eq!(cont.execute(&a), ZiaError::NotAProgram.to_string());
	}
	// A pair of previously used symbols cannot be interpreted as a program unless their concepts are composed of any concrete concepts.
	#[test]
	fn used_symbol_in_a_pair_is_not_a_program(a in "\\PC*", b in "\\PC*") {
		prop_assume!(a != b);
		assume_abstract!(b);
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let command = format!("{} {}", a, b);
		assert_eq!(cont.execute(&command), ZiaError::NotAProgram.to_string());
	}
	// An expression with a nested pair composed of previously used symbols cannot be interpreted as a program unless their concepts are composed of any concrete concepts.
	#[test]
	fn used_symbol_in_a_nested_pair_is_not_a_program(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*",
	) {
		assume_abstract!(c);
		prop_assume!((a != b) && (b != c) && (c != a));
		let mut cont = Context::new();
		let_reduction!(cont, a, b);
		let_reduction!(cont, b, c);
		let command = format!("{} ({} {})", a, b, c);
		assert_eq!(cont.execute(&command), ZiaError::NotAProgram.to_string());
	}
	#[test]
	fn symbol_whose_normal_form_is_a_program_is_a_program(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		let mut cont = Context::new();
		assert_eq!(cont.execute(&format!("let ({} (-> ((label_of {}) ->)))", a, b)), "");
		assert_eq!(cont.execute(&a), ZiaError::NotAProgram.to_string());
		assert_eq!(cont.execute(&format!("{} ->", a)), b);
	}
	#[test]
	fn symbol_whose_definition_is_a_program_is_a_program(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		prop_assume!(a != b);
		let mut cont = Context::new();
		assert_eq!(cont.execute(&format!("let ({} (:= ((label_of({} :=))->))", a, b)), "");
		assert_eq!(cont.execute(&a), b);
	}
	#[test]
	fn symbol_whose_normal_form_is_a_builtin_concept(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		let mut cont = Context::new();
		assert_eq!(cont.execute(&format!("let ({} (-> :=))", a)), "");
		assert_eq!(cont.execute(&format!("{} {}", a, b)), ZiaError::NotAProgram.to_string());
		assert_eq!(cont.execute(&format!("(label_of (({} {}) ->)) ->", b, a)), b);
	}
}
