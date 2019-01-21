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

proptest! {
	// The label of a new symbol should reduce to the string of the symbol. 
	#[test]
	fn fresh_symbol(a in "\\PC*") {
		assume_symbol!(a);
		let mut cont = Context::new();
		let command = format!("(label_of ({} :=)) ->", a);
		prop_assert_eq!(cont.execute(&command), a);
	}
	// The label of the expansion of a previously unused concept which is composed of a pair of previously unused concepts should reduce to the string representation of the pair. 
	#[test]
	fn fresh_pair(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		print_pair_expansion!(cont, a, b, c);
	}
	// The label of the expansion of a previosuly unused concept which is composed of a nested pair of previously unused concepts should reduce to the string representation of the nested pair. 
	#[test]
	fn fresh_nested_pairs(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
		assume_abstract!(a);
		assume_symbols!(a,b,c,d);
		prop_assume!(a != b);
		prop_assume!(a != c);
		prop_assume!(a != d);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= ({} ({} {}))))", a, b, c, d);
		prop_assert_eq!(cont.execute(&let_command), "");
		let print_command = format!("(label_of ({} :=)) ->", a);
		let result = format!("{} ({} {})", b, c, d);
		prop_assert_eq!(cont.execute(&print_command), result);
	}
	// The label of the expansion of a previously used concept which is composed of a pair of previously unused concepts should reduce to the string representation of the pair. 
	#[test]
	fn defining_used_symbol_as_fresh_pair(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*", 
		d in "\\PC*",
		e in "\\PC*",
	) {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		// `a` will contain `d` and `e` so assume it's not the same as them.
		prop_assume!(a != d);
		prop_assume!(a != e);
		// `d` and `e` must not have been previously been used so can't be the same as `c` either
		prop_assume!(c != d);
		prop_assume!(c != e);
		let_definition!(cont, b, d, e);
		print_pair_expansion!(cont, b, d, e);
	}
	// The label of the expansion of a previously unused concept which is composed of a pair of previously used concepts should reduce to the string representation of the pair.
	#[test]
	fn defining_fresh_symbol_as_used_pair(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*", 
		d in "\\PC*",
	) {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		// `d` is not previously used so can't be equal to `a` either
		prop_assume!(a != d);
		let_definition!(cont, d, b, c); // Does this refactor `a` to `d`?
		print_pair_expansion!(cont, d, b, c);
	}
	// The label of the expansion of a previously used concept which is composed of a pair of previously used concepts should reduce to the string representation of the pair.
	#[test]
	fn old_pair(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*", 
		d in "\\PC*",
		e in "\\PC*", 
		f in "\\PC*", 
	) {
		// If `c == f` then `b` would be defined as an existing concept. This is not allowed because `b` is already used.
		prop_assume!(c != f);
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let_definition!(cont, d, e, f);
		let_definition!(cont, b, e, c);
		print_pair_expansion!(cont, b, e, c);
	}
	// The interpreter should not accept a definition where the lefthand side is not a symbol
	#[test]
	fn pair_on_the_left(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		assume_symbols!(a, b, c);
		let mut cont = Context::new();
		let let_command = format!("let (({} {}) (:= {}))", a, b, c);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::BadDefinition.to_string()
		);
	}
	// Refactoring a symbol that was never previously used with another symbol that has never been used has no effect so the interpreter should tell you if this is unnecessary.
	#[test]
	fn fresh_refactor(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		// if `a == b` then the definition is redundant, not the refactor
		prop_assume!(a != b);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= {}))", a, b);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::RedundantRefactor.to_string()
		);
	}
	// Refactoring of a symbol should be reflected in the label of a concept that was composed by it.
	#[test]
	fn refactor(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*", 
		d in "\\PC*",
	) {
		let mut cont = Context::new();
		// if `d` is not a symbol then it's a bad definition
		assume_symbol!(d);
		// if `d == b` then the definition is redundant
		prop_assume!(d != b);
		let_definition!(cont, a, b, c);
		let let_command = format!("let ({} (:= {}))", d, b);
		prop_assert_eq!(cont.execute(&let_command), "");
		print_pair_expansion!(cont, a, d, c);
	}
	// Should not be able to refactor a used symbol to another used symbol
	#[test]
	fn bad_refactor(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let let_command = format!("let ({} (:= {}))", b, a);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::DefinitionCollision.to_string()
		);
	}
	// Should not be able to define a used symbol in terms of a pair of symbols that have already been defined.
	#[test]
	fn defining_used_symbol_as_used_pair(
		a in "\\PC*",
		b in "\\PC*",
		c in "\\PC*",
		d in "\\PC*",
		e in "\\PC*",
		f in "\\PC*",
	) {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let_definition!(cont, f, d, e);
		let let_command = format!("let ({} (:= ({} {})))", d, b, c);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::DefinitionCollision.to_string()
		);
	}
	// Cannot define a symbol in terms of that symbol
	#[test]
	fn definition_loop(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= ({} {})))", a, a, b);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::InfiniteDefinition.to_string()
		);
	}
	// Cannot define a symbol in terms of that symbol
	#[test]
	fn nested_definition_loop(a in "\\PC*", b in "\\PC*") {
		assume_symbols!(a, b);
		prop_assume!(a != b);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= (({} {}) {})))", a, a, b, b);
		prop_assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::InfiniteDefinition.to_string()
		);
	}
	// Cannot define a concept in terms of concepts defined in terms of the former concept.
	#[test]
	fn chained_definitions_loop(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, c, a, b);
		let let_command = format!("let ({} (:= ({} {})))", a, c, b);
		assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::InfiniteDefinition.to_string()
		);
	}
	// If a concept did have a definition but had that defintion removed, the concept is removed from the context.
	#[test]
	fn remove_definition(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let remove_definition_command = format!("let ({} (:= {}))", a, a);
		assert_eq!(cont.execute(&remove_definition_command), "");
		let print_command = format!("(label_of ({} :=)) ->", a);
		assert_eq!(cont.execute(&print_command), a);
		let refactor_command = format!("let ({} (:= {}))", a, b);
		assert_eq!(
		    cont.execute(&refactor_command),
		    ZiaError::RedundantRefactor.to_string()
		);
	}
	// If a concept's definition that doesn't exist tries to get removed than the interpreter should let the user know
	#[test]
	fn redundantly_remove_definition(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let remove_definition_command = format!("let ({} (:= {}))", b, b);
		assert_eq!(
		    cont.execute(&remove_definition_command),
		    ZiaError::RedundantDefinitionRemoval.to_string()
		);
	}
	// If the definition has already been specified in the same way, the interpreter will let the user know  
	#[test]
	fn redundancy(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
		let mut cont = Context::new();
		let_definition!(cont, a, b, c);
		let let_command = format!("let ({} (:= ({} {})))", a, b, c);
		assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::RedundantDefinition.to_string()
		);
	}
	// Concrete concepts should not be able to be expanded.
	#[test]
	fn setting_definition_of_concrete(a in "\\PC*", b in "\\PC*", c in "label_of|:=|->|let") {
		assume_symbols!(a,b);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= ({} {})))", c, a, b);
		assert_eq!(
		    cont.execute(&let_command),
		    ZiaError::SettingDefinitionOfConcrete.to_string()
		);
	}
}
