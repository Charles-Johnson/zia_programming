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
extern crate zia;

use zia::{Context, ContextMaker, Execute, ZiaError};

macro_rules! assume_symbol {
	($a:ident) => (
		prop_assume!($a.len() > 0);
		prop_assume!(!$a.contains(' '));
		prop_assume!(!$a.contains('('));
		prop_assume!(!$a.contains(')'));
	)
}

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
		prop_assume!(a != ":=");
		assume_symbol!(a);
		assume_symbol!(b);
		assume_symbol!(c);
		prop_assume!(a != b);
		prop_assume!(a != c);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= ({} {})))", a, b, c);
		assert_eq!(cont.execute(&let_command), "");
		let print_command = format!("(label_of ({} :=)) ->", a);
		let result = format!("{} {}", b, c);
		assert_eq!(cont.execute(&print_command), result);
	}
	// The label of the expansion of a previosuly unused concept which is composed of a nested pair of previously unused concepts should reduce to the string representation of the nested pair. 
	#[test]
	fn fresh_nested_pairs(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
		prop_assume!(a != ":=");
		assume_symbol!(a);
		assume_symbol!(b);
		assume_symbol!(c);
		assume_symbol!(d);
		prop_assume!(a != b);
		prop_assume!(a != c);
		prop_assume!(a != d);
		let mut cont = Context::new();
		let let_command = format!("let ({} (:= ({} ({} {}))))", a, b, c, d);
		assert_eq!(cont.execute(&let_command), "");
		let print_command = format!("(label_of ({} :=)) ->", a);
		let result = format!("{} ({} {})", b, c, d);
		assert_eq!(cont.execute(&print_command), result);
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
		prop_assume!(a != ":=");
		assume_symbol!(a);
		assume_symbol!(b);
		assume_symbol!(c);
		assume_symbol!(d);
		assume_symbol!(e);
		prop_assume!(a != b);
		prop_assume!(a != c);
		prop_assume!(a != d);
		prop_assume!(a != e);
		prop_assume!(b != d);
		prop_assume!(b != e);
		prop_assume!(c != d);
		prop_assume!(c != e);
		let mut cont = Context::new();
		let first_let_command = format!("let ({} (:= ({} {})))", a, b, c);
		assert_eq!(cont.execute(&first_let_command), "");
		let second_let_command = format!("let ({} (:= ({} {})))", b, d, e);
		assert_eq!(cont.execute(&second_let_command), "");
		let print_command = format!("(label_of ({} :=)) ->", b);
		let result = format!("{} {}", d, e);
		assert_eq!(cont.execute(&print_command), result);
	}
	// The label of the expansion of a previously unused concept which is composed of a pair of previously used concepts should reduce to the string representation of the pair.
	#[test]
	fn defining_fresh_symbol_as_used_pair(
		a in "\\PC*", 
		b in "\\PC*", 
		c in "\\PC*", 
		d in "\\PC*",
	) {
		assume_symbol!(a);
		assume_symbol!(b);
		assume_symbol!(c);
		assume_symbol!(d);
		prop_assume!(a != ":=");
		prop_assume!(a != b);
		prop_assume!(a != c);
		prop_assume!(d != b);
		prop_assume!(d != c);
		prop_assume!(a != d);
		let mut cont = Context::new();
		let first_let_command = format!("let ({} (:= ({} {})))", a, b, c);
		assert_eq!(cont.execute(&first_let_command), "");
		let second_let_command = format!("let ({} (:= ({} {})))", d, b, c);
		assert_eq!(cont.execute(&second_let_command), "");
		let print_command = format!("(label_of ({} :=)) ->", d);
		let result = format!("{} {}", b, c);
		assert_eq!(cont.execute(&print_command), result);
	}
}
#[test]
fn old_pair() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(cont.execute("let (d (:= (e f)))"), "",);
    assert_eq!(cont.execute("let (b (:= (e c)))"), "");
    assert_eq!(cont.execute("(label_of (b :=)) ->"), "e c");
}
#[test]
fn pair_on_the_left() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let ((a b) (:= c))"),
        ZiaError::BadDefinition.to_string()
    );
}
#[test]
fn fresh_refactor() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (a (:= b))"),
        ZiaError::RedundantRefactor.to_string()
    );
}
#[test]
fn refactor() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(cont.execute("let (d (:= b))"), "");
    assert_eq!(cont.execute("(label_of (a :=)) ->"), "d c");
}
#[test]
fn bad_refactor() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(
        cont.execute("let (b (:= a))"),
        ZiaError::DefinitionCollision.to_string()
    );
}
#[test]
fn defining_used_symbol_as_used_pair() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(cont.execute("let (f (:= (d e)))"), "");
    assert_eq!(
        cont.execute("let (d (:= (b c)))"),
        ZiaError::DefinitionCollision.to_string()
    );
}
#[test]
fn definition_loop() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (a (:= (a b)))"),
        ZiaError::InfiniteDefinition.to_string()
    );
}
#[test]
fn nested_definition_loop() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (a (:= ((a b) b)))"),
        ZiaError::InfiniteDefinition.to_string()
    );
}
#[test]
fn chained_definitions_loop() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (c (:= (a b)))"), "");
    assert_eq!(
        cont.execute("let (a (:= (c b)))"),
        ZiaError::InfiniteDefinition.to_string()
    );
}
#[test]
fn remove_definition() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(cont.execute("let (a (:= a))"), "");
    assert_eq!(cont.execute("(label_of (a :=)) ->"), "a");
    assert_eq!(
        cont.execute("let (a (:= b))"),
        ZiaError::RedundantRefactor.to_string()
    );
}
#[test]
fn redundantly_remove_definition() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(
        cont.execute("let (b (:= b))"),
        ZiaError::RedundantDefinitionRemoval.to_string()
    );
}
#[test]
fn redundancy() {
    let mut cont = Context::new();
    assert_eq!(cont.execute("let (a (:= (b c)))"), "");
    assert_eq!(
        cont.execute("let (a (:= (b c)))"),
        ZiaError::RedundantDefinition.to_string()
    );
}
#[test]
fn setting_definition_of_concrete() {
    let mut cont = Context::new();
    assert_eq!(
        cont.execute("let (:= (:= (a b)))"),
        ZiaError::SettingDefinitionOfConcrete.to_string()
    );
}
