//  Testing library for the Zia programming language.
// Copyright (C) 2019 Charles Johnson
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

#[macro_use]
extern crate lazy_static;
extern crate proptest;
#[macro_use]
extern crate maplit;

use std::collections::HashSet;

// Common pattern in tests where a pair of symbols reduces to another symbol. If `b == c` and `b` and `c` are previously unused symbols the tests that use this macro fail as if they forgot the reduction rule.
#[macro_export]
macro_rules! reduce_pair {
    ($cont:ident, $a:ident, $b:ident, $c:ident) => {
        assume_symbols!($a, $b, $c);
        let reduction = format!("let {} {} -> {}", $a, $b, $c);
        prop_assert_eq!($cont.execute(&reduction), "");
    };
}

// Checks if a string can respresent a symbol
#[macro_export]
macro_rules! assume_symbol {
    ($a:ident) => {
        prop_assume!($a.len() > 0);
        prop_assume!(!$a.contains(' '));
        prop_assume!(!$a.contains('('));
        prop_assume!(!$a.contains(')'));
        prop_assume!(!$a.starts_with('_') || !$a.ends_with('_'));
    };
}

// Checks if all strings each represent a symbol
#[macro_export]
macro_rules! assume_symbols {
	($($a:ident),*) => ($(assume_symbol!($a);)*)
}

// Saves having to construct a new `HashSet` each time.
#[macro_export]
lazy_static! {
    pub static ref CONCRETE_SYMBOLS: HashSet<String> = hashset! {
        "label_of".into(), "let".into(), ":=".into(), "->".into(), "true".into(), "false".into(), "assoc".into(), "right".into(), "left".into(), "prec".into(), "default".into(), ">".into(), "=>".into(), "exists_such_that".into()
    };
}

// A symbol is abstract if isn't the same as one of the concrete symbols
#[macro_export]
macro_rules! assume_abstract {
    ($a:ident) => {
        prop_assume!(!CONCRETE_SYMBOLS.contains(&$a))
    };
}

// Common pattern in tests where a concept is defined from a pair of symbols
#[macro_export]
macro_rules! let_definition {
    ($cont:ident, $a:ident, $b:ident, $c:ident) => {
        assume_abstract!($a);
        assume_symbols!($a, $b, $c);
        prop_assume!($a != $b);
        prop_assume!($a != $c);
        let let_command = format!("let {} := {} {}", $a, $b, $c);
        prop_assert_eq!($cont.execute(&let_command), "");
    };
}
