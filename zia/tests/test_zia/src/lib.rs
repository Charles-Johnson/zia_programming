/*  Testing library for the Zia programming language.
    Copyright (C) 2019 Charles Johnson

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
extern crate lazy_static;
extern crate proptest;

use std::collections::HashSet;

// Checks if a string can respresent a symbol
#[macro_export]
macro_rules! assume_symbol {
    ($a:ident) => {
        prop_assume!($a.len() > 0);
        prop_assume!(!$a.contains(' '));
        prop_assume!(!$a.contains('('));
        prop_assume!(!$a.contains(')'));
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
    pub static ref CONCRETE_SYMBOLS: HashSet<String> = {
        let mut cs = HashSet::new();
        cs.insert("label_of".to_string());
        cs.insert("let".to_string());
        cs.insert(":=".to_string());
        cs.insert("->".to_string());
        cs
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
        let let_command = format!("let ({} (:= ({} {})))", $a, $b, $c);
        prop_assert_eq!($cont.execute(&let_command), "");
    };
}
// Common pattern in tests where a concept's label is reduced to a pair
#[macro_export]
macro_rules! print_pair_expansion {
    ($cont:ident, $a:ident, $b:ident, $c:ident) => {
        let print_command = format!("(label_of ({} :=)) ->", $a);
        let result = format!("{} {}", $b, $c);
        prop_assert_eq!($cont.execute(&print_command), result);
    };
}

// Common pattern in tests where a symbol reduces to another symbol
#[macro_export]
macro_rules! let_reduction {
    ($cont:ident, $a:ident, $b:ident) => {
        assume_symbols!($a, $b);
        assume_abstract!($a);
        let reduction = format!("let ({} (-> {}))", $a, $b);
        prop_assert_eq!($cont.execute(&reduction), "");
    };
}
