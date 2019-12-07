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
use zia::Context;

proptest! {
    // A previously unused symbol cannotreduce
    #[test]
    fn fresh_symbol_is_not_a_program(a in "\\PC*") {
        assume_abstract!(a);
        assume_symbol!(a);
        let mut cont = Context::new();
        assert_eq!(cont.execute(&a), a);
    }
    // A pair of previously unused symbols cannot reduce
    #[test]
    fn fresh_pair_does_not_reduce(a in "\\PC*", b in "\\PC*") {
        assume_abstract!(a);
        assume_abstract!(b);
        assume_symbols!(a, b);
        let mut cont = Context::new();
        let command = format!("{} {}", a, b);
        assert_eq!(cont.execute(&command), command);
    }
    // An expression of previously unused symbols containing a nested pair cannot reduce
    #[test]
    fn fresh_nested_pair_does_not_reduce(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        assume_abstract!(a);
        assume_abstract!(b);
        assume_abstract!(c);
        assume_symbols!(a, b, c);
        let mut cont = Context::new();
        let command = format!("{} {} {}", a, b, c);
        assert_eq!(cont.execute(&command), command);
    }
    // A previously used symbol cannot reduce unless it is a reducible concepts.
    #[test]
    fn used_symbol_does_not_reduce(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        assert_eq!(cont.execute(&c), c);
    }
    // A pair of previously used symbols cannot reduce unless their concepts are composed of any reducible concepts.
    #[test]
    fn used_symbol_in_a_pair_does_not_reduce(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        prop_assume!(b != c);
        let command = format!("{} {}", a, c);
        assert_eq!(cont.execute(&command), command);
    }
    // An expression with a nested pair composed of previously used symbols cannot reduce unless their concepts are composed of any reducible concepts.
    #[test]
    fn used_symbol_in_a_nested_pair_does_not_reduce(
        a in "\\PC*",
        b in "\\PC*",
        c in "\\PC*",
    ) {
        assume_abstract!(c);
        prop_assume!((a != b) && (b != c) && (c != a));
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        let command = format!("{} {} {}", a, b, c);
        assert_eq!(cont.execute(&command), command);
    }
}
