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
use zia::{Context, ContextMaker, ZiaError};

proptest! {
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
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        reduce_pair!(cont, d, e, f);
        let_definition!(cont, g, c, f);
        let print = format!("({} {}) {} {}", a, b, d, e);
        assert_eq!(
            cont.execute(&print),
            g
        );
    }
    // The interpreter should not allow a definition of a concept in terms of concepts that may reduce to the former concept.
    #[test]
    fn sneeky_infinite_reduction_chain(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
        assume_abstract!(a);
        assume_symbols!(a, b, c, d);
        let mut cont = Context::new();
        let reduction = format!("let ({} {}) -> {}", c, d, a);
        assert_eq!(cont.execute(&reduction), "");
        let definition = format!("let {} := {} {} {}", a, b, c, d);
        assert_eq!(
            cont.execute(&definition),
            ZiaError::InfiniteDefinition.to_string()
        );
    }
    // A reduction defined for a concept which is used to compose labelled concepts should not be accepted by the interpreter.
    #[test]
    fn reducing_part_of_a_labelled_concept(
        a in "\\PC*",
        b in "\\PC*",
        c in "\\PC*",
        d in "\\PC*",
        e in "\\PC*",
    ) {
        assume_abstract!(a);
        assume_symbols!(a, b, c, d, e);
        prop_assume!(a != b && a != c && a != d);
        let mut cont = Context::new();
        let definition = format!("let {} := {} {} {}", a, b, c, d);
        assert_eq!(cont.execute(&definition), "");
        let reduction = format!("let ({} {}) -> {}", c, d, e);
        assert_eq!(
            cont.execute(&reduction),
            ""
        );
    }
}
