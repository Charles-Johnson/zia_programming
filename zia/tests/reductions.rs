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

use zia::{Context, ContextMaker, Execute, ZiaError};

proptest! {
    // If a pair of symbols reduces to another symbol then the label of the reduction of that pair must be a string of that other symbol.
    #[test]
    fn pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        let print = format!("{} {}", a, b);
        prop_assert_eq!(cont.execute(&print), c);
    }
    // Checking whether two reduction rules can be correctly chained together for an expression with a nested pair.
    #[test]
    fn nested_pair_to_symbol(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        prop_assume!(b != c);
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        reduce_pair!(cont, a, c, b);
        let print = format!("{} {} {}", a, a, b);
        prop_assert_eq!(cont.execute(&print), b);
    }
    // A concept should not be able to reduce to a concept whose normal form is the former concept.
    #[test]
    fn cycle(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
        // to prevent redundant reduction
        prop_assume!(a != b);
        assume_symbols!(a, b, c, d);
        let mut cont = Context::new();
        let reduction0 = format!("let ({} {}) -> {} {}", a, b, c, d);
        assert_eq!(cont.execute(&reduction0), "");
        let reduction1 = format!("let ({} {}) -> {} {}", c, d, a, b);
        prop_assert_eq!(
            cont.execute(&reduction1),
            ZiaError::CyclicReduction.to_string()
        );
    }
    // If only a symbol is enclosed in parentheses, then this should be the same as if the parentheses weren't there
    #[test]
    fn trivial_parentheses(a in "\\PC*") {
        let mut cont = Context::new();
        assume_symbol!(a);
        let print = format!("({})", a);
        prop_assert_eq!(cont.execute(&print), a);
    }
    // A concept should not be able to reduce to something composed of that concept.
    #[test]
    fn infinite_expansion(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        assume_symbols!(a, b, c);
        let reduction = format!("let ({} {}) -> {} {} {}", a, b, c, a, b);
        let mut cont = Context::new();
        assert_eq!(
            cont.execute(&reduction),
            ZiaError::ExpandingReduction.to_string()
        );
    }
    // A concept that used to doubly reduce but whose first reduction no longer reduces should doubly reduce to its first reduction.
    #[test]
    fn broken_end_chain(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*", e in "\\PC*") {
        // to prevent rendundant reductions and looping reductions
        prop_assume!(a != b && b != c && c != a);
        assume_symbols!(a, b, c, d, e);
        let mut cont = Context::new();
        let reduction0 = format!("let ({} {}) -> {} {}", a, b, c, d);
        assert_eq!(cont.execute(&reduction0), "");
        let reduction1 = format!("let ({} {}) -> {}", c, d, e);
        assert_eq!(cont.execute(&reduction1), "");
        let reduction2 = format!("let ({} {}) -> {} {}", c, d, c, d);
        assert_eq!(cont.execute(&reduction2), "");
        let print = format!("{} {}", a, b);
        assert_eq!(cont.execute(&print), format!("{} {}", c, d));
    }
    // A concept that used to triply reduce but whose second reduction no longer reduces should triply reduce to its second reduction.
    #[test]
    fn broken_middle_chain(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*", e in "\\PC*", f in "\\PC*", g in "\\PC*") {
        // to prevent rendundant reductions and looping reductions
        prop_assume!(a != b && b != c && c != d && c != a && d != a && d != b);
        assume_symbols!(a, b, c, d, e, f, g);
        let mut cont = Context::new();
        let reduction0 = format!("let ({} {}) -> {} {}", a, b, c, d);
        assert_eq!(cont.execute(&reduction0), "");
        let reduction1 = format!("let ({} {}) -> {} {}", c, d, e, f);
        assert_eq!(cont.execute(&reduction1), "");
        let reduction2 = format!("let ({} {}) -> {}", e, f, g);
        assert_eq!(cont.execute(&reduction2), "");
        let reduction3 = format!("let ({} {}) -> {} {}", e, f, e, f);
        assert_eq!(cont.execute(&reduction3), "");
        let print = format!("{} {}", a, b);
        assert_eq!(cont.execute(&print), format!("{} {}", e, f));
    }
    // Checking that reduction rules can be changed correctly
    #[test]
    fn change_reduction_rule(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
        // to prevent rendundant reductions
        prop_assume!(a != b && b != c && c != d);
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        reduce_pair!(cont, a, b, d);
        let print = format!("{} {}", a, b);
        assert_eq!(cont.execute(&print), d);
    }
    // It is not necessarily redundant to make a concept reduce to its normal form.
    #[test]
    fn leapfrog_reduction_rule(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*", e in "\\PC*") {
        // `a`, `b` and `c` all need to be unique to prevent redundant reductions and looping reductions.
        prop_assume!(a != b && b != c && c != a);
        assume_symbols!(a, b, c, d);
        let mut cont = Context::new();
        let reduction = format!("let ({} {}) -> {} {}", a, b, c, d);
        assert_eq!(cont.execute(&reduction), "");
        reduce_pair!(cont, c, d, e);
        reduce_pair!(cont, a, b, e);
    }
    // It is redundant to specify a reduction rule that is already true
    #[test]
    fn redundancy(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
        prop_assume!(a != b);
        assume_symbol!(d);
        let mut cont = Context::new();
        reduce_pair!(cont, a, b, c);
        let reduction = format!("let ({} {}) -> {}", a, b, c);
        assert_eq!(
            cont.execute(&reduction),
            ZiaError::RedundantReduction.to_string()
        );
        let remove_reduction = format!("let ({} {}) -> {} {}", c, d, c, d);
        assert_eq!(
            cont.execute(&remove_reduction),
            ZiaError::RedundantReduction.to_string()
        );
    }
}
