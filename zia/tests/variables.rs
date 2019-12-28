/*  Library for the Zia programming language.
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
extern crate proptest;
extern crate zia;

use zia::Context;

proptest! {
    #[test]
    fn single_variable_reduction(a in "a|b|c|d", b in "a|b|c|d", c in "a|b|c|d", d in "a|b|c|d") {
        let mut context = Context::new();
        assert_eq!(context.execute(&format!("let (_{}_ {}) -> {}", a, b, c)), "");
        assert_eq!(context.execute(&format!("{} {}", d, b)), c);
    }
    #[test]
    fn repeated_variable_reduction(a in "a|b|c", b in "a|b|c", c in "a|b|c") {
        prop_assume!(b != c);
        let mut context = Context::new();
        // Define how a + a can be written as 2 a
        assert_eq!(context.execute(&format!("let (_{0}_ + _{0}_) -> 2 _{0}_", a)), "");
        // Check whether a + b -> 2 a if a = b
        assert_eq!(context.execute(&format!("{0} + {0}", b)), format!("2 {}", b));
        // Check whether a + b doesn't reduce if a != b
        assert_eq!(context.execute(&format!("{} + {}", b, c)), format!("{} + {}", b, c));
    }
}
