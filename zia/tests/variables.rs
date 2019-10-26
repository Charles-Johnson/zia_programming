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
#[macro_use]
extern crate test_zia;
extern crate zia;

use zia::{Context, ZiaError};

proptest!{
    #[test]
    fn single_variable_reduction(a in "\\PC*", b in "\\PC*", c in "\\PC*", d in "\\PC*") {
        assume_symbols!(a, b, c, d);
        let mut context = Context::new();
        assert_eq!(context.execute(&format!("let (_{}_ {}) -> {}", a, b, c)), "");
        assert_eq!(context.execute(&format!("{} {}", d, b)), c);
    }
    #[test]
    fn repeated_variable_reduction(a in "\\PC*", b in "\\PC*", c in "\\PC*") {
        assume_symbols!(a, b, c);
        let mut context = Context::new();
        assert_eq!(context.execute(&format!("let (_{0}_ + _{0}_) -> 2 _{0}_", a)), "");
        assert_eq!(context.execute(&format!("{0} + {0}", b)), format!("2 {}", b));
        assert_eq!(context.execute(&format!("{} + {}", b, c)), format!("{} {}", b, c));
    }
}