//  Library for the Zia programming language.
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
extern crate zia;

use zia::NEW_CONTEXT;

#[test]
fn single_variable_reduction() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("let _a_ b -> c"), "");
    assert_eq!(context.execute("d b"), "c");
}
#[test]
fn repeated_variable_reduction() {
    let mut context = NEW_CONTEXT.clone();
    // Define how a + a can be written as 2 a
    assert_eq!(context.execute("let _a_ + _a_ -> 2 _a_"), "");
    // Check whether a + b -> 2 a if a = b
    assert_eq!(context.execute("b + b"), "2 b");
    // Check whether a + b doesn't reduce if a != b
    assert_eq!(context.execute("b + c"), "b + c");
}
