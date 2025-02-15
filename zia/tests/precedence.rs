//  Library for the Zia programming language.
// Copyright (C) 2020 Charles Johnson
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

use zia::multi_threaded::NEW_CONTEXT;

#[test]
fn lower_than_default_precedence() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("let default > prec b"), "");
    assert_eq!(context.execute("prec b"), "prec b");
    assert_eq!(context.execute("c d b"), "(c d) b");
}

#[test]
fn default_precedence() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("prec a"), "default");
}
