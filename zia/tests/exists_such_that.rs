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

use zia::NEW_CONTEXT;

#[test]
fn some_concept_is_true() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("_x_ exists_such_that _x_"), "true");
}

#[test]
fn some_concepts_precedence_is_below_default() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("_x_ exists_such_that default > prec _x_"), "true")
}