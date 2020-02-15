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
#![feature(test)]
extern crate test;
extern crate zia;

use test::Bencher;
use zia::NEW_CONTEXT;

#[test]
fn simple_condition() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("let a => b"), "");
    assert_eq!(context.execute("let a"), "");
    assert_eq!(context.execute("b"), "true");
}

#[test]
fn partial_order_transitivity() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(
        context.execute(
            "let (_y_ exists_such_that (_x_ > _y_) and _y_ > _z_) => _x_ > _z_"
        ),
        ""
    );
    assert_eq!(context.execute("let a > b"), "");
    assert_eq!(context.execute("let b > c"), "");
    assert_eq!(context.execute("a > c"), "true");
}

#[bench]
fn partial_order_transitivity_bench(b: &mut Bencher) {
    b.iter(|| {
        let mut context = NEW_CONTEXT.clone();
        context.execute(
            "let (_y_ exists_such_that (_x_ > _y_) and _y_ > _z_) => _x_ > _z_"
        );
        context.execute("let a > b");
        context.execute("let b > c");
        context.execute("a > c");
    });
}