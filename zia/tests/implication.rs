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
extern crate simple_logger;
extern crate test;
extern crate zia;

use test::Bencher;
use zia::multi_threaded::NEW_CONTEXT;

#[test]
fn simple_condition() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("let a => b"), "");
    assert_eq!(context.execute("let a"), "");
    assert_eq!(context.execute("b"), "true");
}
#[test]
fn negated_condition() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("let a => (b -> false)"), "");
    assert_eq!(context.execute("let a"), "");
    assert_eq!(context.execute("b"), "false");
}

fn partial_order_transitivity() {
    let mut context = NEW_CONTEXT.clone();
    assert_eq!(context.execute("default > (prec let)"), "true");
}
#[test]
fn partial_order_transitivity_test() {
    partial_order_transitivity();
}

#[bench]
fn partial_order_transitivity_bench(b: &mut Bencher) {
    b.iter(|| {
        partial_order_transitivity();
    });
}
