/*  Library for the Zia programming language.
    Copyright (C) 2018  Charles Johnson

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
use std::rc::Rc;

pub trait DisplayJoint {
    fn display_joint(&self) -> String;
}

pub trait MaybeConcept {
    fn get_concept(&self) -> Option<usize>;
}

pub trait Pair<U> {
    fn from_pair((String, Option<usize>), &Rc<U>, &Rc<U>) -> Self;
}

pub trait MightExpand<U> {
    fn get_expansion(&self) -> Option<(Rc<U>, Rc<U>)>;
}
