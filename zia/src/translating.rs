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

use delta::ApplyDelta;
use reading::{BindConcept, FindWhatReducesToIt, GetDefinition, Label};
use std::{fmt::Debug, str::FromStr};

pub trait SyntaxFinder<T>
where
    Self: StringConcept + Label,
    T: FindWhatReducesToIt + GetDefinition,
{
    fn concept_from_label(&self, deltas: &Self::Delta, s: &str) -> Option<usize>;
    fn ast_from_symbol<U: FromStr + BindConcept>(&self, deltas: &Self::Delta, s: &str) -> U
    where
        <U as FromStr>::Err: Debug;
}

pub trait StringConcept
where
    Self: ApplyDelta,
{
    fn get_string_concept(&self, &Self::Delta, &str) -> Option<usize>;
}
