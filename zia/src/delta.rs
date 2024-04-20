use crate::{
    context_delta::{DirectConceptDelta, NestedDelta, SharedDelta},
    snap_shot::Reader,
};
use std::fmt::Debug;
//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019  Charles Johnson
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

pub trait Apply<SDCD>: Reader<SDCD>
where
    SDCD: Clone
        + AsRef<DirectConceptDelta<Self::ConceptId>>
        + From<DirectConceptDelta<Self::ConceptId>>
        + Debug,
{
    fn apply<
        D: SharedDelta<NestedDelta = NestedDelta<Self::ConceptId, SDCD, D>>,
    >(
        &mut self,
        _: NestedDelta<Self::ConceptId, SDCD, D>,
    );
}
