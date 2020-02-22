//  Library for the Zia programming language.
// Copyright (C) 2018 to 2019 Charles Johnson
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
use crate::{
    context::is_variable,
    errors::{ZiaError, ZiaResult},
};
use std::{
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
    str::FromStr,
};

/// Represents syntax as a full binary tree and links syntax to concepts where possible.
#[derive(Clone, Debug)]
pub struct SyntaxTree {
    /// The root of this syntax tree, represented as a `String`.
    syntax: String,
    /// Index of the concept that the syntax may represent.
    concept: Option<usize>,
    /// This syntax tree may expand to two syntax trees or not expand further.
    expansion: Option<(Rc<SyntaxTree>, Rc<SyntaxTree>)>,
}

impl PartialEq<SyntaxTree> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Self) -> bool {
        self.to_string() == other.to_string()
    }
}

impl PartialEq<Rc<SyntaxTree>> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Rc<Self>) -> bool {
        self.to_string() == other.to_string() && self.concept == other.concept
    }
}

impl fmt::Display for SyntaxTree {
    /// Displays the same as the inside of an `SyntaxTree` variant.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.syntax)
    }
}

impl FromStr for SyntaxTree {
    type Err = ZiaError;

    fn from_str(syntax: &str) -> ZiaResult<Self> {
        Ok(Self {
            syntax: syntax.to_string(),
            concept: None,
            expansion: None,
        })
    }
}

impl Eq for SyntaxTree {}

impl Hash for SyntaxTree {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.syntax.hash(state);
        self.concept.hash(state);
    }
}

impl SyntaxTree {
    pub fn bind_concept(mut self, concept: usize) -> Self {
        self.concept = Some(concept);
        self
    }

    pub fn contains(&self, other: &Self) -> bool {
        if let Some((ref left, ref right)) = self.get_expansion() {
            other == left
                || other == right
                || left.contains(other)
                || right.contains(other)
        } else {
            false
        }
    }

    /// An expression does have an expansion while a symbol does not.
    pub fn get_expansion(&self) -> Option<(Rc<Self>, Rc<Self>)> {
        self.expansion.clone()
    }

    pub fn bind_pair(
        mut self,
        lefthand: &Rc<Self>,
        righthand: &Rc<Self>,
    ) -> Self {
        self.expansion = Some((lefthand.clone(), righthand.clone()));
        self
    }

    pub const fn get_concept(&self) -> Option<usize> {
        self.concept
    }

    pub fn is_variable(&self) -> bool {
        if is_variable(&self.syntax) {
            true
        } else if let Some((l, r)) = self.get_expansion() {
            is_variable(&l.syntax) || is_variable(&r.syntax)
        } else {
            false
        }
    }
}
