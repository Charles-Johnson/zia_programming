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
use std::{
    fmt,
    hash::{Hash, Hasher},
    sync::Arc,
};

/// Represents syntax as a full binary tree and links syntax to concepts where possible.
#[derive(Clone, Debug)]
pub struct SyntaxTree {
    /// The root of this syntax tree, represented as a `String`.
    syntax: Option<String>,
    /// Index of the concept that the syntax may represent.
    concept: Option<usize>,
    /// This syntax tree may expand to two syntax trees or not expand further.
    expansion: Option<(Arc<SyntaxTree>, Arc<SyntaxTree>)>,
}

impl PartialEq<SyntaxTree> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Self) -> bool {
        if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
            ss == os
        } else if let (Some(sc), Some(oc)) = (&self.concept, &other.concept) {
            sc == oc
        } else {
            self.expansion == other.expansion
        }
    }
}

impl PartialEq<Arc<SyntaxTree>> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Arc<Self>) -> bool {
        if let (Some(ss), Some(os)) = (&self.syntax, &other.syntax) {
            ss == os
        } else {
            self.concept == other.concept
        }
    }
}

impl fmt::Display for SyntaxTree {
    /// Displays the same as the inside of an `SyntaxTree` variant.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.syntax.clone().unwrap_or_else(|| self
                .expansion
                .as_ref()
                .map_or_else(
                    || "".into(),
                    |(l, r)| l.to_string() + " " + &r.to_string()
                ))
        )
    }
}

impl<S> From<S> for SyntaxTree
where
    S: Into<String>,
{
    fn from(syntax: S) -> Self {
        Self {
            syntax: Some(syntax.into()),
            concept: None,
            expansion: None,
        }
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
    pub fn new_concept(concept_id: usize) -> Self {
        Self {
            syntax: None,
            concept: Some(concept_id),
            expansion: None,
        }
    }

    #[cfg(test)]
    pub fn new_pair(
        left: impl Into<Arc<SyntaxTree>>,
        right: impl Into<Arc<SyntaxTree>>,
    ) -> Self {
        Self {
            syntax: None,
            concept: None,
            expansion: Some((left.into(), right.into())),
        }
    }

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
    pub fn get_expansion(&self) -> Option<(Arc<Self>, Arc<Self>)> {
        self.expansion.clone()
    }

    pub fn bind_pair(
        mut self,
        lefthand: impl Into<Arc<Self>>,
        righthand: impl Into<Arc<Self>>,
    ) -> Self {
        self.expansion = Some((lefthand.into(), righthand.into()));
        self
    }

    pub const fn get_concept(&self) -> Option<usize> {
        self.concept
    }

    pub fn is_variable(&self) -> bool {
        if self.syntax.as_ref().map_or(false, |s| is_variable(s)) {
            true
        } else if let Some((l, r)) = self.get_expansion() {
            l.is_variable() || r.is_variable()
        } else {
            false
        }
    }
}

// impl fmt::Debug for SyntaxTree {
//     fn fmt(
//         &self,
//         formatter: &mut std::fmt::Formatter,
//     ) -> Result<(), std::fmt::Error> {
//         formatter.write_str(
//             self.syntax.clone().unwrap_or_else(|| "".into()).as_str(),
//         )
//     }
// }

pub fn is_variable(string: &str) -> bool {
    string.starts_with('_') && string.ends_with('_')
}
