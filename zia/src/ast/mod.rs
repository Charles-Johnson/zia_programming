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
use reading::{MaybeConcept, MightExpand, Pair};
use std::{fmt, rc::Rc};

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

impl MaybeConcept for SyntaxTree {
    /// Gets the possible concept from the inside type of the variant.
    fn get_concept(&self) -> Option<usize> {
        self.concept
    }
}

impl PartialEq<SyntaxTree> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &SyntaxTree) -> bool {
        self.to_string() == other.to_string()
    }
}

impl PartialEq<Rc<SyntaxTree>> for SyntaxTree {
    /// `SyntaxTree`s are equal if the syntax they represent is the same.
    fn eq(&self, other: &Rc<SyntaxTree>) -> bool {
        self.to_string() == other.to_string()
    }
}

impl MightExpand<SyntaxTree> for SyntaxTree {
    /// An expression does have an expansion while a symbol does not.
    fn get_expansion(&self) -> Option<(Rc<SyntaxTree>, Rc<SyntaxTree>)> {
        self.expansion.clone()
    }
}

impl fmt::Display for SyntaxTree {
    /// Displays the same as the inside of an `SyntaxTree` variant.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.syntax)
    }
}

impl Pair for SyntaxTree {
    /// Combines a pair of syntax trees into a `SyntaxTree` whilst specifying the overall syntax and maybe a concept.
    fn from_pair(
        syntax: (String, Option<usize>),
        lefthand: &Rc<SyntaxTree>,
        righthand: &Rc<SyntaxTree>,
    ) -> SyntaxTree {
        SyntaxTree {
            syntax: syntax.0,
            concept: syntax.1,
            expansion: Some((lefthand.clone(), righthand.clone())),
        }
    }
}

impl From<(String, Option<usize>)> for SyntaxTree {
    /// Constructs a `Symbol` variant from the syntax string and a possible associated concept.  
    fn from(syntax: (String, Option<usize>)) -> SyntaxTree {
        SyntaxTree {
            syntax: syntax.0,
            concept: syntax.1,
            expansion: None,
        }
    }
}
