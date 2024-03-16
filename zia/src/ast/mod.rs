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
#![allow(clippy::redundant_pub_crate)]

mod r#macro;
mod node;
mod r#trait;

pub use node::{Node as SyntaxNode, SyntaxLeaf};
pub(crate) use r#macro::impl_syntax_tree;
pub use r#trait::{SyntaxTree, ExampleSubstitutions};

pub fn is_variable(string: &str) -> bool {
    string.starts_with('_') && string.ends_with('_') && !string.contains(' ')
}
