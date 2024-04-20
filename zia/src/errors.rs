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

use thiserror::Error;

use crate::associativity::Associativity;

pub type ZiaResult<T> = Result<T, ZiaError>;

/// All the expected ways a Zia command could be invalid.
#[derive(Debug, PartialEq, Eq, Clone, Error)]
pub enum ZiaError {
    /// When specifying a reduction rule that already exists.
    #[error("That reduction rule already exists.")]
    RedundantReduction,
    /// When specifying a definition that already exists.
    #[error("That definition already exists.")]
    RedundantComposition,
    /// When refactoring a symbol that hasn't been used.
    #[error("Relabelling something that doesn't yet exist has no effect.")]
    RedundantRefactor,
    /// When removing a definition from a concept with no definition.
    #[error("Removing a definition that doesn't exist is redundant.")]
    RedundantCompositionRemoval,
    /// When defining an expanded expression.
    #[error("Cannot define expressions.")]
    BadComposition,
    /// When the command would complete a cycle of chained reduction rules.
    #[error("Cannot allow a chain of reduction rules to loop.")]
    CyclicReduction,
    /// When syntax tree cannot be expanded further
    #[error("Cannot expand syntax further")]
    CannotExpandFurther,
    /// When syntax tree cannot be reduced further
    #[error("Cannot reduce syntax further")]
    CannotReduceFurther,
    /// When a concept is contained within the concept that it reduces to.
    #[error("Cannot reduce a concept to an expression containing itself.")]
    ExpandingReduction,
    /// When a required symbol is missing from a command
    #[error("Missing {}", symbol)]
    MissingSymbol {
        symbol: &'static str,
    },
    /// When a concept is contained within the normal form of its definition.
    #[error(
        "Cannot define a concept as an expression whose normal form contains itself."
    )]
    InfiniteComposition,
    /// When a command contains a pair of parentheses with no syntax inside.
    #[error("Parentheses need to contain a symbol or expression.")]
    EmptyParentheses,
    /// When the interpreter cannot determine the tree structure of an expression.
    #[error("Ambiguity due to lack of precedence or associativity defined for the symbols in that expression.")]
    AmbiguousExpression,
    #[error("Could not find lowest precendence for `{tokens:?}`")]
    LowestPrecendenceNotFound {
        tokens: Vec<String>,
    },
    #[error("Tokens with the lowest precendence don't all have the same associativity. The token `{token}` is `{associativity:?}` associative whereas the rest of the tokens are `{other_associativity:?}` associative")]
    DifferentAssociativityAmongstLowestPrecendenceTokens {
        token: String,
        associativity: Associativity,
        other_associativity: Associativity,
    },
    /// When trying to refactor a used symbol as another used symbol or expression.
    #[error(
        "Cannot define a used symbol as another used symbol or expression."
    )]
    CompositionCollision,
    /// When trying to define the composition of a concrete concept.
    #[error("Cannot set a definition of a concrete concept")]
    SettingCompositionOfConcrete,
    /// When trying to specify a reduction rule for a concrete concept.
    #[error("Cannot reduce a concrete concept")]
    ConcreteReduction,
    /// When trying to specify a reduction rule for a concept whose components reduce to something else.
    #[error("Concept is already composed of concepts with their own reduction rules.")]
    MultipleReductionPaths,
    /// When symbol is expected to be used by a concept but isn't.
    #[error("Symbol was expected to be used to label a concept but isn't.")]
    UnusedSymbol,
    #[error("Tried to label a concept without a concept of a label.")]
    NoLabelConcept,
    #[error("Cannot quantify over compound expressions or constant concepts")]
    CanOnlyQuantifyOverVariables,
    #[error("Multiple smart pointers to context delta")]
    MultiplePointersToDelta,
}
