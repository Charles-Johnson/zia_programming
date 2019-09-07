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
along with this program. If not, see <http://www.gnu.org/licenses/>.*/

use snafu::Snafu;

pub type ZiaResult<T> = Result<T, ZiaError>;

pub fn map_err_variant<T, E, F>(
    result: Result<T, E>,
    error_variant: &E,
    result_on_error: F,
) -> Result<T, E>
where
    F: FnOnce() -> Result<T, E>,
    E: PartialEq + Clone,
{
    match result {
        Err(ref err) if err == error_variant => result_on_error(),
        _ => result,
    }
}

/// All the expected ways a Zia command could be invalid.
#[derive(Debug, PartialEq, Clone, Snafu)]
pub enum ZiaError {
    /// When specifying a reduction rule that already exists.
    #[snafu(display("That reduction rule already exists."))]
    RedundantReduction,
    /// When specifying a definition that already exists.
    #[snafu(display("That definition already exists."))]
    RedundantDefinition,
    /// When refactoring a symbol that hasn't been used.
    #[snafu(display("Relabelling something that doesn't yet exist has no effect."))]
    RedundantRefactor,
    /// When removing a definition from a concept with no definition.
    #[snafu(display("Removing a definition that doesn't exist is redundant."))]
    RedundantDefinitionRemoval,
    /// When defining an expanded expression.
    #[snafu(display("Cannot define expressions."))]
    BadDefinition,
    /// When the command would complete a cycle of chained reduction rules.
    #[snafu(display("Cannot allow a chain of reduction rules to loop."))]
    CyclicReduction,
    /// When syntax tree cannot be expanded further
    #[snafu(display("Cannot expand syntax further"))]
    CannotExpandFurther,
    /// When syntax tree cannot be reduced further
    #[snafu(display("Cannot reduce syntax further"))]
    CannotReduceFurther,
    /// When a concept is contained within the concept that it reduces to.  
    #[snafu(display("Cannot reduce a concept to an expression containing itself."))]
    ExpandingReduction,
    /// When a required symbol is missing from a command
    #[snafu(display("Missing {}", symbol))]
    MissingSymbol { symbol: &'static str },
    /// When a concept is contained within the normal form of its definition.
    #[snafu(display(
        "Cannot define a concept as an expression whose normal form contains itself."
    ))]
    InfiniteDefinition,
    /// When a command contains a pair of parentheses with no syntax inside.
    #[snafu(display("Parentheses need to contain a symbol or expression."))]
    EmptyParentheses,
    /// When the interpreter cannot determine the tree structure of an expression.
    #[snafu(display("Ambiguity due to lack of precedence or associativity defined for the symbols in that expression."))]
    AmbiguousExpression,
    /// When trying to refactor a used symbol as another used symbol or expression.
    #[snafu(display("Cannot define a used symbol as another used symbol or expression."))]
    DefinitionCollision,
    /// When trying to define the composition of a concrete concept.
    #[snafu(display("Cannot set a definition of a concrete concept"))]
    SettingDefinitionOfConcrete,
    /// When trying to specify a reduction rule for a concrete concept.
    #[snafu(display("Cannot reduce a concrete concept"))]
    ConcreteReduction,
    /// When trying to specify a reduction rule for a concept whose components reduce to something else.
    #[snafu(display("Concept is already composed of concepts with their own reduction rules."))]
    MultipleReductionPaths,
    /// When symbol is expected to be used by a concept but isn't.
    #[snafu(display("Symbol was expected to be used to label a concept but isn't."))]
    UnusedSymbol,
}
