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

use std::{error::Error, fmt};

pub type ZiaResult<T> = Result<T, ZiaError>;

/// All the expected ways a Zia command could be invalid.
#[derive(Debug)]
pub enum ZiaError {
    /// When specifying a reduction rule that already exists.
    RedundantReduction,
    /// When specifying a definition that already exists.
    RedundantDefinition,
    /// When refactoring a symbol that hasn't been used.
    RedundantRefactor,
    /// When removing a definition from a concept with no definition.
    RedundantDefinitionRemoval,
    /// When the command cannot be interpreted as a routine.
    NotAProgram,
    /// When defining an expanded expression.
    BadDefinition,
    /// When the command would complete a cycle of chained reduction rules.
    CyclicReduction,
    /// When a concept is contained within the concept that it reduces to.  
    ExpandingReduction,
    /// When a concept is contained within its definition.
    InfiniteDefinition,
    /// When a command contains a pair of parentheses with no syntax inside.
    EmptyParentheses,
    /// When the interpreter cannot determine the tree structure of an expression.
    AmbiguousExpression,
    /// When trying to refactor a used symbol as another used symbol or expression.
    DefinitionCollision,
    /// When trying to define the composition of a concrete concept.
    SettingDefinitionOfConcrete,
    /// When trying to specify a reduction rule for a concrete concept.
    ConcreteReduction,
    /// When trying to specify a reduction rule for a concept whose components reduce to something else.
    MultipleReductionPaths,
}

impl Error for ZiaError {
    fn description(&self) -> &str {
        "All the expected ways a Zia command could be invalid"
    }

    fn source(&self) -> Option<&(dyn Error + 'static)> {
        None
    }
}

impl fmt::Display for ZiaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
	        ZiaError::RedundantReduction => "That reduction rule already exists.",
			ZiaError::RedundantDefinition => "That definition already exists.",
			ZiaError::RedundantRefactor => "Relabelling something that doesn't yet exist has no effect.",
			ZiaError::RedundantDefinitionRemoval => "Removing a definition that doesn't exist is redundant.",
	        ZiaError::NotAProgram => "No program exists for this syntax.",
	        ZiaError::BadDefinition => "Cannot define expressions.",
	        ZiaError::CyclicReduction => "Cannot allow a chain of reduction rules to loop.",
	        ZiaError::ExpandingReduction => "Cannot reduce a concept to an expression containing itself.",
	        ZiaError::InfiniteDefinition => "Cannot define a concept as an expression containing itself.",
			ZiaError::EmptyParentheses => "Parentheses need to contain a symbol or expression.",
			ZiaError::AmbiguousExpression => "Ambiguity due to lack of precedence or associativity defined for the symbols in that expression.",
			ZiaError::DefinitionCollision => "Cannot define a used symbol as another used symbol or expression.",
			ZiaError::SettingDefinitionOfConcrete => "Cannot set a definition of a concrete concept",
			ZiaError::ConcreteReduction => "Cannot reduce a concrete concept", 
			ZiaError::MultipleReductionPaths => "Concept is already composed of concepts with their own reduction rules."
	    })
    }
}
