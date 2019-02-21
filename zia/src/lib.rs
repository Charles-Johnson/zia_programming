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

//! # Interpreter for the Zia programming language
//! The Zia project aims to develop a programming language that can be used to program itself. 
//! Instead of storing the source code as plain text and editing the raw text (which can easily break 
//! the program), the runtime environment of the interpreter (the `Context`) can be saved to disk and 
//! used in other programs. All the programming is done using an interactive shell such as
//! [IZia](https://github.com/Charles-Johnson/zia_programming/tree/master/izia). The commands sent are interpreted based on the 
//! `Context`. They are used to incrementally modify, test and debug the `Context`.  
//!
//! Expressions for Zia commands represent a binary tree where parentheses group a pair of expressions 
//! and a space separates a pair of expressions. For example `"(ll lr) (rl rr)"` represents a perfect binary tree of height 2 with leaves `"ll"`, `"lr"`, `"rl"`, `"rr"` going from left to right.
//! 
//! The leaves of the tree can be any unicode string without spaces or parentheses. These symbols may 
//! be recognised by the intepreter as concepts or if not used to label new concepts.
//!
//! Currently, only the lowest-level functionality has been implemented. It's important that programs
//! are represented consistently and transparently within the `Context` in order to achieve a 
//! self-describing system. The syntax shown below may appear awkward but more convenient syntax will 
//! be possible once more functionality is added. For example, the need to group pairs of expressions 
//! in parentheses will be alleviated by functionality to set the relative precedence and associativity
//! of concepts. 
//! 
//! So far there are 4 built-in concepts. A new `Context` labels these with the symbols, `"label_of"`, `"->"`, `":="`, `"let"`, but the labels can be changed to different symbols (e.g. for different languages or disciplines).
//!
//! # Examples
//!
//! ```
//! extern crate zia;
//! use zia::{Context, ContextMaker, Execute, ZiaError};
//!
//! // Construct a new `Context` using the `new` method of the `ContextMaker` trait
//! let mut context = Context::new();
//!
//! // Specify the rule that the concept "a" reduces to concept "b"
//! context.execute("let (a (-> b))");
//! assert_eq!(context.execute("(label_of (a ->)) ->"), "b");
//!
//! // Change the rule so that concept "a" instead reduces to concept "c"
//! context.execute("let (a (-> c))");
//! assert_eq!(context.execute("(label_of (a ->)) ->"), "c");
//!
//! // Change the rule so "a" doesn't reduce to anything
//! context.execute("let (a (-> a))");
//! assert_eq!(context.execute("(label_of (a ->)) ->"), "a");
//!
//! // Try to specify a rule that already exists 
//! assert_eq!(context.execute("let (a (-> a))"), ZiaError::RedundantReduction.to_string());
//! context.execute("let (a (-> b))");
//! assert_eq!(context.execute("let (a (-> b))"), ZiaError::RedundantReduction.to_string());
//!
//! // Specify the composition of "c"
//! context.execute("let(c (:= (a b)))");
//! assert_eq!(context.execute("(label_of (c :=)) ->"), "a b");
//!
//! // Print the label of the expansion of a concept with no composition 
//! assert_eq!(context.execute("(label_of (a :=)) ->"), "a");
//!
//! // Relabel "b" to "e"
//! context.execute("let (e (:= b))");
//! assert_eq!(context.execute("(label_of (c :=)) ->"), "a e");
//! assert_eq!(context.execute("(label_of (c ->)) ->"), "e e");
//!
//! // Try to specify the rule to reduce a concept whose composition reduces to something else
//! assert_eq!(context.execute("let (c (-> f))"), ZiaError::MultipleReductionPaths.to_string());
//!
//! // Try to specify the composition of a concept in terms of itself
//! assert_eq!(context.execute("let (i (:= (i j)))"), ZiaError::InfiniteDefinition.to_string());
//!
//! // Try to specify the reduction of concept in terms of itself
//! assert_eq!(context.execute("let (i (-> (i j)))"), ZiaError::ExpandingReduction.to_string());
//!
//! // Try to specify the composition of a concept in terms of something that reduces to itself
//! assert_eq!(context.execute("let (e (:= (a d)))"), ZiaError::ExpandingReduction.to_string());
//! ```

#[macro_use]
extern crate slog;
extern crate slog_term;

#[cfg(test)]
#[macro_use]
extern crate proptest; 

/// Traits for adding concepts to the context.
mod adding;

/// Abstract syntax tree. Relates syntax to concepts.
mod ast;

/// The units that make up the context. Defined in terms of their relationship with other concepts.
mod concepts;

/// Integers that represent concrete concepts.
mod constants;

/// The container of concepts that coordinates adding, reading, writing and removing of concepts.
mod context;

/// The trait for describing incremental changes in state.
mod delta;

/// The errors that the users could make when making commands.
mod errors;

// Trait for logging.
mod logging;

/// Traits for reading concepts within the context.
mod reading;

/// Traits for removing concepts from the context.
mod removing;

/// Traits for the context to translate strings into abstract syntax trees.
mod translating;

/// Traits for writing concepts within the context.
mod writing;

pub use adding::ContextMaker;
use adding::{ConceptMaker, Container, ExecuteReduction, FindOrInsertDefinition, Labeller};
pub use ast::SyntaxTree;
use concepts::{AbstractPart, CommonPart, Concept};
use constants::{LABEL, DEFINE, LET, REDUCTION};
use context::Context as GenericContext;
pub use errors::ZiaError;
use errors::ZiaResult;
use logging::Logger;
use reading::{
    DisplayJoint, FindWhatReducesToIt, GetDefinition, GetDefinitionOf, GetLabel, GetReduction,
    MaybeConcept, MaybeString, MightExpand, Pair, SyntaxReader,
};
use removing::DefinitionDeleter;
use std::rc::Rc;
use translating::SyntaxConverter;
use writing::{
    MakeReduceFrom, NoLongerReducesFrom, RemoveAsDefinitionOf, RemoveDefinition, RemoveReduction,
    SetAsDefinitionOf, SetDefinition, SetReduction,
};

/// A container for adding, writing, reading and removing `Concept`s.
pub type Context = GenericContext<Concept>;

/// Executing a command based on a string to add, write, read, or remove contained concepts.  
pub trait Execute<T>
where
    Self: Call<T> + SyntaxConverter<T> + Logger,
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString
        + GetDefinitionOf
        + GetReduction
        + FindWhatReducesToIt,
    Self::S: Container
        + Pair<Self::S>
        + Clone
        + From<(String, Option<usize>)>
        + DisplayJoint
        + PartialEq<Self::S>,
{
    fn execute(&mut self, command: &str) -> String {
        trace!(self.logger(), "execute({})", command);
        let ast = match self.ast_from_expression(command) {
            Ok(a) => a,
            Err(e) => return e.to_string(),
        };
        match self.call(&ast) {
            Ok(s) => s,
            Err(e) => e.to_string(),
        }
    }
}

impl<S, T> Execute<T> for S
where
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString
        + GetDefinitionOf
        + GetReduction
        + FindWhatReducesToIt,
    S: Call<T> + SyntaxConverter<T> + Logger,
    S::S: Container
        + Pair<S::S>
        + Clone
        + From<(String, Option<usize>)>
        + DisplayJoint
        + PartialEq<Self::S>,
{
}

impl FindOrInsertDefinition<Concept> for Context {
    /// When a specific composition of concepts does not exist as its own concept, a new abstract concept is defined as that composition.
    type A = AbstractPart;
}

impl Labeller<Concept> for Context {
    /// The `setup` method labels concrete concepts.
    type C = CommonPart;
}

impl ConceptMaker<Concept> for Context {
    /// New concepts are made from syntax trees.
    type S = SyntaxTree;
}

/// Calling a program expressed as a syntax tree to read or write contained concepts.  
pub trait Call<T>
where
    Self: Definer<T> + ExecuteReduction<T> + SyntaxReader<T>,
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + FindWhatReducesToIt
        + GetReduction
        + GetDefinition
        + GetDefinitionOf
        + MaybeString,
    Self::S: Container
        + Pair<Self::S>
        + Clone
        + From<(String, Option<usize>)>
        + DisplayJoint
        + PartialEq<Self::S>,
{
    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&mut self, ast: &Rc<Self::S>) -> ZiaResult<String> {
		if let Some(c) = ast.get_concept() {
			if let Some(s) = self.read_concept(&vec!(), c).get_string() {
				return Ok(s);
			}
		}
        match ast.get_expansion() {
            Some((ref left, ref right)) => self.call_pair(left, right),
            None => self.try_expanding_then_call(ast),
        }
    }
    /// If the associated concept of the lefthand part of the syntax tree is LET then `call_as_righthand` is called with the left and right of the lefthand syntax. Tries to get the concept associated with the righthand part of the syntax. If the associated concept is `->` then `call` is called with the reduction of the lefthand part of the syntax. Otherwise `Err(ZiaError::NotAProgram)` is returned.
    fn call_pair(&mut self, left: &Rc<Self::S>, right: &Rc<Self::S>) -> ZiaResult<String> {
        if let Some(c) = left.get_concept() {
            if c == LET {
                if let Some((ref rightleft, ref rightright)) = right.get_expansion() {
                    return self.call_as_righthand(rightleft, rightright);
                }
            }
        }
        match right.get_concept() {
            Some(c) => match c {
                REDUCTION => {
					if let Some((leftleft, leftright)) = left.get_expansion() {
						if let Some(con) = leftleft.get_concept() {
							if con == LABEL {
								return self.reduce_label_of(&leftright);
							}
						}
					};
					let reduced_syntax = match self.reduce(left) {
                    	None => left.clone(),
                    	Some(rleft) => rleft,
                	};
					self.call(&reduced_syntax)
				},
                _ => Err(ZiaError::NotAProgram)
            },
            None => Err(ZiaError::NotAProgram),
        }
    }
	fn reduce_label_of(&self, ast: &Rc<Self::S>) -> ZiaResult<String> {
		if let Some((left, right)) = ast.get_expansion() {
			if let Some(concept) = right.get_concept() {
				if concept == REDUCTION {
					return self.reduce_label_of(&match self.reduce(&left) {
						None => left,
						Some(reduction) => reduction,
					});
				}
				if concept == DEFINE {
					return Ok(self.expand(&left).to_string());
				}
			}
		}
		Ok(ast.to_string())
	}
    /// If the abstract syntax tree can be expanded, then `call` is called with this expansion. If not then an `Err(ZiaError::NotAProgram)` is returned
    fn try_expanding_then_call(&mut self, ast: &Rc<Self::S>) -> ZiaResult<String> {
        let expansion = &self.expand(ast);
        if expansion != ast {
            self.call(expansion)
        } else {
            Err(ZiaError::NotAProgram)
        }
    }
    /// If the abstract syntax tree can be reduced, then `call` is called with this reduction. If not then an `Err(ZiaError::NotAProgram)` is returned
    fn try_reducing_then_call(&mut self, ast: &Rc<Self::S>) -> ZiaResult<String> {
        let normal_form = &self.recursively_reduce(ast);
        if normal_form != ast {
            self.call(normal_form)
        } else {
            Err(ZiaError::NotAProgram)
        }
    }
    /// If the righthand part of the syntax can be expanded, then `match_righthand_pair` is called. If not, `Err(ZiaError::NotAProgram)` is returned.
    fn call_as_righthand(&mut self, left: &Self::S, right: &Self::S) -> ZiaResult<String> {
        match right.get_expansion() {
            Some((ref rightleft, ref rightright)) => {
                self.match_righthand_pair(left, rightleft, rightright)
            }
            None => Err(ZiaError::NotAProgram),
        }
    }
    /// If the lefthand of the righthand part of the syntax is `->` then `execute_reduction` is called with the lefthand part and the righthand of the righthand part of the syntax. Similarly for `:=`, `execute_definition` is called. If the lefthand of the righthand part of the syntax is associated with a concept which isn't `->` or `:=` then if this concept reduces, `match_righthand_pair` is called with this reduced concept as an abstract syntax tree.
    fn match_righthand_pair(
        &mut self,
        left: &Self::S,
        rightleft: &Self::S,
        rightright: &Self::S,
    ) -> ZiaResult<String> {
        match rightleft.get_concept() {
            Some(c) => match c {
                REDUCTION => self.execute_reduction(left, rightright),
                DEFINE => self.execute_definition(left, rightright),
                _ => {
                    let rightleft_reduction = self.read_concept(&vec!(), c).get_reduction();
                    if let Some(r) = rightleft_reduction {
                        let ast = self.to_ast::<Self::S>(r);
                        self.match_righthand_pair(left, &ast, rightright)
                    } else {
                        Err(ZiaError::NotAProgram)
                    }
                }
            },
            None => Err(ZiaError::NotAProgram),
        }
    }
}

impl<S, T> Call<T> for S
where
    S: Definer<T> + ExecuteReduction<T> + SyntaxReader<T>,
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + FindWhatReducesToIt
        + GetReduction
        + GetDefinition
        + GetDefinitionOf
        + MaybeString,
    S::S: Container
        + Pair<S::S>
        + Clone
        + From<(String, Option<usize>)>
        + DisplayJoint
        + PartialEq<Self::S>,
{
}

/// Defining new syntax in terms of old syntax.
pub trait Definer<T>
where
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + FindWhatReducesToIt
        + GetReduction
        + GetDefinition
        + GetDefinitionOf
        + MaybeString,
    Self: GetLabel<T> + ConceptMaker<T> + DefinitionDeleter<T>,
    Self::S: Pair<Self::S> + Container,
{
    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteDefinition)`. Otherwise `define` is called.
    fn execute_definition(&mut self, new: &Self::S, old: &Self::S) -> ZiaResult<String> {
        if old.contains(new) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            try!(self.define(new, old));
            Ok("".to_string())
        }
    }
    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadDefinition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(&mut self, new: &Self::S, old: &Self::S) -> ZiaResult<()> {
        if new.get_expansion().is_some() {
            Err(ZiaError::BadDefinition)
        } else {
            match (new.get_concept(), old.get_concept(), old.get_expansion()) {
                (_, None, None) => Err(ZiaError::RedundantRefactor),
                (None, Some(b), None) => self.relabel(b, &new.to_string()),
                (None, Some(b), Some(_)) => {
                    if self.get_label(b).is_none() {
                        self.label(b, &new.to_string())
                    } else {
                        self.relabel(b, &new.to_string())
                    }
                }
                (None, None, Some((ref left, ref right))) => {
                    try!(self.define_new_syntax(new.to_string(), left, right));
                    Ok(())
                }
                (Some(a), Some(b), None) => {
                    if a == b {
                        self.cleanly_delete_definition(a)
                    } else {
                        Err(ZiaError::DefinitionCollision)
                    }
                }
                (Some(a), Some(b), Some(_)) => {
                    if a == b {
                        Err(ZiaError::RedundantDefinition)
                    } else {
                        Err(ZiaError::DefinitionCollision)
                    }
                }
                (Some(a), None, Some((ref left, ref right))) => self.redefine(a, left, right),
            }
        }
    }
    /// Defining a concept as a composition whose syntax is given by `left` and `right`. If the concept already has a definition, then the concepts of this composition are relabelled with `left` and `right`. Otherwise new concepts are made from `left` and `right` to define the concept.
    fn redefine(&mut self, concept: usize, left: &Self::S, right: &Self::S) -> ZiaResult<()> {
        if let Some((left_concept, right_concept)) = self.read_concept(&vec!(), concept).get_definition() {
            try!(self.relabel(left_concept, &left.to_string()));
            self.relabel(right_concept, &right.to_string())
        } else {
            let left_concept = try!(self.concept_from_ast(left));
            let right_concept = try!(self.concept_from_ast(right));
            try!(self.insert_definition(concept, left_concept, right_concept));
            Ok(())
        }
    }
    /// Unlabels a concept and gives it a new label.
    fn relabel(&mut self, concept: usize, new_label: &str) -> ZiaResult<()> {
        try!(self.unlabel(concept));
        self.label(concept, new_label)
    }
    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    fn define_new_syntax(
        &mut self,
        syntax: String,
        left: &Rc<Self::S>,
        right: &Rc<Self::S>,
    ) -> ZiaResult<usize> {
        let definition_concept =
            if let (Some(l), Some(r)) = (left.get_concept(), right.get_concept()) {
                self.find_definition(l, r)
            } else {
                None
            };
        let new_syntax_tree = Self::S::from_pair((syntax, definition_concept), left, right);
        self.concept_from_ast(&new_syntax_tree)
    }
}

impl<S, T> Definer<T> for S
where
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + RemoveDefinition
        + RemoveAsDefinitionOf
        + SetReduction
        + MakeReduceFrom
        + RemoveReduction
        + NoLongerReducesFrom
        + SetDefinition
        + SetAsDefinitionOf
        + FindWhatReducesToIt
        + GetReduction
        + GetDefinition
        + GetDefinitionOf
        + MaybeString,
    S: ConceptMaker<T> + GetLabel<T> + DefinitionDeleter<T>,
    S::S: Pair<S::S> + Container,
{
}
