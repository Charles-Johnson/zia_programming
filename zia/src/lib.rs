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
//! [IZia](https://github.com/Charles-Johnson/zia_programming/tree/master/izia). The commands sent are
//! interpreted based on the `Context`. They are used to incrementally modify, test and debug the `Context`.  
//!
//! Expressions for Zia commands represent a binary tree where parentheses group a pair of expressions
//! and a space separates a pair of expressions. For example `"(ll lr) (rl rr)"` represents a perfect
//! binary tree of height 2 with leaves `"ll"`, `"lr"`, `"rl"`, `"rr"` going from left to right.
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
//! So far there are 10 built-in concepts. A new `Context` labels these with the symbols, `"label_of"`,
//! `"->"`, `":="`, `"let"`, `"true"`, `"false"`, `"assoc"`, `"right"`, `"left"`, ">-", but the labels
//! can be changed to different symbols for different languages or disciplines.
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
//! // Specify the rule that the concept "a b" reduces to concept "c"
//! assert_eq!(context.execute("let (a b) -> c"), "");
//! assert_eq!(context.execute("a b"), "c");
//!
//! // Change the rule so that concept "a b" instead reduces to concept "d"
//! assert_eq!(context.execute("let (a b) -> d"), "");
//! assert_eq!(context.execute("a b"), "d");
//!
//! // Change the rule so "a b" doesn't reduce any further
//! assert_eq!(context.execute("let (a b) -> a b"), "");
//! assert_eq!(context.execute("a b"), "a b");
//!
//! // Try to specify a rule that already exists
//! assert_eq!(context.execute("let (a b) -> a b"), ZiaError::RedundantReduction.to_string());
//! assert_eq!(context.execute("let (a b) -> c"), "");
//! assert_eq!(context.execute("let (a b) -> c"), ZiaError::RedundantReduction.to_string());
//!
//! // Relabel "label_of" to "표시"
//! assert_eq!(context.execute("let 표시 := label_of"), "");
//! assert_eq!(context.execute("표시 a b"), "\'c\'");
//!
//! // You can reduce a labelled concept
//! assert_eq!(context.execute("let a -> d"), "");
//!
//! // Try to specify the composition of a concept in terms of itself
//! assert_eq!(context.execute("let b := a b"), ZiaError::InfiniteDefinition.to_string());
//!
//! // Try to specify the reduction of concept in terms of itself
//! assert_eq!(context.execute("let (c d) -> (c d) e"), ZiaError::ExpandingReduction.to_string());
//!
//! // Determine the truth of a reduction
//! assert_eq!(context.execute("a -> d"), "true");
//! assert_eq!(context.execute("d -> a"), "false");
//!
//! // A concept never reduces to itself
//! assert_eq!(context.execute("a -> a"), "false");
//!
//! // Cannot reduce a reduction expression between unrelated concepts
//! assert_eq!(context.execute("d -> f"), "d -> f");
//!
//! // Can ask whether a reduction is true or false
//! assert_eq!(context.execute("(a -> d) -> true"), "true");
//! assert_eq!(context.execute("(d -> a) -> false"), "true");
//!
//! // Let an arbitary symbol be true
//! assert_eq!(context.execute("let g"), "");
//! assert_eq!(context.execute("g"), "true");
//!
//! // Let an arbitary expression be true
//! assert_eq!(context.execute("let h i j"), "");
//! assert_eq!(context.execute("h i j"), "true");
//!
//! // Determine associativity of symbol
//! assert_eq!(context.execute("assoc a"), "right");
//! 
//! // Define patterns
//! assert_eq!(context.execute("let (_x_ and false) -> false"), "");
//! assert_eq!(context.execute("foo and false"), "false");
//! ```

#[macro_use]
extern crate maplit;
#[macro_use]
extern crate slog;
extern crate slog_term;
extern crate snafu;

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
use constants::{DEFINE, LABEL, LET, REDUCTION, TRUE};
use context::Context as GenericContext;
use delta::Delta;
pub use errors::ZiaError;
use errors::{map_err_variant, ZiaResult};
use logging::Logger;
use reading::{
    BindConcept, BindPair, FindWhatReducesToIt, GetDefinition, GetDefinitionOf, GetLabel,
    GetReduction, MaybeConcept, MaybeString, SyntaxReader,
};
use removing::DefinitionDeleter;
use std::{
    fmt::{Debug, Display},
    rc::Rc,
    str::FromStr,
    collections::HashMap,
};
use translating::SyntaxConverter;
use writing::{
    MakeReduceFrom, NoLongerReducesFrom, RemoveAsDefinitionOf, RemoveDefinition, RemoveReduction,
    SetAsDefinitionOf, SetDefinition, SetReduction,
};

/// A container for adding, writing, reading and removing `Concept`s.
pub type Context = GenericContext<Concept>;

impl Execute<Concept, SyntaxTree> for Context {}

impl Call<Concept, SyntaxTree> for Context {}

impl Definer<Concept, SyntaxTree> for Context {}

impl ConceptMaker<Concept, SyntaxTree> for Context {}

impl FindOrInsertDefinition<Concept> for Context {
    /// When a specific composition of concepts does not exist as its own concept, a new abstract concept is defined as that composition.
    type A = AbstractPart;
}

impl Labeller<Concept> for Context {
    /// The `setup` method labels concrete concepts.
    type C = CommonPart;
}

/// Executing a command based on a string to add, write, read, or remove contained concepts.  
pub trait Execute<T, U>
where
    Self: Call<T, U> + SyntaxConverter<T, U> + Logger,
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
        + FindWhatReducesToIt
        + Debug
        + Clone,
    U: Container
        + BindPair
        + Debug
        + Clone
        + FromStr
        + BindConcept
        + PartialEq<U>
        + Display
        + MaybeConcept,
    <U as FromStr>::Err: Debug,
    Self::Delta: Clone + Debug + Default + Delta,
{
    fn execute(&mut self, command: &str) -> String {
        info!(self.logger(), "execute({})", command);
        let mut delta = Self::Delta::default();
        let string = self
            .ast_from_expression(&delta, command)
            .and_then(|a| self.call(&mut delta, &a))
            .unwrap_or_else(|e| e.to_string());
        info!(self.logger(), "execute({}) -> {:?}", command, delta);
        self.apply(delta);
        string
    }
}

/// Calling a program expressed as a syntax tree to read or write contained concepts.  
pub trait Call<T, U>
where
    Self: Definer<T, U> + ExecuteReduction<T, U> + SyntaxReader<T, U> + Logger,
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
        + MaybeString
        + Debug
        + Clone,
    U: Container
        + BindPair
        + Clone
        + FromStr
        + BindConcept
        + PartialEq<U>
        + Debug
        + MaybeConcept
        + Display,
    <U as FromStr>::Err: Debug,
    Self::Delta: Clone + Debug + Default + Delta,
{
    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&self, delta: &mut Self::Delta, ast: &Rc<U>) -> ZiaResult<String> {
        match ast
            .get_concept()
            .and_then(|c| self.read_concept(delta, c).get_string())
        {
            Some(s) => Ok(s),
            None => match ast.get_expansion() {
                Some((ref left, ref right)) => map_err_variant(
                    self.call_pair(delta, left, right),
                    &ZiaError::CannotReduceFurther,
                    || {
                        map_err_variant(
                            self.try_reducing_then_call(delta, ast),
                            &ZiaError::CannotReduceFurther,
                            || Ok(self.contract_pair(delta, left, right).to_string()),
                        )
                    },
                ),
                None => map_err_variant(
                    self.try_reducing_then_call(delta, ast),
                    &ZiaError::CannotReduceFurther,
                    || {
                        map_err_variant(
                            self.try_expanding_then_call(delta, ast),
                            &ZiaError::CannotExpandFurther,
                            || Ok(ast.to_string()),
                        )
                    },
                ),
            },
        }
    }
    /// If the associated concept of the lefthand part of the syntax tree is LET then `call_as_righthand` is called with the left and right of the lefthand syntax. Tries to get the concept associated with the righthand part of the syntax. If the associated concept is `->` then `call` is called with the reduction of the lefthand part of the syntax. Otherwise `Err(ZiaError::NotAProgram)` is returned.
    fn call_pair(
        &self,
        delta: &mut Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> ZiaResult<String> {
        left.get_concept()
            .and_then(|lc| match lc {
                LET => right
                    .get_expansion()
                    .and_then(|(left, right)| {
                        self.execute_let(delta, &left, &right)
                            .and_then(|x| match x {
                                Err(ZiaError::CannotReduceFurther) => None,
                                Err(ZiaError::UnusedSymbol) => None,
                                _ => Some(x),
                            })
                    })
                    .or_else(|| {
                        Some({
                            let true_syntax = self.to_ast(delta, TRUE);
                            self.execute_reduction(delta, right, &true_syntax)
                        })
                    })
                    .map(|r| r.map(|()| "".to_string())),
                LABEL => Some(Ok("'".to_string()
                    + &right
                        .get_concept()
                        .and_then(|c| self.get_label(delta, c))
                        .unwrap_or_else(|| right.to_string())
                    + "'")),
                _ => None,
            })
            .unwrap_or_else(|| match right.get_concept() {
                Some(c) if c == REDUCTION => self.try_reducing_then_call(delta, &left),
                _ => self.reduce_and_call_pair(delta, left, right),
            })
    }
    fn reduce_and_call_pair(
        &self,
        delta: &mut Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> ZiaResult<String> {
        let reduced_left = self.reduce(delta, left, &HashMap::new());
        let reduced_right = self.reduce(delta, right, &HashMap::new());
        match (reduced_left, reduced_right) {
            (None, None) => Err(ZiaError::CannotReduceFurther),
            (Some(rl), None) => self.call_pair(delta, &rl, right),
            (None, Some(rr)) => self.call_pair(delta, left, &rr),
            (Some(rl), Some(rr)) => self.call_pair(delta, &rl, &rr),
        }
    }
    /// If the abstract syntax tree can be expanded, then `call` is called with this expansion. If not then an `Err(ZiaError::NotAProgram)` is returned
    fn try_expanding_then_call(
        &self,
        delta: &mut Self::Delta,
        ast: &Rc<U>,
    ) -> ZiaResult<String> {
        let expansion = &self.expand(delta, ast);
        if expansion != ast {
            self.call(delta, expansion)
        } else {
            Err(ZiaError::CannotExpandFurther)
        }
    }
    /// If the abstract syntax tree can be reduced, then `call` is called with this reduction. If not then an `Err(ZiaError::CannotReduceFurther)` is returned
    fn try_reducing_then_call(
        &self,
        delta: &mut Self::Delta,
        ast: &Rc<U>,
    ) -> ZiaResult<String> {
        let normal_form = &self.recursively_reduce(delta, ast);
        if normal_form != ast {
            self.call(delta, normal_form)
        } else {
            Err(ZiaError::CannotReduceFurther)
        }
    }
    /// If the righthand part of the syntax can be expanded, then `match_righthand_pair` is called. If not, `Err(ZiaError::CannotExpandFurther)` is returned.
    fn execute_let(
        &self,
        delta: &mut Self::Delta,
        left: &U,
        right: &U,
    ) -> Option<ZiaResult<()>> {
        right
            .get_expansion()
            .map(|(ref rightleft, ref rightright)| {
                self.match_righthand_pair(delta, left, rightleft, rightright)
            })
    }
    /// If the lefthand of the righthand part of the syntax is `->` then `execute_reduction` is called with the lefthand part and the righthand of the
    /// righthand part of the syntax. Similarly for `:=`, `execute_definition` is called. If the lefthand of the righthand part of the syntax is associated
    /// with a concept which isn't `->` or `:=` then if this concept reduces, `match_righthand_pair` is called with this reduced concept as an abstract syntax tree.
    fn match_righthand_pair(
        &self,
        delta: &mut Self::Delta,
        left: &U,
        rightleft: &U,
        rightright: &U,
    ) -> ZiaResult<()> {
        match rightleft.get_concept() {
            Some(c) => match c {
                REDUCTION => self.execute_reduction(delta, left, rightright),
                DEFINE => self.execute_definition(delta, left, rightright),
                _ => {
                    let rightleft_reduction = self.read_concept(delta, c).get_reduction();
                    if let Some(r) = rightleft_reduction {
                        let ast = self.to_ast(delta, r);
                        self.match_righthand_pair(delta, left, &ast, rightright)
                    } else {
                        Err(ZiaError::CannotReduceFurther)
                    }
                }
            },
            None => Err(ZiaError::UnusedSymbol),
        }
    }
}

/// Defining new syntax in terms of old syntax.
pub trait Definer<T, U>
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
        + MaybeString
        + Debug
        + Clone,
    Self: GetLabel<T> + ConceptMaker<T, U> + DefinitionDeleter<T, U>,
    U: BindPair + Container + MaybeConcept + Display + FromStr + BindConcept,
    <U as FromStr>::Err: Debug,
    Self::Delta: Clone + Debug + Default + Delta,
{
    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteDefinition)`. Otherwise `define` is called.
    fn execute_definition(&self, delta: &mut Self::Delta, new: &U, old: &U) -> ZiaResult<()> {
        if old.contains(new) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.define(delta, new, old)
        }
    }
    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadDefinition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(&self, delta: &mut Self::Delta, new: &U, old: &U) -> ZiaResult<()> {
        if new.get_expansion().is_some() {
            Err(ZiaError::BadDefinition)
        } else {
            match (new.get_concept(), old.get_concept(), old.get_expansion()) {
                (_, None, None) => Err(ZiaError::RedundantRefactor),
                (None, Some(b), None) => self.relabel(delta, b, &new.to_string()),
                (None, Some(b), Some(_)) => {
                    if self.get_label(delta, b).is_none() {
                        self.label(delta, b, &new.to_string())
                    } else {
                        self.relabel(delta, b, &new.to_string())
                    }
                }
                (None, None, Some((ref left, ref right))) => {
                    self.define_new_syntax(delta, &new.to_string(), left, right)
                }
                (Some(a), Some(b), None) => {
                    if a == b {
                        self.cleanly_delete_definition(delta, a)
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
                (Some(a), None, Some((ref left, ref right))) => {
                    self.redefine(delta, a, left, right)
                }
            }
        }
    }
    /// Defining a concept as a composition whose syntax is given by `left` and `right`. If the concept already has a definition, then the concepts of this composition are relabelled with `left` and `right`. Otherwise new concepts are made from `left` and `right` to define the concept.
    fn redefine(
        &self,
        delta: &mut Self::Delta,
        concept: usize,
        left: &U,
        right: &U,
    ) -> ZiaResult<()> {
        if let Some((left_concept, right_concept)) =
            self.read_concept(delta, concept).get_definition()
        {
            self.relabel(delta, left_concept, &left.to_string())?;
            self.relabel(delta, right_concept, &right.to_string())
        } else {
            let left_concept = self.concept_from_ast(delta, left)?;
            let right_concept = self.concept_from_ast(delta, right)?;
            self.insert_definition(delta, concept, left_concept, right_concept)
        }
    }
    /// Unlabels a concept and gives it a new label.
    fn relabel(
        &self,
        delta: &mut Self::Delta,
        concept: usize,
        new_label: &str,
    ) -> ZiaResult<()> {
        self.unlabel(delta, concept)?;
        self.label(delta, concept, new_label)
    }
    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    fn define_new_syntax(
        &self,
        delta: &mut Self::Delta,
        syntax: &str,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_then(|l| {
                right.get_concept().and_then(|r| {
                    self.find_definition(delta, l, r)
                        .map(|concept| syntax.parse::<U>().unwrap().bind_concept(concept))
                })
            })
            .unwrap_or_else(|| syntax.parse::<U>().unwrap())
            .bind_pair(left, right);
        self.concept_from_ast(delta, &new_syntax_tree)?;
        Ok(())
    }
}
