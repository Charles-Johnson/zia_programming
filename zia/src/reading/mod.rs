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

mod concepts;
mod syntax;

pub use self::concepts::*;
pub use self::syntax::*;
use delta::ApplyDelta;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

pub trait SyntaxReader<U>: ApplyDelta {
    /// Expands syntax by definition of its associated concept.
    fn expand(&self, deltas: &Self::Delta, ast: &Rc<U>) -> Rc<U>;
    /// Reduces the syntax as much as possible (returns the normal form syntax).
    fn recursively_reduce(&self, deltas: &Self::Delta, ast: &Rc<U>) -> Rc<U>;
    fn determine_reduction_truth(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> Option<bool>;
    fn determine_evidence_of_reduction(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> Option<bool>;
    /// Reduces the syntax by using the reduction rules of associated concepts.
    fn reduce(
        &self,
        deltas: &Self::Delta,
        ast: &Rc<U>,
        variable_mask: &HashMap<usize, Rc<U>>,
    ) -> Option<Rc<U>>;
    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
        variable_mask: &HashMap<usize, Rc<U>>,
    ) -> Option<Rc<U>>;
    fn filter_generalisations_for_pair(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> Vec<(usize, HashMap<usize, Rc<U>>)>;
    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        rightleft: &Rc<U>,
        rightright: &Rc<U>,
    ) -> Option<Rc<U>>;
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(
        &self,
        deltas: &Self::Delta,
        concept: usize,
        variable_mask: &HashMap<usize, Rc<U>>,
    ) -> Option<Rc<U>>;
    /// Returns the syntax for a concept.
    fn to_ast(&self, deltas: &Self::Delta, concept: usize) -> Rc<U>;
    fn combine(&self, deltas: &Self::Delta, ast: &Rc<U>, other: &Rc<U>) -> Rc<U>;
    fn join(&self, deltas: &Self::Delta, left: &Rc<U>, right: &Rc<U>) -> U;
    fn display_joint(&self, deltas: &Self::Delta, left: &Rc<U>, right: &Rc<U>) -> String;
    fn get_associativity(&self, deltas: &Self::Delta, ast: &Rc<U>) -> Option<Associativity>;
    fn has_higher_precedence(
        &self,
        deltas: &Self::Delta,
        left: &Rc<U>,
        right: &Rc<U>,
    ) -> Option<bool>;
    /// Returns the updated branch of abstract syntax tree that may have had the left or right parts updated.
    fn match_left_right(
        &self,
        deltas: &Self::Delta,
        left: Option<Rc<U>>,
        right: Option<Rc<U>>,
        original_left: &Rc<U>,
        original_right: &Rc<U>,
    ) -> Option<Rc<U>>;
    /// Returns the abstract syntax from two syntax parts, using the label and concept of the composition of associated concepts if it exists.
    fn contract_pair(&self, deltas: &Self::Delta, lefthand: &Rc<U>, righthand: &Rc<U>) -> Rc<U>;
}

#[derive(Debug, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

pub trait GetLabel: ApplyDelta {
    fn get_label(&self, deltas: &Self::Delta, concept: usize) -> Option<String>;
}

pub trait Label: ApplyDelta {
    fn get_labellee(&self, deltas: &Self::Delta, concept: usize) -> Option<usize>;
}

pub trait GetConceptOfLabel: ApplyDelta {
    fn get_concept_of_label(&self, deltas: &Self::Delta, concept: usize) -> Option<usize>;
}

pub trait MaybeDisconnected: ApplyDelta {
    fn is_disconnected(&self, deltas: &Self::Delta, concept: usize) -> bool;
    fn righthand_of_without_label_is_empty(&self, deltas: &Self::Delta, con: usize) -> bool;
}

pub trait FindDefinition: ApplyDelta {
    fn find_definition(
        &self,
        deltas: &Self::Delta,
        lefthand: usize,
        righthand: usize,
    ) -> Option<usize>;
}

pub trait FindWhatItsANormalFormOf: ApplyDelta {
    fn find_what_its_a_normal_form_of(&self, deltas: &Self::Delta, con: usize) -> HashSet<usize>;
}

pub trait Container: ApplyDelta {
    fn contains(&self, deltas: &Self::Delta, outer: usize, inner: usize) -> bool;
}

pub trait BindConcept {
    fn bind_concept(self, usize) -> Self;
}

pub trait Variable
where
    Self: ApplyDelta,
{
    fn has_variable(&self, deltas: &Self::Delta, usize) -> bool;
}
