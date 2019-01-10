/*  Library for the Zia programming language.
    Copyright (C) 2018  Charles Johnson

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
use constants::LABEL;
use std::{collections::HashSet, fmt, rc::Rc};

pub trait SyntaxReader<T>
where
    Self: GetLabel<T> + Combine<T>,
    T: GetDefinitionOf + GetDefinition + GetReduction + MaybeString,
{
    /// Expands syntax by definition of its associated concept.
    fn expand<
        U: MaybeConcept
            + MightExpand<U>
            + fmt::Display
            + Clone
            + Pair<U>
            + DisplayJoint
            + From<(String, Option<usize>)>,
    >(
        &self,
        ast: &Rc<U>,
    ) -> Rc<U> {
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) = self.read_concept(con).get_definition() {
                self.combine(
                    &self.expand(&self.to_ast::<U>(left)),
                    &self.expand(&self.to_ast::<U>(right)),
                )
            } else {
                self.to_ast::<U>(con)
            }
        } else if let Some((ref left, ref right)) = ast.get_expansion() {
            self.combine(&self.expand(left), &self.expand(right))
        } else {
            ast.clone()
        }
    }
    /// Reduces the syntax as much as possible (returns the normal form syntax).
    fn recursively_reduce<
        U: From<(String, Option<usize>)>
            + MightExpand<U>
            + Clone
            + Pair<U>
            + MaybeConcept
            + DisplayJoint,
    >(
        &self,
        ast: &Rc<U>,
    ) -> Rc<U> {
        match self.reduce(ast) {
            Some(ref a) => self.recursively_reduce(a),
            None => ast.clone(),
        }
    }
    /// Reduces the syntax by using the reduction rules of associated concepts.
    fn reduce<
        U: From<(String, Option<usize>)>
            + MightExpand<U>
            + Clone
            + Pair<U>
            + MaybeConcept
            + DisplayJoint,
    >(
        &self,
        ast: &Rc<U>,
    ) -> Option<Rc<U>> {
        match ast.get_concept() {
            Some(c) => self.reduce_concept::<U>(c),
            None => match ast.get_expansion() {
                None => None,
                Some((ref left, ref right)) => {
                    self.match_left_right::<U>(self.reduce(left), self.reduce(right), left, right)
                }
            },
        }
    }
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept<
        U: From<(String, Option<usize>)> + Clone + Pair<U> + MaybeConcept + DisplayJoint,
    >(
        &self,
        concept: usize,
    ) -> Option<Rc<U>> {
        match self.read_concept(concept).get_reduction() {
            None => match self.read_concept(concept).get_definition() {
                Some((left, right)) => {
                    let left_result = self.reduce_concept::<U>(left);
                    let right_result = self.reduce_concept::<U>(right);
                    self.match_left_right::<U>(
                        left_result,
                        right_result,
                        &self.to_ast::<U>(left),
                        &self.to_ast::<U>(right),
                    )
                }
                None => None,
            },
            Some(n) => Some(self.to_ast::<U>(n)),
        }
    }
    /// Returns the syntax for a concept.
    fn to_ast<U: From<(String, Option<usize>)> + Clone + Pair<U> + MaybeConcept + DisplayJoint>(
        &self,
        concept: usize,
    ) -> Rc<U> {
        match self.get_label(concept) {
            Some(s) => Rc::new(U::from((s, Some(concept)))),
            None => match self.read_concept(concept).get_definition() {
                Some((left, right)) => {
                    self.combine(&self.to_ast::<U>(left), &self.to_ast::<U>(right))
                }
                None => panic!("Unlabelled concept with no definition"),
            },
        }
    }
    /// Returns the updated branch of abstract syntax tree that may have had the left or right parts updated.
    fn match_left_right<U: Pair<U> + MaybeConcept + DisplayJoint>(
        &self,
        left: Option<Rc<U>>,
        right: Option<Rc<U>>,
        original_left: &Rc<U>,
        original_right: &Rc<U>,
    ) -> Option<Rc<U>> {
        match (left, right) {
            (None, None) => None,
            (Some(new_left), None) => Some(self.contract_pair::<U>(&new_left, original_right)),
            (None, Some(new_right)) => Some(self.contract_pair::<U>(original_left, &new_right)),
            (Some(new_left), Some(new_right)) => {
                Some(self.contract_pair::<U>(&new_left, &new_right))
            }
        }
    }
    /// Returns the abstract syntax from two syntax parts, using the label and concept of the composition of associated concepts if it exists.
    fn contract_pair<U: MaybeConcept + Pair<U> + DisplayJoint>(
        &self,
        lefthand: &Rc<U>,
        righthand: &Rc<U>,
    ) -> Rc<U> {
        let syntax = match (lefthand.get_concept(), righthand.get_concept()) {
            (Some(lc), Some(rc)) => {
                let maydef = self.find_definition(lc, rc);
                (
                    match maydef {
                        Some(def) => match self.get_label(def) {
                            Some(a) => a,
                            None => lefthand.display_joint() + " " + &righthand.display_joint(),
                        },
                        None => lefthand.display_joint() + " " + &righthand.display_joint(),
                    },
                    maydef,
                )
            }
            _ => (
                lefthand.display_joint() + " " + &righthand.display_joint(),
                None,
            ),
        };
        Rc::new(U::from_pair(syntax, lefthand, righthand))
    }
}

impl<S, T> SyntaxReader<T> for S
where
    S: GetLabel<T> + Combine<T>,
    T: GetDefinitionOf + GetDefinition + MaybeString + GetReduction,
{
}
pub trait Display<T>
where
    Self: GetLabel<T>,
    T: MaybeString + GetDefinitionOf + GetDefinition + GetReduction,
{
    fn display(&self, concept: usize) -> String {
        match self.read_concept(concept).get_string() {
            Some(s) => "\"".to_string() + &s + "\"",
            None => match self.get_label(concept) {
                Some(l) => l,
                None => match self.read_concept(concept).get_definition() {
                    Some((left, right)) => {
                        let mut left_string = self.display(left);
                        if left_string.contains(' ') {
                            left_string = "(".to_string() + &left_string;
                        }
                        let mut right_string = self.display(right);
                        if right_string.contains(' ') {
                            right_string += ")";
                        }
                        left_string + " " + &right_string
                    }
                    None => panic!("Unlabelled concept with no definition!"),
                },
            },
        }
    }
}

impl<S, T> Display<T> for S
where
    S: GetLabel<T>,
    T: MaybeString + GetDefinitionOf + GetDefinition + GetReduction,
{
}
pub trait GetLabel<T>
where
    T: MaybeString + GetDefinitionOf + GetDefinition + GetReduction,
    Self: GetNormalForm<T> + GetConceptOfLabel<T>,
{
    fn get_label(&self, concept: usize) -> Option<String> {
        match self.get_concept_of_label(concept) {
            None => None,
            Some(d) => match self.get_normal_form(d) {
                None => None,
                Some(n) => self.read_concept(n).get_string(),
            },
        }
    }
}

impl<S, T> GetLabel<T> for S
where
    T: MaybeString + GetDefinitionOf + GetDefinition + GetReduction,
    S: GetNormalForm<T> + GetConceptOfLabel<T>,
{
}
pub trait Combine<T>
where
    Self: FindDefinition<T>,
    T: GetDefinitionOf,
{
    fn combine<U: DisplayJoint + MaybeConcept + Pair<U> + Sized>(
        &self,
        ast: &Rc<U>,
        other: &Rc<U>,
    ) -> Rc<U> {
        let left_string = ast.display_joint();
        let right_string = other.display_joint();
        let definition = if let (Some(l), Some(r)) = (ast.get_concept(), other.get_concept()) {
            self.find_definition(l, r)
        } else {
            None
        };
        Rc::new(U::from_pair(
            (left_string + " " + &right_string, definition),
            ast,
            other,
        ))
    }
}

impl<S, T> Combine<T> for S
where
    T: GetDefinitionOf,
    S: FindDefinition<T>,
{
}
pub trait Label<T>
where
    T: GetDefinition + FindWhatReducesToIt,
    Self: FindWhatItsANormalFormOf<T>,
{
    fn get_labellee(&self, concept: usize) -> Option<usize> {
        let mut candidates: Vec<usize> = Vec::new();
        for label in self.find_what_its_a_normal_form_of(concept) {
            match self.read_concept(label).get_definition() {
                None => continue,
                Some((r, x)) => {
                    if r == LABEL {
                        candidates.push(x)
                    } else {
                        continue;
                    }
                }
            };
        }
        match candidates.len() {
            0 => None,
            1 => Some(candidates[0]),
            _ => panic!("Multiple concepts are labelled with the same string"),
        }
    }
}

impl<S, T> Label<T> for S
where
    S: FindWhatItsANormalFormOf<T>,
    T: GetDefinition + FindWhatReducesToIt,
{
}
pub trait GetNormalForm<T>
where
    T: GetReduction,
    Self: ConceptReader<T>,
{
    fn get_normal_form(&self, concept: usize) -> Option<usize> {
        match self.read_concept(concept).get_reduction() {
            None => None,
            Some(n) => match self.get_normal_form(n) {
                None => Some(n),
                Some(m) => Some(m),
            },
        }
    }
}

impl<S, T> GetNormalForm<T> for S
where
    S: ConceptReader<T>,
    T: GetReduction,
{
}

pub trait GetConceptOfLabel<T>
where
    T: GetDefinition + GetDefinitionOf,
    Self: ConceptReader<T>,
{
    fn get_concept_of_label(&self, concept: usize) -> Option<usize> {
        for candidate in self.read_concept(concept).get_righthand_of().iter() {
            match self.read_concept(*candidate).get_definition() {
                None => panic!("Candidate should have a definition!"),
                Some((left, _)) => {
                    if left == LABEL {
                        return Some(*candidate);
                    }
                }
            };
        }
        None
    }
}

impl<S, T> GetConceptOfLabel<T> for S
where
    T: GetDefinition + GetDefinitionOf,
    S: ConceptReader<T>,
{
}

pub trait MaybeDisconnected<T>
where
    T: GetReduction + FindWhatReducesToIt + GetDefinition + GetDefinitionOf,
    Self: ConceptReader<T>,
{
    fn is_disconnected(&self, concept: usize) -> bool {
        self.read_concept(concept).get_reduction().is_none()
            && self.read_concept(concept).get_definition().is_none()
            && self.read_concept(concept).get_lefthand_of().is_empty()
            && self.righthand_of_without_label_is_empty(concept)
            && self
                .read_concept(concept)
                .find_what_reduces_to_it()
                .is_empty()
    }
    fn righthand_of_without_label_is_empty(&self, con: usize) -> bool {
        for concept in self.read_concept(con).get_righthand_of().iter() {
            if let Some((left, _)) = self.read_concept(*concept).get_definition() {
                if left != LABEL {
                    return false;
                }
            }
        }
        true
    }
}

impl<S, T> MaybeDisconnected<T> for S
where
    T: GetReduction + FindWhatReducesToIt + GetDefinition + GetDefinitionOf,
    S: ConceptReader<T>,
{
}

pub trait FindDefinition<T>
where
    T: GetDefinitionOf,
    Self: ConceptReader<T>,
{
    fn find_definition(&self, lefthand: usize, righthand: usize) -> Option<usize> {
        let has_lefthand = self.read_concept(lefthand).get_lefthand_of();
        let has_righthand = self.read_concept(righthand).get_righthand_of();
        let mut candidates = has_lefthand.intersection(&has_righthand);
        match candidates.next() {
            None => None,
            Some(index) => match candidates.next() {
                None => Some(*index),
                Some(_) => {
                    panic!("Multiple definitions with the same lefthand and righthand pair exist.")
                }
            },
        }
    }
}

impl<S, T> FindDefinition<T> for S
where
    T: GetDefinitionOf,
    S: ConceptReader<T>,
{
}

pub trait FindWhatItsANormalFormOf<T>
where
    T: FindWhatReducesToIt,
    Self: ConceptReader<T>,
{
    fn find_what_its_a_normal_form_of(&self, con: usize) -> HashSet<usize> {
        let mut normal_form_of = self.read_concept(con).find_what_reduces_to_it();
        for concept in normal_form_of.clone().iter() {
            for concept2 in self.find_what_its_a_normal_form_of(*concept).iter() {
                normal_form_of.insert(*concept2);
            }
        }
        normal_form_of
    }
}

impl<S, T> FindWhatItsANormalFormOf<T> for S
where
    S: ConceptReader<T>,
    T: FindWhatReducesToIt,
{
}

pub trait Container<T>
where
    Self: ConceptReader<T>,
    T: GetDefinition,
{
    fn contains(&self, outer: usize, inner: usize) -> bool {
        if let Some((left, right)) = self.read_concept(outer).get_definition() {
            left == inner
                || right == inner
                || self.contains(left, inner)
                || self.contains(right, inner)
        } else {
            false
        }
    }
}

impl<S, T> Container<T> for S
where
    S: ConceptReader<T>,
    T: GetDefinition,
{
}

pub trait ConceptReader<T> {
    fn read_concept(&self, usize) -> &T;
}
