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

use constants::LABEL;
use delta::Delta;
use errors::{ZiaError, ZiaResult};
use reading::{FindDefinition, MaybeString, MightExpand};
use std::{fmt, rc::Rc};
use writing::{
    DeleteReduction, GetDefinition, GetDefinitionOf, GetNormalForm, GetReduction, InsertDefinition,
    MakeReduceFrom, MaybeConcept, NoLongerReducesFrom, RemoveReduction, SetAsDefinitionOf,
    SetDefinition, SetReduction, UpdateReduction,
};

pub trait ExecuteReduction<T>
where
    Self: ConceptMaker<T> + DeleteReduction<T>,
    T: SetReduction
        + From<Self::C>
        + From<Self::A>
        + MakeReduceFrom
        + GetDefinitionOf
        + From<String>
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString,
    Self::S: Container + PartialEq,
{
    fn execute_reduction(&mut self, syntax: &Self::S, normal_form: &Self::S) -> ZiaResult<String> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            try!(self.try_removing_reduction::<Self::S>(syntax));
            Ok("".to_string())
        } else {
            let syntax_concept = try!(self.concept_from_ast(syntax));
            let normal_form_concept = try!(self.concept_from_ast(normal_form));
            try!(self.update_reduction(syntax_concept, normal_form_concept));
            Ok("".to_string())
        }
    }
}

impl<S, T> ExecuteReduction<T> for S
where
    S: ConceptMaker<T> + DeleteReduction<T>,
    T: SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + From<Self::C>
        + From<Self::A>
        + From<String>
        + RemoveReduction
        + NoLongerReducesFrom
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinition
        + MaybeString,
    Self::S: Container + PartialEq<Self::S>,
{
}

pub trait Container
where
    Self: MightExpand<Self> + PartialEq<Rc<Self>> + Sized,
{
    fn contains(&self, other: &Self) -> bool {
        if let Some((ref left, ref right)) = self.get_expansion() {
            other == left || other == right || left.contains(other) || right.contains(other)
        } else {
            false
        }
    }
}

impl<T> Container for T where T: MightExpand<T> + PartialEq<Rc<T>> + Sized {}

pub trait ConceptMaker<T>
where
    T: From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinitionOf
        + GetDefinition
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString
        + GetReduction,
    Self: Labeller<T> + GetNormalForm<T>,
{
    type S: MightExpand<Self::S> + MaybeConcept + fmt::Display;
    fn concept_from_ast(&mut self, ast: &Self::S) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(string),
                Some((ref left, ref right)) => {
                    let mut leftc = try!(self.concept_from_ast(left));
                    let mut rightc = try!(self.concept_from_ast(right));
                    let concept = try!(self.find_or_insert_definition(leftc, rightc));
                    if !string.contains(' ') {
                        try!(self.label(concept, string));
                    }
                    Ok(concept)
                }
            }
        }
    }
}

/// Preparing a context by labelling concrete concepts.
pub trait ContextMaker<T>
where
    Self: Labeller<T> + Default,
    T: GetDefinitionOf
        + From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString,
{
    fn new() -> Self {
        let mut cont = Self::default();
        cont.setup().unwrap();
        cont
    }
}

impl<S, T> ContextMaker<T> for S
where
    S: Labeller<T> + Default,
    T: GetDefinitionOf
        + From<String>
        + From<Self::C>
        + From<Self::A>
        + SetReduction
        + MakeReduceFrom
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + MaybeString,
{
}

pub trait Labeller<T>
where
    T: SetReduction
        + MakeReduceFrom
        + From<String>
        + GetDefinitionOf
        + SetDefinition
        + SetAsDefinitionOf
        + GetReduction
        + GetDefinition
        + GetReduction
        + MaybeString
        + From<Self::C>
        + From<Self::A>,
    Self: StringMaker<T> + FindOrInsertDefinition<T> + UpdateReduction<T>,
{
    type C: Default;
    fn label(&mut self, concept: usize, string: &str) -> ZiaResult<()> {
        let definition = try!(self.find_or_insert_definition(LABEL, concept));
        let (string_id, deltas) = self.new_string(&vec!(), string);
        self.apply_all(deltas);
        self.update_reduction(definition, string_id)
    }
    fn new_labelled_default(&mut self, string: &str) -> ZiaResult<usize> {
        let (new_default, delta) = self.new_default::<Self::A>(&vec!());
        self.apply(delta);
        try!(self.label(new_default, string));
        Ok(new_default)
    }
    fn setup(&mut self) -> ZiaResult<()> {
        let mut deltas = Vec::<Self::Delta>::new();
        deltas.reserve(4);
        let (label_concept, delta1) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta1);
        let (define_concept, delta2) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta2);
        let (reduction_concept, delta3) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta3);
        let (let_concept, delta4) = self.new_default::<Self::C>(&deltas);
        deltas.push(delta4);
        self.apply_all(deltas);
		try!(self.label(label_concept, "label_of"));
        try!(self.label(define_concept, ":="));
        try!(self.label(reduction_concept, "->"));
        self.label(let_concept, "let")
    }
}

pub trait FindOrInsertDefinition<T>
where
    T: From<Self::A>
        + GetDefinition
        + GetReduction
        + SetDefinition
        + SetAsDefinitionOf
        + GetDefinitionOf,
    Self: DefaultMaker<T> + InsertDefinition<T> + FindDefinition<T>,
{
    type A: Default;
    fn find_or_insert_definition(&mut self, lefthand: usize, righthand: usize) -> ZiaResult<usize> {
        let pair = self.find_definition(lefthand, righthand);
        match pair {
            None => {
                let (definition, delta) = self.new_default::<Self::A>(&vec!());
                self.apply(delta);
                try!(self.insert_definition(definition, lefthand, righthand));
                Ok(definition)
            }
            Some(def) => Ok(def),
        }
    }
}

pub trait StringMaker<T>
where
    T: From<String>,
    Self: ConceptAdderDelta<T> + StringAdderDelta,
{
    fn new_string(&self, deltas: &Vec<Self::Delta>, string: &str) -> (usize, Vec<Self::Delta>) {
        let string_concept = string.to_string().into();
        let (index, concept_delta) = self.add_concept_delta(deltas, string_concept);
        let string_delta = self.add_string_delta(index, string);
        let deltas = vec!{concept_delta, string_delta};
        (index, deltas)
    }
}

impl<S, T> StringMaker<T> for S
where
    T: From<String>,
    S: ConceptAdderDelta<T> + StringAdderDelta,
{
}

pub trait DefaultMaker<T>
where
    Self: ConceptAdderDelta<T>,
{
    fn new_default<V: Default + Into<T>>(&self, deltas: &Vec<Self::Delta>) -> (usize, Self::Delta) {
        let concept: T = V::default().into();
        self.add_concept_delta(deltas, concept)
    }
}

impl<S, T> DefaultMaker<T> for S where S: ConceptAdderDelta<T> {}

#[cfg(test)]
mod tests {
	use adding::{StringMaker, Labeller, ContextMaker, FindOrInsertDefinition, Container, ConceptMaker};
    use ast::SyntaxTree;
	use context::Context;
	use concepts::Concept;
    use delta::Delta;
	use reading::{GetLabel, GetDefinition, ConceptReader, Pair};
    use std::rc::Rc;
	use translating::{StringConcept, SyntaxFinder};
	proptest! {
		#[test]
		fn getting_new_strings_id(string: String) {
			let mut cont = Context::<Concept>::default();
			let (new_id, deltas) = cont.new_string(&vec!(), &string);
            cont.apply_all(deltas);
			assert_eq!(cont.get_string_concept(&string), Some(new_id));
		}
		#[test]
		fn getting_label_of_new_labelled_default(string: String) {
			let mut cont = Context::<Concept>::default();
			let concept_id = try!(cont.new_labelled_default(&string));
			assert_eq!(cont.get_label(concept_id), Some(string));
		}
		#[test]
		fn the_same_pair_of_ids_has_the_same_definition(left in 0usize..4usize, right in 0usize..4usize) {
			let mut cont = Context::<Concept>::new();
			assert_eq!(
				try!(cont.find_or_insert_definition(left, right)), 
				try!(cont.find_or_insert_definition(left, right)),
			);
		}
		#[test]
		fn correct_definition(left in 0usize..4usize, right in 0usize..4usize) {
			let mut cont = Context::<Concept>::new();
			let def = try!(cont.find_or_insert_definition(left, right));
			assert_eq!(
				cont.read_concept(def).get_definition(), 
				Some((left, right)),
			);
		}
        #[test]
        fn syntax_contains_other_syntax(s1 in "\\PC", id1: usize, s2 in "\\PC", id2: usize, s3 in "\\PC", id3: usize) {
            let ast1 = SyntaxTree::from((s1, Some(id1)));
            let ast2 = SyntaxTree::from((s2, Some(id2)));
            let ast3 = SyntaxTree::from_pair((s3, Some(id3)), &Rc::new(ast1.clone()), &Rc::new(ast2.clone()));
            assert!(ast3.contains(&ast1));
            assert!(ast3.contains(&ast2));
        }
        #[test]
        fn concept_from_ast_has_the_right_label(s in "\\PC") {
            let ast = SyntaxTree::from((s.clone(), None));
            let mut cont = Context::default();
            let id = try!(cont.concept_from_ast(&ast));
            assert_eq!(cont.get_label(id), Some(s));
        }
    }
	#[test]
	fn getting_ids_of_concrete_concepts() {
		let cont = Context::<Concept>::new();
		assert_eq!(cont.concept_from_label("label_of"), Some(0));
		assert_eq!(cont.concept_from_label(":="), Some(1));
		assert_eq!(cont.concept_from_label("->"), Some(2));
		assert_eq!(cont.concept_from_label("let"), Some(3));
	}
}


pub trait StringAdderDelta 
where
    Self: Delta,
{
    fn add_string_delta(&self, usize, &str) -> Self::Delta;
}

pub trait StringAdder {
    fn add_string(&mut self, usize, &str);
}

pub trait ConceptAdderDelta<T> 
where
    Self: Delta,
{
    fn add_concept_delta(&self, &Vec<Self::Delta>, T) -> (usize, Self::Delta);
}

pub trait ConceptAdder<T> {
    fn add_concept(&mut self, T) -> usize;
}
