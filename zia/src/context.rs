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

use adding::Container as SyntaxContainer;
use ast::SyntaxTree;
use concepts::{AbstractPart, Concept, ConceptDelta as CD};
use constants::{ASSOC, DEFINE, FALSE, LABEL, LEFT, LET, PRECEDENCE, REDUCTION, RIGHT, TRUE};
use delta::{ApplyDelta, Delta};
use errors::{map_err_variant, ZiaError, ZiaResult};
use reading::{
    Associativity, BindConcept, BindPair, Container, FindDefinition,
    GetLabel, Label, MaybeConcept,
    MightExpand, SyntaxReader,
};
use slog;
use slog::Drain;
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    default::Default,
    fmt::Debug,
    rc::Rc,
    str::FromStr,
};
use writing::{
    DeleteReduction, InsertDefinition, MakeReduceFromDelta, SetAsDefinitionOfDelta,
    SetDefinitionDelta, SetReductionDelta, UpdateReduction,
};

/// A container for adding, reading, writing and removing concepts of generic type `T`.
pub struct Context {
    /// Relates a String value to the index where the concept corresponding to the String is stored
    /// in the `concepts` field.
    string_map: HashMap<String, usize>,
    /// Concepts may be stored at an index of this vector as `Some(T)`. If that concept is removed
    /// from the context, `None` will be left at its index.
    concepts: Vec<Option<Concept>>,
    /// Keeps track of indices of the `concepts` field that have `None`.
    gaps: Vec<usize>,
    logger: slog::Logger,
    variables: HashSet<usize>,
}

impl Context {
    pub fn execute(&mut self, command: &str) -> String {
        info!(self.logger, "execute({})", command);
        let mut delta = ContextDelta::default();
        let string = self
            .ast_from_expression(&delta, command)
            .and_then(|a| self.call(&mut delta, &a))
            .unwrap_or_else(|e| e.to_string());
        info!(self.logger, "execute({}) -> {:?}", command, delta);
        self.apply(delta);
        string
    }
    fn has_variable(&self, delta: &ContextDelta, concept: usize) -> bool {
        let in_previous_variables = self.variables.contains(&concept);
        delta
            .concept
            .get(&concept)
            .map(|(cd, v)| match cd {
                ConceptDelta::Insert(_) => *v,
                ConceptDelta::Remove(_) => false,
                ConceptDelta::Update(_) => in_previous_variables,
            })
            .unwrap_or(in_previous_variables)
    }
    fn get_concept(&self, id: usize) -> Option<&Concept> {
        match self.concepts.get(id) {
            Some(Some(c)) => Some(c),
            _ => None,
        }
    }
    fn write_concept(&mut self, id: usize) -> &mut Concept {
        match self.concepts[id] {
            Some(ref mut c) => c,
            None => panic!("No concept with id = {}", id),
        }
    }
    fn read_concept(&self, delta: &ContextDelta, id: usize) -> Concept {
        delta
            .concept
            .get(&id)
            .and_then(|(cd, _)| match cd {
                ConceptDelta::Insert(c) => Some(c.clone()),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(d) => {
                    let mut concept = self
                        .get_concept(id)
                        .expect("Deltas imply that a concept that doesn't exist will be updated!")
                        .clone();
                    concept.apply(d.clone());
                    Some(concept)
                }
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .expect(&format!("No concept with id = {}", id))
                    .clone()
            })
    }
    fn add_concept_delta(
        &self,
        delta: &ContextDelta,
        concept: Concept,
        variable: bool,
    ) -> (ContextDelta, usize) {
        let mut added_gaps = Vec::<usize>::new();
        let mut removed_gaps = HashSet::<usize>::new();
        let mut new_concept_length = self.concepts.len();
        for (id, (cd, _)) in &delta.concept {
            match cd {
                ConceptDelta::Insert(_) => {
                    if *id >= new_concept_length {
                        new_concept_length = *id + 1
                    }
                }
                _ => (),
            };
        }
        for (id, (cd, _)) in &delta.concept {
            match cd {
                ConceptDelta::Insert(_) => {
                    removed_gaps.insert(*id);
                }
                ConceptDelta::Remove(_) => {
                    added_gaps.push(*id);
                    removed_gaps.remove(id);
                }
                ConceptDelta::Update(_) => (),
            }
        }
        let index: usize;
        let mut gap_index = if self.gaps.is_empty() {
            None
        } else {
            Some(self.gaps.len() - 1)
        };
        loop {
            match (added_gaps.pop(), gap_index) {
                (Some(id), _) => {
                    if removed_gaps.contains(&id) {
                        continue;
                    } else {
                        index = id;
                        break;
                    }
                }
                (None, Some(gi)) => {
                    if removed_gaps.contains(&self.gaps[gi]) {
                        if gi == 0 {
                            index = new_concept_length;
                            break;
                        } else {
                            gap_index = Some(gi - 1);
                            continue;
                        }
                    } else {
                        index = self.gaps[gi];
                        break;
                    }
                }
                (None, None) => {
                    index = new_concept_length;
                    break;
                }
            };
        }
        (
            ContextDelta {
                concept: hashmap! {index => (ConceptDelta::Insert(concept), variable)},
                string: hashmap! {},
            },
            index,
        )
    }
    fn delete_definition(
        &self,
        delta: &ContextDelta,
        concept: usize,
        left: usize,
        right: usize,
    ) -> ContextDelta {
        let mut edited_concept: Option<Concept> = Some(self.read_concept(delta, concept));
        let mut edited_left: Option<Concept> = Some(self.read_concept(delta, left));
        let mut edited_right: Option<Concept> = Some(self.read_concept(delta, right));
        let mut concept_delta = CD::default();
        let mut left_delta = CD::default();
        let mut right_delta = CD::default();
        delta.concept.get(&concept).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => concept_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_concept = None;
                concept_delta = CD::default();
            }
        });
        delta.concept.get(&left).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => left_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_left = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_left = None;
                left_delta = CD::default();
            }
        });
        delta.concept.get(&right).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => right_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_right = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_right = None;
                right_delta = CD::default();
            }
        });
        ContextDelta {
            concept: hashmap! {
                concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .remove_definition_delta(&concept_delta),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                left => (
                    ConceptDelta::Update(
                        edited_left
                            .expect("Left previously removed!")
                            .remove_as_lefthand_of_delta(&left_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                right => (
                    ConceptDelta::Update(
                        edited_right
                            .expect("Right previously removed!")
                            .remove_as_righthand_of_delta(&right_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
            },
            string: hashmap! {},
        }
    }
    fn add_string_delta(string_id: usize, string: &str) -> ContextDelta {
        ContextDelta {
            string: hashmap! {string.to_string() => StringDelta::Insert(string_id)},
            concept: HashMap::default(),
        }
    }
    fn add_string(&mut self, string_id: usize, string: &str) {
        self.string_map.insert(string.to_string(), string_id);
    }
    fn get_normal_form(&self, deltas: &ContextDelta, concept: usize) -> Option<usize> {
        self.read_concept(deltas, concept)
            .get_reduction()
            .map(|n| self.get_normal_form(deltas, n).unwrap_or(n))
    }
    fn get_concept_of_label(&self, deltas: &ContextDelta, concept: usize) -> Option<usize> {
        self.read_concept(deltas, concept)
            .get_righthand_of()
            .iter()
            .filter(|candidate| {
                self.read_concept(deltas, **candidate)
                    .get_definition()
                    .expect("Candidate should have a definition!")
                    .0
                    == LABEL
            })
            .nth(0)
            .cloned()
    }
    fn is_disconnected(&self, deltas: &ContextDelta, concept: usize) -> bool {
        self.read_concept(deltas, concept).get_reduction().is_none()
            && self
                .read_concept(deltas, concept)
                .get_definition()
                .is_none()
            && self
                .read_concept(deltas, concept)
                .get_lefthand_of()
                .is_empty()
            && self.righthand_of_without_label_is_empty(deltas, concept)
            && self
                .read_concept(deltas, concept)
                .find_what_reduces_to_it()
                .is_empty()
    }
    fn righthand_of_without_label_is_empty(&self, deltas: &ContextDelta, con: usize) -> bool {
        self.read_concept(deltas, con)
            .get_righthand_of()
            .iter()
            .filter_map(|concept| {
                self.read_concept(deltas, *concept)
                    .get_definition()
                    .filter(|(left, _)| *left != LABEL)
            })
            .nth(0)
            .is_none()
    }
    fn find_what_its_a_normal_form_of(&self, deltas: &ContextDelta, con: usize) -> HashSet<usize> {
        let mut normal_form_of = self
            .read_concept(deltas, con)
            .find_what_reduces_to_it()
            .clone();
        for concept in normal_form_of.clone().iter() {
            for concept2 in self.find_what_its_a_normal_form_of(deltas, *concept).iter() {
                normal_form_of.insert(*concept2);
            }
        }
        normal_form_of
    }
    fn remove_concept(&self, delta: &mut ContextDelta, concept: usize) {
        if let Some(ref s) = self.read_concept(delta, concept).get_string() {
            self.remove_string_delta(delta, s);
        }
        self.blindly_remove_concept_delta(delta, concept);
    }
    fn blindly_remove_concept(&mut self, id: usize) {
        self.concepts[id] = None;
        self.gaps.push(id);
    }
    fn blindly_remove_concept_delta(&self, delta: &mut ContextDelta, id: usize) {
        let concept = delta
            .concept
            .get(&id)
            .and_then(|(cd, _)| match cd {
                ConceptDelta::Insert(c) => Some(c),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(_) => self.get_concept(id),
            })
            .unwrap_or_else(|| {
                self.get_concept(id)
                    .expect("Concept will be already removed!")
            })
            .clone();
        delta.concept.insert(
            id,
            (ConceptDelta::Remove(concept), self.has_variable(delta, id)),
        );
    }
    fn remove_string_delta(&self, delta: &mut ContextDelta, string: &str) {
        let index = *(delta
            .string
            .get(string)
            .and_then(|sd| match sd {
                StringDelta::Update { after, .. } => Some(after),
                StringDelta::Insert(index) => Some(index),
                StringDelta::Remove(_) => None,
            })
            .expect("string already removed or doesn't exist"));
        delta
            .string
            .insert(string.to_string(), StringDelta::Remove(index));
    }
    fn remove_string(&mut self, string: &str) {
        self.string_map
            .remove(string)
            .expect("No string to remove!");
    }
    fn unlabel(&self, deltas: &mut ContextDelta, concept: usize) -> ZiaResult<()> {
        let concept_of_label = self
            .get_concept_of_label(deltas, concept)
            .expect("No label to remove");
        self.delete_reduction(deltas, concept_of_label)
    }
    fn set_concept_definition_deltas(
        &self,
        delta: &mut ContextDelta,
        concept: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        let concept_delta = self
            .read_concept(delta, concept)
            .set_definition_delta(lefthand, righthand)?;
        update_concept_delta(delta.concept.entry(concept), concept_delta);
        let lefthand_delta = self
            .read_concept(delta, lefthand)
            .add_as_lefthand_of_delta(concept);
        update_concept_delta(delta.concept.entry(lefthand), lefthand_delta);
        let righthand_delta = self
            .read_concept(delta, righthand)
            .add_as_righthand_of_delta(concept);
        update_concept_delta(delta.concept.entry(righthand), righthand_delta);
        Ok(())
    }
    fn concept_reduction_deltas(
        &self,
        delta: &mut ContextDelta,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()> {
        let concept_delta = self
            .read_concept(delta, concept)
            .make_reduce_to_delta(reduction)?;
        let reduction_delta = self
            .read_concept(delta, reduction)
            .make_reduce_from_delta(concept);
        update_concept_delta(delta.concept.entry(concept), concept_delta);
        update_concept_delta(delta.concept.entry(reduction), reduction_delta);
        Ok(())
    }
    fn remove_concept_reduction(
        &self,
        delta: &ContextDelta,
        concept: usize,
        reduction: usize,
    ) -> ContextDelta {
        let mut edited_concept: Option<Concept> = Some(self.read_concept(delta, concept));
        let mut concept_delta = CD::default();
        delta.concept.get(&concept).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => concept_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_concept = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_concept = None;
                concept_delta = CD::default();
            }
        });
        let mut edited_reduction: Option<Concept> = Some(self.read_concept(delta, reduction));
        let mut reduction_delta = CD::default();
        delta.concept.get(&reduction).map(|(cd, _)| match cd {
            ConceptDelta::Update(d) => reduction_delta.combine(d.clone()),
            ConceptDelta::Insert(t) => edited_reduction = Some(t.clone()),
            ConceptDelta::Remove(_) => {
                edited_reduction = None;
                reduction_delta = CD::default();
            }
        });
        ContextDelta {
            concept: hashmap! {
                concept => (
                    ConceptDelta::Update(
                        edited_concept
                            .expect("Concept previously removed!")
                            .make_reduce_to_none_delta(&concept_delta),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
                reduction => (
                    ConceptDelta::Update(
                        edited_reduction
                            .expect("Reduction previously removed!")
                            .no_longer_reduces_from_delta(&reduction_delta, concept),
                    ),
                    false, // Doesn't affect anything whether true or false
                ),
            },
            string: hashmap! {},
        }
    }
    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&self, delta: &mut ContextDelta, ast: &Rc<SyntaxTree>) -> ZiaResult<String> {
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
        delta: &mut ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
        delta: &mut ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
        delta: &mut ContextDelta,
        ast: &Rc<SyntaxTree>,
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
        delta: &mut ContextDelta,
        ast: &Rc<SyntaxTree>,
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
        delta: &mut ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
        delta: &mut ContextDelta,
        left: &Rc<SyntaxTree>,
        rightleft: &Rc<SyntaxTree>,
        rightright: &Rc<SyntaxTree>,
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
    fn relabel(&self, delta: &mut ContextDelta, concept: usize, new_label: &str) -> ZiaResult<()> {
        self.unlabel(delta, concept)?;
        self.label(delta, concept, new_label)
    }
    fn redefine(
        &self,
        delta: &mut ContextDelta,
        concept: usize,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
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
    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteDefinition)`. Otherwise `define` is called.
    fn execute_definition(
        &self,
        delta: &mut ContextDelta,
        new: &Rc<SyntaxTree>,
        old: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
        if old.contains(new) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.define(delta, new, old)
        }
    }
    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadDefinition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(
        &self,
        delta: &mut ContextDelta,
        new: &Rc<SyntaxTree>,
        old: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
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
    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    fn define_new_syntax(
        &self,
        delta: &mut ContextDelta,
        syntax: &str,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_then(|l| {
                right.get_concept().and_then(|r| {
                    self.find_definition(delta, l, r)
                        .map(|concept| syntax.parse::<SyntaxTree>().unwrap().bind_concept(concept))
                })
            })
            .unwrap_or_else(|| syntax.parse::<SyntaxTree>().unwrap())
            .bind_pair(left, right);
        self.concept_from_ast(delta, &new_syntax_tree)?;
        Ok(())
    }
    pub fn new() -> Self {
        let mut cont = Self::default();
        let delta = cont.setup().unwrap();
        info!(cont.logger, "Setup a new context: {:?}", &delta);
        cont.apply(delta);
        cont
    }
    fn ast_from_expression(&self, deltas: &ContextDelta, s: &str) -> ZiaResult<Rc<SyntaxTree>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(deltas, &tokens)
    }
    fn ast_from_tokens(
        &self,
        deltas: &ContextDelta,
        tokens: &[String],
    ) -> ZiaResult<Rc<SyntaxTree>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(deltas, &tokens[0]),
            2 => self.ast_from_pair(deltas, &tokens[0], &tokens[1]),
            _ => {
                let precedence_syntax = self.to_ast(deltas, PRECEDENCE);
                let (lp_syntax, lp_indices, _number_of_tokens) = tokens.iter().try_fold(
                    (Vec::<Rc<SyntaxTree>>::new(), Vec::<usize>::new(), None),
                    |(lowest_precedence_syntax, lp_indices, prev_index), token| {
                        let this_index = prev_index.map(|x| x + 1).or(Some(0));
                        let syntax_of_token = self.ast_from_token(deltas, token)?;
                        let comparing_precedence_of_token =
                            self.combine(deltas, &precedence_syntax, &syntax_of_token);
                        for syntax in lowest_precedence_syntax.clone() {
                            let comparing_between_tokens =
                                self.combine(deltas, &syntax, &comparing_precedence_of_token);
                            match self
                                .reduce(deltas, &comparing_between_tokens, &HashMap::new())
                                .and_then(|s| s.get_concept())
                            {
                                // syntax of token has even lower precedence than some previous lowest precendence syntax
                                Some(TRUE) => {
                                    return Ok((
                                        vec![syntax_of_token],
                                        vec![this_index.unwrap()],
                                        this_index,
                                    ))
                                }
                                // syntax of token has higher precedence than some previous lowest precendence syntax
                                Some(FALSE) => {
                                    return Ok((lowest_precedence_syntax, lp_indices, this_index))
                                }
                                _ => (),
                            };
                        }
                        // syntax of token has neither higher or lower precedence than the lowest precedence syntax
                        let mut hps = lowest_precedence_syntax.clone();
                        hps.push(syntax_of_token);
                        let mut hi = lp_indices.clone();
                        hi.push(this_index.unwrap());
                        Ok((hps, hi, this_index))
                    },
                )?;
                let assoc = lp_syntax.iter().try_fold(None, |assoc, syntax| {
                    match (self.get_associativity(deltas, &syntax), assoc) {
                        (Some(x), Some(y)) => {
                            if x == y {
                                Ok(Some(x))
                            } else {
                                Err(ZiaError::AmbiguousExpression)
                            }
                        }
                        (Some(x), None) => Ok(Some(x)),
                        (None, _) => Err(ZiaError::AmbiguousExpression),
                    }
                });
                match assoc? {
                    Some(Associativity::Right) => lp_indices
                        .iter()
                        .rev()
                        .try_fold((None, None), |(tail, prev_lp_index), lp_index| {
                            let slice = match prev_lp_index {
                                Some(i) => &tokens[*lp_index..i],
                                None => &tokens[*lp_index..],
                            };
                            let lp_with_the_rest = self.ast_from_tokens(deltas, slice)?;
                            Ok((
                                Some(match tail {
                                    None => lp_with_the_rest,
                                    Some(t) => self.combine(deltas, &lp_with_the_rest, &t),
                                }),
                                Some(*lp_index),
                            ))
                        })?
                        .0
                        .ok_or(ZiaError::AmbiguousExpression),
                    Some(Associativity::Left) => lp_indices
                        .iter()
                        .try_fold((None, None), |(head, prev_lp_index), lp_index| {
                            let slice = match prev_lp_index {
                                Some(i) => &tokens[i..*lp_index],
                                None => &tokens[..*lp_index],
                            };
                            let lp_with_the_rest = self.ast_from_tokens(deltas, slice)?;
                            Ok((
                                Some(match head {
                                    None => lp_with_the_rest,
                                    Some(h) => self.combine(deltas, &h, &lp_with_the_rest),
                                }),
                                Some(*lp_index),
                            ))
                        })?
                        .0
                        .ok_or(ZiaError::AmbiguousExpression),
                    _ => Err(ZiaError::AmbiguousExpression),
                }
            }
        }
    }
    fn ast_from_pair(
        &self,
        deltas: &ContextDelta,
        left: &str,
        right: &str,
    ) -> ZiaResult<Rc<SyntaxTree>> {
        let lefthand = self.ast_from_token(deltas, left)?;
        let righthand = self.ast_from_token(deltas, right)?;
        Ok(self.combine(deltas, &lefthand, &righthand))
    }
    fn ast_from_token(&self, deltas: &ContextDelta, t: &str) -> ZiaResult<Rc<SyntaxTree>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(deltas, t)
        } else {
            Ok(Rc::new(self.ast_from_symbol::<SyntaxTree>(deltas, t)))
        }
    }
    fn concept_from_label(&self, deltas: &ContextDelta, s: &str) -> Option<usize> {
        self.get_string_concept(deltas, s)
            .and_then(|c| self.get_labellee(deltas, c))
    }
    fn ast_from_symbol<U: FromStr + BindConcept>(&self, deltas: &ContextDelta, s: &str) -> U
    where
        <U as FromStr>::Err: Debug,
    {
        self.concept_from_label(deltas, s)
            .map(|concept| s.parse::<U>().unwrap().bind_concept(concept))
            .unwrap_or_else(|| s.parse().unwrap())
    }
    fn get_string_concept(&self, delta: &ContextDelta, s: &str) -> Option<usize> {
        delta
            .string
            .get(s)
            .map(|string_delta| match string_delta {
                StringDelta::Update { after, .. } => Some(after),
                StringDelta::Insert(concept) => Some(concept),
                StringDelta::Remove(_) => None,
            })
            .unwrap_or_else(|| self.string_map.get(s))
            .cloned()
    }
    fn cleanly_delete_definition(&self, delta: &mut ContextDelta, concept: usize) -> ZiaResult<()> {
        match self.read_concept(delta, concept).get_definition() {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                let extra_delta = self.delete_definition(delta, concept, left, right);
                delta.combine(extra_delta);
                self.try_delete_concept(delta, concept)?;
                self.try_delete_concept(delta, left)?;
                self.try_delete_concept(delta, right)
            }
        }
    }
    fn try_delete_concept(
        &self,
        previous_deltas: &mut ContextDelta,
        concept: usize,
    ) -> ZiaResult<()> {
        if self.is_disconnected(previous_deltas, concept) {
            self.unlabel(previous_deltas, concept)?;
            self.remove_concept(previous_deltas, concept)
        }
        Ok(())
    }
    fn label(&self, deltas: &mut ContextDelta, concept: usize, string: &str) -> ZiaResult<()> {
        if string.starts_with('_') && string.ends_with('_') {
            Ok(())
        } else {
            let definition = self.find_or_insert_definition(deltas, LABEL, concept, false)?;
            let string_id = self.new_string(deltas, string);
            self.update_reduction(deltas, definition, string_id)
        }
    }
    fn new_labelled_default(&self, deltas: &mut ContextDelta, string: &str) -> ZiaResult<usize> {
        let new_default =
            self.new_default::<AbstractPart>(deltas, string.starts_with('_') && string.ends_with('_'));
        self.label(deltas, new_default, string)?;
        Ok(new_default)
    }
    fn setup(&mut self) -> ZiaResult<ContextDelta> {
        let mut delta = ContextDelta::default();
        let concrete_constructor = |local_delta: &mut ContextDelta| {
            let (delta, index) = self.add_concept_delta(local_delta, Concept::default(), false);
            local_delta.combine(delta);
            index
        };
        let labels = vec![
            "label_of", ":=", "->", "let", "true", "false", "assoc", "right", "left", ">-",
        ];
        let concepts = delta.repeat(concrete_constructor, labels.len());
        let label = |local_delta: &mut ContextDelta, concept: usize, string: &str| {
            self.label(local_delta, concept, string)
        };
        delta.multiply(label, concepts, labels)?;
        Ok(delta)
    }
    fn find_or_insert_definition(
        &self,
        deltas: &mut ContextDelta,
        lefthand: usize,
        righthand: usize,
        variable: bool,
    ) -> ZiaResult<usize> {
        let pair = self.find_definition(deltas, lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default::<AbstractPart>(deltas, variable);
                self.insert_definition(deltas, definition, lefthand, righthand)?;
                Ok(definition)
            }
            Some(def) => Ok(def),
        }
    }
    fn new_string(&self, original_delta: &mut ContextDelta, string: &str) -> usize {
        let string_concept = string.to_string().into();
        let (delta, index) = self.add_concept_delta(original_delta, string_concept, false);
        original_delta.combine(delta);
        let string_delta = Self::add_string_delta(index, string);
        original_delta.combine(string_delta);
        index
    }
    fn new_default<V: Default + Into<Concept>>(
        &self,
        original_delta: &mut ContextDelta,
        variable: bool,
    ) -> usize {
        let concept: Concept = V::default().into();
        let (delta, index) = self.add_concept_delta(original_delta, concept, variable);
        original_delta.combine(delta);
        index
    }
    fn execute_reduction(
        &self,
        deltas: &mut ContextDelta,
        syntax: &SyntaxTree,
        normal_form: &SyntaxTree,
    ) -> ZiaResult<()> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            self.try_removing_reduction(deltas, syntax)
        } else {
            let syntax_concept = self.concept_from_ast(deltas, syntax)?;
            let normal_form_concept = self.concept_from_ast(deltas, normal_form)?;
            self.update_reduction(deltas, syntax_concept, normal_form_concept)
        }
    }
    fn concept_from_ast(&self, deltas: &mut ContextDelta, ast: &SyntaxTree) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else if let Some(c) = self.concept_from_label(deltas, &ast.to_string()) {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(deltas, string),
                Some((ref left, ref right)) => {
                    let leftc = self.concept_from_ast(deltas, left)?;
                    let rightc = self.concept_from_ast(deltas, right)?;
                    let ls = left.to_string();
                    let rs = right.to_string();
                    let concept = self.find_or_insert_definition(
                        deltas,
                        leftc,
                        rightc,
                        ls.starts_with('_') && ls.ends_with('_')
                            || rs.starts_with('_') && rs.ends_with('_'),
                    )?;
                    if !string.contains(' ') {
                        self.label(deltas, concept, string)?;
                    }
                    Ok(concept)
                }
            }
        }
    }
}

fn update_concept_delta(entry: Entry<usize, (ConceptDelta, bool)>, concept_delta: CD) {
    entry
        .and_modify(|(cd, _)| match cd {
            ConceptDelta::Update(d) => {
                d.combine(concept_delta.clone());
                *cd = ConceptDelta::Update(d.clone());
            }
            ConceptDelta::Insert(c) => {
                c.apply(concept_delta.clone());
                *cd = ConceptDelta::Insert(c.clone());
            }
            ConceptDelta::Remove(_) => panic!("Concept will already be removed"),
        })
        .or_insert((ConceptDelta::Update(concept_delta), false));
}

impl Container for Context {
    fn contains(&self, deltas: &ContextDelta, outer: usize, inner: usize) -> bool {
        if let Some((left, right)) = self.read_concept(deltas, outer).get_definition() {
            left == inner
                || right == inner
                || self.contains(deltas, left, inner)
                || self.contains(deltas, right, inner)
        } else {
            false
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContextDelta {
    string: HashMap<String, StringDelta>,
    concept: HashMap<usize, (ConceptDelta, bool)>,
}

#[derive(Clone, Debug)]
pub enum StringDelta {
    Insert(usize),
    Remove(usize),
    Update { before: usize, after: usize },
}

#[derive(Clone, Debug)]
pub enum ConceptDelta {
    Insert(Concept),
    Remove(Concept),
    Update(CD),
}

impl Delta for ContextDelta {
    fn combine(&mut self, other: ContextDelta) {
        for (other_key, (other_value, v2)) in other.concept {
            let mut remove_key = false;
            let mut update_delta = None;
            self.concept
                .entry(other_key)
                .and_modify(|(cd, v1)| match (cd, &other_value) {
                    (ConceptDelta::Insert(c1), ConceptDelta::Remove(c2))
                        if c1 == c2 && *v1 == v2 =>
                    {
                        remove_key = true;
                    }
                    (ConceptDelta::Remove(c1), ConceptDelta::Insert(c2)) => {
                        if c1 == c2 {
                            remove_key = true;
                        } else {
                            update_delta = Some(c1.diff(c2.clone()));
                        }
                    }
                    (ConceptDelta::Insert(c), ConceptDelta::Update(cd)) => {
                        c.apply(cd.clone());
                        *v1 = v2;
                    }
                    (ConceptDelta::Update(cd1), ConceptDelta::Update(cd2)) => {
                        cd1.combine(cd2.clone());
                        *v1 = v2;
                    }
                    _ => panic!("Something went wrong when combining concept deltas!"),
                })
                .or_insert((other_value, v2));
            if remove_key {
                self.concept.remove(&other_key);
            }
            update_delta.map(|cd| {
                self.concept
                    .insert(other_key, (ConceptDelta::Update(cd), v2))
            });
        }
        for (other_key, other_sd) in other.string {
            let mut remove_string = false;
            let mut sd_to_update = None;
            self.string
                .entry(other_key.clone())
                .and_modify(|sd| match (sd, &other_sd) {
                    (StringDelta::Insert(u1), StringDelta::Remove(u2)) if u1 == u2 => {
                        remove_string = true;
                    }
                    (StringDelta::Remove(u1), StringDelta::Insert(u2)) => {
                        if u1 == u2 {
                            remove_string = true;
                        } else {
                            sd_to_update = Some(StringDelta::Update {
                                before: *u1,
                                after: *u2,
                            });
                        }
                    }
                    (StringDelta::Insert(u), StringDelta::Update { before, after })
                        if u == before =>
                    {
                        *u = *after;
                    }
                    (
                        StringDelta::Update { after: a1, .. },
                        StringDelta::Update {
                            before: b2,
                            after: a2,
                        },
                    ) if a1 == b2 => {
                        *a1 = *a2;
                    }
                    _ => panic!("Something went wrong when combining string deltas!"),
                })
                .or_insert(other_sd);
            if remove_string {
                self.string.remove(&other_key);
            }
            sd_to_update.map(|sd| self.string.insert(other_key, sd));
        }
    }
}

impl ApplyDelta for Context {
    type Delta = ContextDelta;
    fn apply(&mut self, delta: ContextDelta) {
        delta.string.iter().for_each(|(s, sd)| match sd {
            StringDelta::Update { after, .. } => {
                self.string_map.insert(s.to_string(), *after);
            }
            StringDelta::Insert(id) => {
                info!(self.logger, "add_string({}, {})", id, &s);
                self.add_string(*id, &s);
            }
            StringDelta::Remove(_) => self.remove_string(&s),
        });
        for (id, (cd, v)) in delta.concept {
            match cd {
                ConceptDelta::Insert(c) => {
                    let padding_needed = id as isize - self.concepts.len() as isize;
                    if 0 <= padding_needed {
                        self.concepts.extend(vec![None; padding_needed as usize]);
                        self.concepts.push(Some(c));
                    } else {
                        self.concepts[id] = Some(c);
                    }
                    if v {
                        self.variables.insert(id);
                    }
                }
                ConceptDelta::Remove(_) => {
                    self.blindly_remove_concept(id);
                    if v {
                        self.variables.remove(&id);
                    }
                }
                ConceptDelta::Update(d) => self.write_concept(id).apply(d),
            }
        }
    }
    fn diff(&self, _other: Context) -> ContextDelta {
        ContextDelta {
            string: hashmap! {},
            concept: hashmap! {},
        }
    }
}

impl Default for Context {
    fn default() -> Context {
        let plain = slog_term::PlainSyncDecorator::new(slog_term::TestStdoutWriter);
        let logger = slog::Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!());
        Context {
            string_map: HashMap::new(),
            concepts: Vec::new(),
            gaps: Vec::new(),
            logger,
            variables: HashSet::new(),
        }
    }
}

impl InsertDefinition for Context {
    fn insert_definition(
        &self,
        deltas: &mut ContextDelta,
        definition: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        if self.contains(deltas, lefthand, definition)
            || self.contains(deltas, righthand, definition)
        {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.check_reductions(deltas, definition, lefthand)?;
            self.check_reductions(deltas, definition, righthand)?;
            self.set_concept_definition_deltas(deltas, definition, lefthand, righthand)?;
            Ok(())
        }
    }
    fn check_reductions(
        &self,
        deltas: &ContextDelta,
        outer_concept: usize,
        inner_concept: usize,
    ) -> ZiaResult<()> {
        if let Some(r) = self.read_concept(deltas, inner_concept).get_reduction() {
            if r == outer_concept || self.contains(deltas, r, outer_concept) {
                Err(ZiaError::InfiniteDefinition)
            } else {
                self.check_reductions(deltas, outer_concept, r)
            }
        } else {
            Ok(())
        }
    }
}

impl UpdateReduction for Context {
    fn update_reduction(
        &self,
        deltas: &mut ContextDelta,
        concept: usize,
        reduction: usize,
    ) -> ZiaResult<()> {
        self.get_normal_form(deltas, reduction)
            .and_then(|n| {
                if concept == n {
                    Some(Err(ZiaError::CyclicReduction))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                self.read_concept(deltas, concept)
                    .get_reduction()
                    .and_then(|r| {
                        if r == reduction {
                            Some(Err(ZiaError::RedundantReduction))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        if reduction == self.get_reduction_of_composition(deltas, concept) {
                            Err(ZiaError::RedundantReduction)
                        } else {
                            self.concept_reduction_deltas(deltas, concept, reduction)
                        }
                    })
            })
    }
    fn get_reduction_of_composition(&self, deltas: &ContextDelta, concept: usize) -> usize {
        self.read_concept(deltas, concept)
            .get_definition()
            .and_then(|(left, right)| {
                self.find_definition(
                    deltas,
                    self.get_reduction_or_reduction_of_composition(deltas, left),
                    self.get_reduction_or_reduction_of_composition(deltas, right),
                )
            })
            .unwrap_or(concept)
    }
    fn get_reduction_or_reduction_of_composition(
        &self,
        deltas: &ContextDelta,
        concept: usize,
    ) -> usize {
        self.read_concept(deltas, concept)
            .get_reduction()
            .unwrap_or_else(|| self.get_reduction_of_composition(deltas, concept))
    }
}

impl DeleteReduction<SyntaxTree> for Context {
    fn try_removing_reduction(
        &self,
        deltas: &mut ContextDelta,
        syntax: &SyntaxTree,
    ) -> ZiaResult<()> {
        if let Some(c) = syntax.get_concept() {
            self.delete_reduction(deltas, c)
        } else {
            Err(ZiaError::RedundantReduction)
        }
    }
    fn delete_reduction(&self, delta: &mut ContextDelta, concept: usize) -> ZiaResult<()> {
        self.read_concept(delta, concept)
            .get_reduction()
            .map(|n| {
                let extra_delta = self.remove_concept_reduction(delta, concept, n);
                delta.combine(extra_delta);
            })
            .ok_or(ZiaError::RedundantReduction)
    }
}

impl FindDefinition for Context {
    fn find_definition(
        &self,
        delta: &ContextDelta,
        lefthand: usize,
        righthand: usize,
    ) -> Option<usize> {
        let lc = self.read_concept(delta, lefthand);
        let rc = self.read_concept(delta, righthand);
        let has_lefthand = lc.get_lefthand_of();
        let has_righthand = rc.get_righthand_of();
        let mut candidates = has_lefthand.intersection(&has_righthand);
        candidates.next().map(|index| {
            candidates.next().map_or(*index, |_| {
                panic!("Multiple definitions with the same lefthand and righthand pair exist.")
            })
        })
    }
}

impl Label for Context {
    fn get_labellee(&self, deltas: &ContextDelta, concept: usize) -> Option<usize> {
        let mut candidates: Vec<usize> = Vec::new();
        for label in self.find_what_its_a_normal_form_of(deltas, concept) {
            match self.read_concept(deltas, label).get_definition() {
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

impl GetLabel for Context {
    fn get_label(&self, deltas: &ContextDelta, concept: usize) -> Option<String> {
        match self.get_concept_of_label(deltas, concept) {
            None => self
                .read_concept(deltas, concept)
                .get_reduction()
                .and_then(|r| self.get_label(deltas, r)),
            Some(d) => self
                .get_normal_form(deltas, d)
                .and_then(|n| self.read_concept(deltas, n).get_string()),
        }
    }
}

impl SyntaxReader<SyntaxTree> for Context {
    /// Expands syntax by definition of its associated concept.
    fn expand(&self, deltas: &ContextDelta, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
        if let Some(con) = ast.get_concept() {
            if let Some((left, right)) = self.read_concept(deltas, con).get_definition() {
                self.combine(
                    deltas,
                    &self.expand(deltas, &self.to_ast(deltas, left)),
                    &self.expand(deltas, &self.to_ast(deltas, right)),
                )
            } else {
                self.to_ast(deltas, con)
            }
        } else if let Some((ref left, ref right)) = ast.get_expansion() {
            self.combine(
                deltas,
                &self.expand(deltas, left),
                &self.expand(deltas, right),
            )
        } else {
            ast.clone()
        }
    }
    /// Reduces the syntax as much as possible (returns the normal form syntax).
    fn recursively_reduce(&self, deltas: &ContextDelta, ast: &Rc<SyntaxTree>) -> Rc<SyntaxTree> {
        match self.reduce(deltas, ast, &HashMap::new()) {
            Some(ref a) => self.recursively_reduce(deltas, a),
            None => ast.clone(),
        }
    }
    fn determine_reduction_truth(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        if left == right {
            Some(false)
        } else {
            self.determine_evidence_of_reduction(deltas, left, right)
                .or_else(|| {
                    self.determine_evidence_of_reduction(deltas, right, left)
                        .map(|x| !x)
                })
        }
    }
    fn determine_evidence_of_reduction(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        self.reduce(deltas, left, &HashMap::new())
            .and_then(|reduced_left| {
                if &reduced_left == right {
                    Some(true)
                } else {
                    self.determine_evidence_of_reduction(deltas, &reduced_left, right)
                }
            })
    }
    /// Reduces the syntax by using the reduction rules of associated concepts.
    fn reduce(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
        variable_mask: &HashMap<usize, Rc<SyntaxTree>>,
    ) -> Option<Rc<SyntaxTree>> {
        ast.get_concept()
            .and_then(|c| self.reduce_concept(deltas, c, variable_mask))
            .or_else(|| {
                ast.get_expansion().and_then(|(ref left, ref right)| {
                    self.reduce_pair(deltas, left, right, variable_mask)
                })
            })
    }
    // Reduces a syntax tree based on the properties of the left and right branches
    fn reduce_pair(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
        variable_mask: &HashMap<usize, Rc<SyntaxTree>>,
    ) -> Option<Rc<SyntaxTree>> {
        left.get_concept()
            .and_then(|lc| match lc {
                ASSOC => Some(self.to_ast(deltas, RIGHT)),
                _ => variable_mask
                    .get(&lc)
                    .and_then(|ast| self.reduce(deltas, ast, variable_mask)),
            })
            .or_else(|| {
                right
                    .get_expansion()
                    .and_then(|(ref rightleft, ref rightright)| {
                        self.reduce_by_expanded_right_branch(deltas, left, rightleft, rightright)
                    })
                    .or_else(|| {
                        self.match_left_right(
                            deltas,
                            self.reduce(deltas, left, variable_mask),
                            self.reduce(deltas, right, variable_mask),
                            left,
                            right,
                        )
                        .or_else(|| {
                            self.filter_generalisations_for_pair(deltas, left, right)
                                .iter()
                                .filter_map(|(generalisation, v_mask)| {
                                    self.reduce_concept(deltas, *generalisation, v_mask)
                                })
                                .nth(0)
                        })
                    })
            })
    }
    fn filter_generalisations_for_pair(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Vec<(usize, HashMap<usize, Rc<SyntaxTree>>)> {
        let mut generalisations = left
            .get_concept()
            .map(|lc| {
                self.read_concept(deltas, lc)
                    .get_lefthand_of()
                    .iter()
                    .filter_map(|lo| {
                        if self.has_variable(deltas, *lo) {
                            self.read_concept(deltas, *lo)
                                .get_definition()
                                .and_then(|(_, r)| {
                                    if self.has_variable(deltas, r) {
                                        match self.read_concept(deltas, r).get_definition() {
                                            Some(_) => None,
                                            None => {
                                                let mut hash_map = HashMap::new();
                                                hash_map.insert(r, right.clone());
                                                Some((*lo, hash_map))
                                            }
                                        }
                                    } else {
                                        None
                                    }
                                })
                        } else {
                            None
                        }
                    })
                    .collect()
            })
            .unwrap_or_else(|| Vec::default());
        generalisations.extend(
            right
                .get_concept()
                .map(|rc| {
                    self.read_concept(deltas, rc)
                        .get_righthand_of()
                        .iter()
                        .filter_map(|ro| {
                            if self.has_variable(deltas, *ro) {
                                self.read_concept(deltas, *ro).get_definition().and_then(
                                    |(l, _)| {
                                        if self.has_variable(deltas, l) {
                                            match self.read_concept(deltas, l).get_definition() {
                                                Some(_) => None,
                                                None => {
                                                    let mut hash_map = HashMap::new();
                                                    hash_map.insert(l, left.clone());
                                                    Some((*ro, hash_map))
                                                }
                                            }
                                        } else {
                                            None
                                        }
                                    },
                                )
                            } else {
                                None
                            }
                        })
                        .collect()
                })
                .unwrap_or_else(|| Vec::default()),
        );
        generalisations
    }
    // Reduces a syntax tree based on the properties of the left branch and the branches of the right branch
    fn reduce_by_expanded_right_branch(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        rightleft: &Rc<SyntaxTree>,
        rightright: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
        rightleft.get_concept().and_then(|rlc| match rlc {
            REDUCTION => self
                .determine_reduction_truth(deltas, left, &rightright)
                .map(|x| {
                    if x {
                        self.to_ast(deltas, TRUE)
                    } else {
                        self.to_ast(deltas, FALSE)
                    }
                }),
            _ => None,
        })
    }
    /// Returns the syntax for the reduction of a concept.
    fn reduce_concept(
        &self,
        deltas: &ContextDelta,
        concept: usize,
        variable_mask: &HashMap<usize, Rc<SyntaxTree>>,
    ) -> Option<Rc<SyntaxTree>> {
        variable_mask
            .get(&concept)
            .and_then(|ast| self.reduce(deltas, ast, variable_mask))
            .or_else(|| {
                self.read_concept(deltas, concept)
                    .get_reduction()
                    .map(|n| self.to_ast(deltas, n))
                    .or_else(|| {
                        self.read_concept(deltas, concept)
                            .get_definition()
                            .and_then(|(left, right)| {
                                let left_result = self.reduce_concept(deltas, left, variable_mask);
                                let right_result =
                                    self.reduce_concept(deltas, right, variable_mask);
                                self.match_left_right(
                                    deltas,
                                    left_result,
                                    right_result,
                                    &self.to_ast(deltas, left),
                                    &self.to_ast(deltas, right),
                                )
                            })
                    })
            })
    }
    /// Returns the syntax for a concept.
    fn to_ast(&self, deltas: &ContextDelta, concept: usize) -> Rc<SyntaxTree> {
        match self.get_label(deltas, concept) {
            Some(s) => Rc::new(s.parse::<SyntaxTree>().unwrap().bind_concept(concept)),
            None => {
                let (left, right) = self
                    .read_concept(deltas, concept)
                    .get_definition()
                    .expect("Unlabelled concept with no definition");
                self.combine(
                    deltas,
                    &self.to_ast(deltas, left),
                    &self.to_ast(deltas, right),
                )
            }
        }
    }
    fn combine(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
        other: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        let syntax = ast
            .get_concept()
            .and_then(|l| {
                other.get_concept().and_then(|r| {
                    self.find_definition(deltas, l, r)
                        .map(|concept| self.join(deltas, ast, other).bind_concept(concept))
                })
            })
            .unwrap_or_else(|| self.join(deltas, ast, other));
        Rc::new(syntax)
    }
    fn join(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> SyntaxTree {
        self.display_joint(deltas, left, right)
            .parse::<SyntaxTree>()
            .unwrap()
            .bind_pair(left, right)
    }
    fn display_joint(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> String {
        let left_string = left
            .get_expansion()
            .map(|(l, r)| match self.get_associativity(deltas, &r).unwrap() {
                Associativity::Left => l.to_string() + " " + &r.to_string(),
                Associativity::Right => {
                    "(".to_string() + &l.to_string() + " " + &r.to_string() + ")"
                }
            })
            .unwrap_or_else(|| left.to_string());
        let right_string = right
            .get_expansion()
            .map(|(l, r)| match self.get_associativity(deltas, &l).unwrap() {
                Associativity::Left => {
                    "(".to_string() + &l.to_string() + " " + &r.to_string() + ")"
                }
                Associativity::Right => l.to_string() + " " + &r.to_string(),
            })
            .unwrap_or_else(|| right.to_string());
        left_string + " " + &right_string
    }
    fn get_associativity(
        &self,
        deltas: &ContextDelta,
        ast: &Rc<SyntaxTree>,
    ) -> Option<Associativity> {
        let assoc_of_ast = self.combine(deltas, &self.to_ast(deltas, ASSOC), &ast);
        self.reduce(deltas, &assoc_of_ast, &HashMap::new())
            .and_then(|ast| match ast.get_concept() {
                Some(LEFT) => Some(Associativity::Left),
                Some(RIGHT) => Some(Associativity::Right),
                _ => None,
            })
    }
    fn has_higher_precedence(
        &self,
        deltas: &ContextDelta,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<bool> {
        let is_higher_prec_than_right =
            self.combine(deltas, &self.to_ast(deltas, PRECEDENCE), &right);
        let left_is_higher_prec_than_right = self.combine(deltas, left, &is_higher_prec_than_right);
        self.reduce(deltas, &left_is_higher_prec_than_right, &HashMap::new())
            .and_then(|ast| match ast.get_concept() {
                Some(TRUE) => Some(true),
                Some(FALSE) => Some(false),
                _ => None,
            })
    }
    /// Returns the updated branch of abstract syntax tree that may have had the left or right parts updated.
    fn match_left_right(
        &self,
        deltas: &ContextDelta,
        left: Option<Rc<SyntaxTree>>,
        right: Option<Rc<SyntaxTree>>,
        original_left: &Rc<SyntaxTree>,
        original_right: &Rc<SyntaxTree>,
    ) -> Option<Rc<SyntaxTree>> {
        match (left, right) {
            (None, None) => None,
            (Some(new_left), None) => Some(self.contract_pair(deltas, &new_left, original_right)),
            (None, Some(new_right)) => Some(self.contract_pair(deltas, original_left, &new_right)),
            (Some(new_left), Some(new_right)) => {
                Some(self.contract_pair(deltas, &new_left, &new_right))
            }
        }
    }
    /// Returns the abstract syntax from two syntax parts, using the label and concept of the composition of associated concepts if it exists.
    fn contract_pair(
        &self,
        deltas: &ContextDelta,
        lefthand: &Rc<SyntaxTree>,
        righthand: &Rc<SyntaxTree>,
    ) -> Rc<SyntaxTree> {
        Rc::new(
            lefthand
                .get_concept()
                .and_then(|lc| {
                    righthand.get_concept().and_then(|rc| {
                        self.find_definition(deltas, lc, rc).and_then(|def| {
                            self.get_label(deltas, def)
                                .map(|label| label.parse::<SyntaxTree>().unwrap().bind_concept(def))
                        })
                    })
                })
                .unwrap_or_else(|| {
                    self.display_joint(deltas, lefthand, righthand)
                        .parse::<SyntaxTree>()
                        .unwrap()
                })
                .bind_pair(lefthand, righthand),
        )
    }
}

fn parse_line(buffer: &str) -> ZiaResult<Vec<String>> {
    let mut tokens: Vec<String> = [].to_vec();
    let mut token = String::new();
    let parenthesis_level = buffer.chars().try_fold(0, |p_level, letter| {
        parse_letter(letter, p_level, &mut token, &mut tokens)
    })?;
    if parenthesis_level != 0 {
        return Err(ZiaError::MissingSymbol { symbol: ")" });
    }
    if token != "" {
        tokens.push(token.clone());
    }
    Ok(tokens)
}

fn parse_letter(
    letter: char,
    mut parenthesis_level: u8,
    token: &mut String,
    tokens: &mut Vec<String>,
) -> ZiaResult<u8> {
    match letter {
        '(' => {
            push_token(letter, parenthesis_level, token, tokens);
            Ok(parenthesis_level + 1)
        }
        ')' => {
            if parenthesis_level > 0 {
                parenthesis_level -= 1;
                push_token(letter, parenthesis_level, token, tokens);
                Ok(parenthesis_level)
            } else {
                Err(ZiaError::MissingSymbol { symbol: "(" })
            }
        }
        ' ' => {
            push_token(letter, parenthesis_level, token, tokens);
            Ok(parenthesis_level)
        }
        '\n' | '\r' => Ok(parenthesis_level),
        _ => {
            token.push(letter);
            Ok(parenthesis_level)
        }
    }
}

fn push_token(letter: char, parenthesis_level: u8, token: &mut String, tokens: &mut Vec<String>) {
    if (token != "") & (parenthesis_level == 0) {
        tokens.push(token.clone());
        *token = String::new();
    }
    if parenthesis_level != 0 {
        token.push(letter);
    }
}
