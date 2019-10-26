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

use ast::SyntaxTree;
use concepts::{AbstractPart, Concept};
use constants::{DEFINE, LABEL, LET, REDUCTION, TRUE};
use context_delta::{ConceptDelta, ContextDelta, StringDelta, update_concept_delta};
use delta::{ApplyDelta, Delta};
use errors::{map_err_variant, ZiaError, ZiaResult};
use snap_shot::SnapShot;
use slog;
use slog::Drain;
use std::{
    collections::HashMap,
    default::Default,
    iter::from_fn,
    mem::swap,
    rc::Rc,
};

pub struct Context {
    snap_shot: SnapShot,
    logger: slog::Logger,
    delta: ContextDelta,
}

impl Context {
    pub fn new() -> Self {
        let mut cont = Self::default();
        cont.setup();
        info!(cont.logger, "Setup a new context: {:?}", &cont.delta);
        cont.commit();
        cont
    }
    pub fn execute(&mut self, command: &str) -> String {
        info!(self.logger, "execute({})", command);
        let string = self
            .snap_shot
            .ast_from_expression(&self.delta, command)
            .and_then(|a| self.call(&a))
            .unwrap_or_else(|e| e.to_string());
        info!(self.logger, "execute({}) -> {:?}", command, self.delta);
        self.commit();
        string
    }
    fn commit(&mut self) {
        let mut delta_to_apply = ContextDelta::default();
        swap(&mut delta_to_apply, &mut self.delta);
        self.snap_shot.apply(delta_to_apply);
    }
    fn setup(&mut self) {
        let mut concrete_constructor = || {
            let (delta, index) =
                self.snap_shot
                    .add_concept_delta(&self.delta, Concept::default(), false);
            self.delta.combine(delta);
            index
        };
        let labels = vec![
            "label_of", ":=", "->", "let", "true", "false", "assoc", "right", "left", ">-",
        ];
        let mut counter = 0;
        let concepts: Vec<usize> = from_fn(|| {
            if counter < labels.len() {
                counter += 1;
                Some(concrete_constructor())
            } else {
                None
            }
        })
        .collect();
        concepts
            .iter()
            .zip(&labels)
            .try_for_each(|(concept, string)| self.label(*concept, string))
            .unwrap();
    }
    fn reduce_and_call_pair(
        &mut self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> ZiaResult<String> {
        let reduced_left = self.snap_shot.reduce(&self.delta, left, &HashMap::new());
        let reduced_right = self.snap_shot.reduce(&self.delta, right, &HashMap::new());
        match (reduced_left, reduced_right) {
            (None, None) => Err(ZiaError::CannotReduceFurther),
            (Some(rl), None) => self.call_pair(&rl, right),
            (None, Some(rr)) => self.call_pair(left, &rr),
            (Some(rl), Some(rr)) => self.call_pair(&rl, &rr),
        }
    }
    /// If the abstract syntax tree can be expanded, then `call` is called with this expansion. If not then an `Err(ZiaError::NotAProgram)` is returned
    fn try_expanding_then_call(&mut self, ast: &Rc<SyntaxTree>) -> ZiaResult<String> {
        let expansion = &self.snap_shot.expand(&self.delta, ast);
        if expansion != ast {
            self.call(expansion)
        } else {
            Err(ZiaError::CannotExpandFurther)
        }
    }
    /// If the abstract syntax tree can be reduced, then `call` is called with this reduction. If not then an `Err(ZiaError::CannotReduceFurther)` is returned
    fn try_reducing_then_call(&mut self, ast: &Rc<SyntaxTree>) -> ZiaResult<String> {
        let normal_form = &self.snap_shot.recursively_reduce(&self.delta, ast);
        if normal_form != ast {
            self.call(normal_form)
        } else {
            Err(ZiaError::CannotReduceFurther)
        }
    }
    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&mut self, ast: &Rc<SyntaxTree>) -> ZiaResult<String> {
        match ast
            .get_concept()
            .and_then(|c| self.snap_shot.read_concept(&self.delta, c).get_string())
        {
            Some(s) => Ok(s),
            None => match ast.get_expansion() {
                Some((ref left, ref right)) => map_err_variant(
                    self.call_pair(left, right),
                    &ZiaError::CannotReduceFurther,
                    || {
                        map_err_variant(
                            self.try_reducing_then_call(ast),
                            &ZiaError::CannotReduceFurther,
                            || {
                                Ok(self
                                    .snap_shot
                                    .contract_pair(&self.delta, left, right)
                                    .to_string())
                            },
                        )
                    },
                ),
                None => map_err_variant(
                    self.try_reducing_then_call(ast),
                    &ZiaError::CannotReduceFurther,
                    || {
                        map_err_variant(
                            self.try_expanding_then_call(ast),
                            &ZiaError::CannotExpandFurther,
                            || Ok(ast.to_string()),
                        )
                    },
                ),
            },
        }
    }
    /// If the associated concept of the lefthand part of the syntax tree is LET then `call_as_righthand` is called with the left and right of the lefthand syntax. Tries to get the concept associated with the righthand part of the syntax. If the associated concept is `->` then `call` is called with the reduction of the lefthand part of the syntax. Otherwise `Err(ZiaError::NotAProgram)` is returned.
    fn call_pair(&mut self, left: &Rc<SyntaxTree>, right: &Rc<SyntaxTree>) -> ZiaResult<String> {
        left.get_concept()
            .and_then(|lc| match lc {
                LET => right
                    .get_expansion()
                    .and_then(|(left, right)| {
                        self.execute_let(&left, &right).and_then(|x| match x {
                            Err(ZiaError::CannotReduceFurther) => None,
                            Err(ZiaError::UnusedSymbol) => None,
                            _ => Some(x),
                        })
                    })
                    .or_else(|| {
                        Some({
                            let true_syntax = self.snap_shot.to_ast(&self.delta, TRUE);
                            self.execute_reduction(right, &true_syntax)
                        })
                    })
                    .map(|r| r.map(|()| "".to_string())),
                LABEL => Some(Ok("'".to_string()
                    + &right
                        .get_concept()
                        .and_then(|c| self.snap_shot.get_label(&self.delta, c))
                        .unwrap_or_else(|| right.to_string())
                    + "'")),
                _ => None,
            })
            .unwrap_or_else(|| match right.get_concept() {
                Some(c) if c == REDUCTION => self.try_reducing_then_call(&left),
                _ => self.reduce_and_call_pair(left, right),
            })
    }
    /// If the righthand part of the syntax can be expanded, then `match_righthand_pair` is called. If not, `Err(ZiaError::CannotExpandFurther)` is returned.
    fn execute_let(
        &mut self,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> Option<ZiaResult<()>> {
        right
            .get_expansion()
            .map(|(ref rightleft, ref rightright)| {
                self.match_righthand_pair(left, rightleft, rightright)
            })
    }
    /// If the lefthand of the righthand part of the syntax is `->` then `execute_reduction` is called with the lefthand part and the righthand of the
    /// righthand part of the syntax. Similarly for `:=`, `execute_definition` is called. If the lefthand of the righthand part of the syntax is associated
    /// with a concept which isn't `->` or `:=` then if this concept reduces, `match_righthand_pair` is called with this reduced concept as an abstract syntax tree.
    fn match_righthand_pair(
        &mut self,
        left: &Rc<SyntaxTree>,
        rightleft: &Rc<SyntaxTree>,
        rightright: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
        match rightleft.get_concept() {
            Some(c) => match c {
                REDUCTION => self.execute_reduction(left, rightright),
                DEFINE => self.execute_definition(left, rightright),
                _ => {
                    let rightleft_reduction =
                        self.snap_shot.read_concept(&self.delta, c).get_reduction();
                    if let Some(r) = rightleft_reduction {
                        let ast = self.snap_shot.to_ast(&self.delta, r);
                        self.match_righthand_pair(left, &ast, rightright)
                    } else {
                        Err(ZiaError::CannotReduceFurther)
                    }
                }
            },
            None => Err(ZiaError::UnusedSymbol),
        }
    }
    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteDefinition)`. Otherwise `define` is called.
    fn execute_definition(&mut self, new: &Rc<SyntaxTree>, old: &Rc<SyntaxTree>) -> ZiaResult<()> {
        if old.contains(new) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.define(new, old)
        }
    }
    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadDefinition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(&mut self, new: &Rc<SyntaxTree>, old: &Rc<SyntaxTree>) -> ZiaResult<()> {
        if new.get_expansion().is_some() {
            Err(ZiaError::BadDefinition)
        } else {
            match (new.get_concept(), old.get_concept(), old.get_expansion()) {
                (_, None, None) => Err(ZiaError::RedundantRefactor),
                (None, Some(b), None) => self.relabel(b, &new.to_string()),
                (None, Some(b), Some(_)) => {
                    if self.snap_shot.get_label(&self.delta, b).is_none() {
                        self.label(b, &new.to_string())
                    } else {
                        self.relabel(b, &new.to_string())
                    }
                }
                (None, None, Some((ref left, ref right))) => {
                    self.define_new_syntax(&new.to_string(), left, right)
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
    fn cleanly_delete_definition(&mut self, concept: usize) -> ZiaResult<()> {
        match self
            .snap_shot
            .read_concept(&self.delta, concept)
            .get_definition()
        {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                let concept_delta_array = self
                    .snap_shot
                    .read_concept(&self.delta, concept)
                    .delete_definition(concept);
                let concept_id_array = [concept, left, right];
                (0..3).for_each(|index| {
                    update_concept_delta(
                        self.delta.concept.entry(concept_id_array[index]),
                        &concept_delta_array[index],
                    )
                });
                self.try_delete_concept(concept)?;
                self.try_delete_concept(left)?;
                self.try_delete_concept(right)
            }
        }
    }
    fn try_delete_concept(&mut self, concept: usize) -> ZiaResult<()> {
        if self.snap_shot.is_disconnected(&self.delta, concept) {
            self.unlabel(concept)?;
            self.remove_concept(concept);
        }
        Ok(())
    }
    fn remove_concept(&mut self, concept: usize) {
        if let Some(ref s) = self
            .snap_shot
            .read_concept(&self.delta, concept)
            .get_string()
        {
            self.remove_string(s);
        }
        self.blindly_remove_concept(concept);
    }
    fn remove_string(&mut self, string: &str) {
        let index = *(self
            .delta
            .string
            .get(string)
            .and_then(|sd| match sd {
                StringDelta::Update { after, .. } => Some(after),
                StringDelta::Insert(index) => Some(index),
                StringDelta::Remove(_) => None,
            })
            .expect("string already removed or doesn't exist"));
        self.delta
            .string
            .insert(string.to_string(), StringDelta::Remove(index));
    }
    fn blindly_remove_concept(&mut self, id: usize) {
        let concept = self
            .delta
            .concept
            .get(&id)
            .and_then(|(cd, _)| match cd {
                ConceptDelta::Insert(c) => Some(c),
                ConceptDelta::Remove(_) => None,
                ConceptDelta::Update(_) => self.snap_shot.get_concept(id),
            })
            .unwrap_or_else(|| {
                self.snap_shot
                    .get_concept(id)
                    .expect("Concept will be already removed!")
            })
            .clone();
        self.delta.concept.insert(
            id,
            (
                ConceptDelta::Remove(concept),
                self.snap_shot.has_variable(&self.delta, id),
            ),
        );
    }
    fn redefine(
        &mut self,
        concept: usize,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
        if let Some((left_concept, right_concept)) = self
            .snap_shot
            .read_concept(&self.delta, concept)
            .get_definition()
        {
            self.relabel(left_concept, &left.to_string())?;
            self.relabel(right_concept, &right.to_string())
        } else {
            let left_concept = self.concept_from_ast(left)?;
            let right_concept = self.concept_from_ast(right)?;
            self.insert_definition(concept, left_concept, right_concept)
        }
    }
    fn relabel(&mut self, concept: usize, new_label: &str) -> ZiaResult<()> {
        self.unlabel(concept)?;
        self.label(concept, new_label)
    }
    fn unlabel(&mut self, concept: usize) -> ZiaResult<()> {
        let concept_of_label = self
            .snap_shot
            .get_concept_of_label(&self.delta, concept)
            .expect("No label to remove");
        self.delete_reduction(concept_of_label)
    }
    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    fn define_new_syntax(
        &mut self,
        syntax: &str,
        left: &Rc<SyntaxTree>,
        right: &Rc<SyntaxTree>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_then(|l| {
                right.get_concept().and_then(|r| {
                    self.snap_shot
                        .find_definition(&self.delta, l, r)
                        .map(|concept| syntax.parse::<SyntaxTree>().unwrap().bind_concept(concept))
                })
            })
            .unwrap_or_else(|| syntax.parse::<SyntaxTree>().unwrap())
            .bind_pair(left, right);
        self.concept_from_ast(&new_syntax_tree)?;
        Ok(())
    }
    fn execute_reduction(
        &mut self,
        syntax: &SyntaxTree,
        normal_form: &SyntaxTree,
    ) -> ZiaResult<()> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            self.try_removing_reduction(syntax)
        } else {
            let syntax_concept = self.concept_from_ast(syntax)?;
            let normal_form_concept = self.concept_from_ast(normal_form)?;
            self.update_reduction(syntax_concept, normal_form_concept)
        }
    }
    fn try_removing_reduction(&mut self, syntax: &SyntaxTree) -> ZiaResult<()> {
        if let Some(c) = syntax.get_concept() {
            self.delete_reduction(c)
        } else {
            Err(ZiaError::RedundantReduction)
        }
    }
    fn delete_reduction(&mut self, concept_id: usize) -> ZiaResult<()> {
        self.snap_shot
            .read_concept(&self.delta, concept_id)
            .remove_reduction(concept_id)
            .map(|z| {
                z.iter().for_each(|(id, concept_delta)| {
                    update_concept_delta(self.delta.concept.entry(*id), concept_delta)
                })
            })
    }
    fn concept_from_ast(&mut self, ast: &SyntaxTree) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else if let Some(c) = self
            .snap_shot
            .concept_from_label(&self.delta, &ast.to_string())
        {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(string),
                Some((ref left, ref right)) => {
                    let leftc = self.concept_from_ast(left)?;
                    let rightc = self.concept_from_ast(right)?;
                    let ls = left.to_string();
                    let rs = right.to_string();
                    let concept = self.find_or_insert_definition(
                        leftc,
                        rightc,
                        ls.starts_with('_') && ls.ends_with('_')
                            || rs.starts_with('_') && rs.ends_with('_'),
                    )?;
                    if !string.contains(' ') {
                        self.label(concept, string)?;
                    }
                    Ok(concept)
                }
            }
        }
    }
    fn new_labelled_default(&mut self, string: &str) -> ZiaResult<usize> {
        let new_default =
            self.new_default::<AbstractPart>(string.starts_with('_') && string.ends_with('_'));
        self.label(new_default, string)?;
        Ok(new_default)
    }
    fn label(&mut self, concept: usize, string: &str) -> ZiaResult<()> {
        if string.starts_with('_') && string.ends_with('_') {
            Ok(())
        } else {
            let definition = self.find_or_insert_definition(LABEL, concept, false)?;
            let string_id = self.new_string(string);
            self.update_reduction(definition, string_id)
        }
    }
    fn new_string(&mut self, string: &str) -> usize {
        let string_concept = string.to_string().into();
        let (delta, index) = self
            .snap_shot
            .add_concept_delta(&self.delta, string_concept, false);
        self.delta.combine(delta);
        let string_delta = SnapShot::add_string_delta(index, string);
        self.delta.combine(string_delta);
        index
    }
    fn find_or_insert_definition(
        &mut self,
        lefthand: usize,
        righthand: usize,
        variable: bool,
    ) -> ZiaResult<usize> {
        let pair = self
            .snap_shot
            .find_definition(&self.delta, lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default::<AbstractPart>(variable);
                self.insert_definition(definition, lefthand, righthand)?;
                Ok(definition)
            }
            Some(def) => Ok(def),
        }
    }
    fn new_default<V: Default + Into<Concept>>(&mut self, variable: bool) -> usize {
        let concept: Concept = V::default().into();
        let (delta, index) = self
            .snap_shot
            .add_concept_delta(&self.delta, concept, variable);
        self.delta.combine(delta);
        index
    }
    fn insert_definition(
        &mut self,
        definition: usize,
        lefthand: usize,
        righthand: usize,
    ) -> ZiaResult<()> {
        if self.snap_shot.contains(&self.delta, lefthand, definition)
            || self.snap_shot.contains(&self.delta, righthand, definition)
        {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.snap_shot
                .check_reductions(&self.delta, definition, lefthand)?;
            self.snap_shot
                .check_reductions(&self.delta, definition, righthand)?;
            let id_array = [definition, lefthand, righthand];
            let concept_delta_array = self
                .snap_shot
                .read_concept(&self.delta, definition)
                .set_definition(definition, lefthand, righthand)?;
            concept_delta_array
                .iter()
                .enumerate()
                .for_each(|(i, concept_delta)| {
                    update_concept_delta(self.delta.concept.entry(id_array[i]), concept_delta)
                });
            Ok(())
        }
    }
    fn update_reduction(&mut self, concept: usize, reduction: usize) -> ZiaResult<()> {
        self.snap_shot
            .get_normal_form(&self.delta, reduction)
            .and_then(|n| {
                if concept == n {
                    Some(Err(ZiaError::CyclicReduction))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                self.snap_shot
                    .read_concept(&self.delta, concept)
                    .get_reduction()
                    .and_then(|r| {
                        if r == reduction {
                            Some(Err(ZiaError::RedundantReduction))
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| {
                        if reduction
                            == self
                                .snap_shot
                                .get_reduction_of_composition(&self.delta, concept)
                        {
                            Err(ZiaError::RedundantReduction)
                        } else {
                            let concept_deltas = self
                                .snap_shot
                                .read_concept(&self.delta, concept)
                                .reduce_to(concept, reduction)?;
                            update_concept_delta(
                                self.delta.concept.entry(concept),
                                &concept_deltas[0],
                            );
                            update_concept_delta(
                                self.delta.concept.entry(reduction),
                                &concept_deltas[1],
                            );
                            Ok(())
                        }
                    })
            })
    }
}

impl Default for Context {
    fn default() -> Context {
        let plain = slog_term::PlainSyncDecorator::new(slog_term::TestStdoutWriter);
        let logger = slog::Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!());
        Context {
            snap_shot: SnapShot::default(),
            logger,
            delta: ContextDelta::default(),
        }
    }
}
