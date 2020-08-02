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

use crate::{
    and_also::AndAlso,
    ast::{is_variable, SyntaxTree},
    concepts::{ConcreteConceptType, SpecificPart},
    context_cache::ContextCache,
    context_delta::{ConceptDelta, ContextDelta, StringDelta},
    context_search::{Comparison, ContextSearch},
    context_snap_shot::Associativity,
    delta::Apply,
    errors::{map_err_variant, ZiaError, ZiaResult},
    parser::parse_line,
    snap_shot::Reader as SnapShotReader,
};
#[cfg(not(target_arch = "wasm32"))]
use slog::{info, o, Drain, Logger};
use std::{default::Default, iter::from_fn, mem::swap, sync::Arc};

#[derive(Clone)]
pub struct Context<S> {
    snap_shot: S,
    #[cfg(not(target_arch = "wasm32"))]
    logger: Logger,
    delta: ContextDelta,
    cache: ContextCache,
}

type ParsingResult = ZiaResult<Arc<SyntaxTree>>;

#[derive(Debug, PartialEq)]
pub struct TokenSubsequence {
    pub syntax: Vec<Arc<SyntaxTree>>,
    pub positions: Vec<usize>,
}

impl<S> Context<S>
where
    S: SnapShotReader + Default + Sync + Apply<Delta = ContextDelta>,
{
    #[must_use]
    pub fn new() -> Self {
        let mut cont = Self::default();
        cont.setup();
        #[cfg(not(target_arch = "wasm32"))]
        info!(cont.logger, "Setup a new context: {:#?}", &cont.delta);
        cont.commit();
        cont
    }

    #[cfg(test)]
    pub fn new_test_case() -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        let logger = {
            let plain =
                slog_term::PlainSyncDecorator::new(slog_term::TestStdoutWriter);
            Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!())
        };
        Self {
            snap_shot: S::new_test_case(),
            #[cfg(not(target_arch = "wasm32"))]
            logger,
            delta: ContextDelta::default(),
            cache: ContextCache::default(),
        }
    }

    pub fn disable_reduction_cache(mut self) -> Self {
        self.cache.disable_reduction_cache();
        self
    }

    pub fn disable_syntax_tree_cache(mut self) -> Self {
        self.cache.disable_syntax_tree_cache();
        self
    }

    pub fn execute(&mut self, command: &str) -> String {
        #[cfg(not(target_arch = "wasm32"))]
        info!(self.logger, "execute({})", command);
        let string = self
            .ast_from_expression(command)
            .and_then(|a| {
                #[cfg(not(target_arch = "wasm32"))]
                info!(
                    self.logger,
                    "ast_from_expression({}) -> {:#?}", command, a
                );
                self.call(&a)
            })
            .unwrap_or_else(|e| e.to_string());
        #[cfg(not(target_arch = "wasm32"))]
        info!(self.logger, "execute({}) -> {:#?}", command, self.delta);
        self.commit();
        string
    }

    pub fn ast_from_expression(&mut self, s: &str) -> ParsingResult {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(&tokens)
    }

    fn ast_from_tokens(&mut self, tokens: &[String]) -> ParsingResult {
        info!(
            self.logger,
            "ast_from_tokens({:#?})", tokens
        );
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(&tokens[0]),
            2 => self.ast_from_pair(&tokens[0], &tokens[1]),
            _ => {
                let TokenSubsequence {
                    syntax: lp_syntax,
                    positions: lp_indices,
                } = self.lowest_precedence_info(tokens)?;
                if lp_indices.is_empty() {
                    return Err(ZiaError::AmbiguousExpression);
                }
                let assoc = lp_syntax.iter().try_fold(None, |assoc, syntax| {
                    match (
                        self.context_search().get_associativity(syntax),
                        assoc,
                    ) {
                        (x, Some(y)) => {
                            if x == y {
                                Ok(Some(x))
                            } else {
                                Err(ZiaError::AmbiguousExpression)
                            }
                        },
                        (x, None) => Ok(Some(x)),
                    }
                })?;
                info!(
                    self.logger,
                    "ast_from_tokens({:#?}): assoc = {:#?}", tokens, assoc
                );
                match assoc {
                    Some(Associativity::Right) => {
                        let tail = lp_indices
                            .iter()
                            .rev()
                            .try_fold((None, None), |state, lp_index| {
                                self.associativity_try_fold_handler(
                                    tokens,
                                    state,
                                    *lp_index,
                                    &Associativity::Right,
                                )
                            })?
                            .0
                            .unwrap(); // Already checked that lp_indices is non-empty;
                        info!(
                            self.logger,
                            "ast_from_tokens({:#?}): tail = {}", tokens, tail
                        );
                        if lp_indices[0] == 0 {
                            Ok(tail)
                        } else {
                            let head =
                                self.ast_from_tokens(&tokens[..lp_indices[0]])?;
                            Ok(self.context_search().combine(&head, &tail))
                        }
                    },
                    Some(Associativity::Left) => lp_indices
                        .iter()
                        .try_fold((None, None), |state, lp_index| {
                            self.associativity_try_fold_handler(
                                tokens,
                                state,
                                *lp_index,
                                &Associativity::Left,
                            )
                        })?
                        .0
                        .ok_or(ZiaError::AmbiguousExpression),
                    None => Err(ZiaError::AmbiguousExpression),
                }
            },
        }
    }

    fn associativity_try_fold_handler(
        &mut self,
        tokens: &[String],
        state: (Option<Arc<SyntaxTree>>, Option<usize>),
        lp_index: usize,
        assoc: &Associativity,
    ) -> ZiaResult<(Option<Arc<SyntaxTree>>, Option<usize>)> {
        let prev_lp_index = state.1;
        let slice = assoc.slice_tokens(tokens, prev_lp_index, lp_index);
        // Required otherwise self.ast_from_tokens will return Err(ZiaError::EmprtyParentheses)
        if slice.is_empty() {
            return Err(ZiaError::AmbiguousExpression);
        }
        let edge_index = match assoc {
            Associativity::Left => slice.len() - 1,
            Associativity::Right => 0,
        };
        let lp_with_the_rest = if lp_index == edge_index {
            let edge_syntax = self.ast_from_token(&slice[edge_index])?;
            if slice.len() == 1 {
                edge_syntax
            } else {
                match assoc {
                    Associativity::Left => {
                        let rest_of_syntax = if slice.len() < 3 {
                            self.ast_from_token(&slice[slice.len() - 1])?
                        } else {
                            self.ast_from_tokens(&slice[..slice.len() - 1])?
                        };
                        self.context_search()
                            .combine(&rest_of_syntax, &edge_syntax)
                    },
                    Associativity::Right => {
                        let rest_of_syntax = if slice.len() < 3 {
                            self.ast_from_token(&slice[1])?
                        } else {
                            self.ast_from_tokens(&slice[1..])?
                        };
                        self.context_search()
                            .combine(&edge_syntax, &rest_of_syntax)
                    },
                }
            }
        } else {
            self.ast_from_tokens(slice)?
        };
        let edge = state.0;
        Ok((
            Some(match edge {
                None => lp_with_the_rest,
                Some(e) => match assoc {
                    Associativity::Left => {
                        self.context_search().combine(&e, &lp_with_the_rest)
                    },
                    Associativity::Right => {
                        self.context_search().combine(&lp_with_the_rest, &e)
                    },
                },
            }),
            Some(lp_index),
        ))
    }

    /// Determine the syntax and the positions in the token sequence of the concepts with the lowest precedence
    pub fn lowest_precedence_info(
        &self,
        tokens: &[String],
    ) -> ZiaResult<TokenSubsequence> {
        info!(
            self.logger,
            "lowest_precedence_info({:#?})", tokens
        );
        let precedence_syntax =
            SyntaxTree::new_concept(S::precedence_id()).into();
        let context_search = self.context_search();
        let (syntax, positions, _number_of_tokens) = tokens.iter().try_fold(
            // Initially assume no concepts have the lowest precedence
            (Vec::<Arc<SyntaxTree>>::new(), Vec::<usize>::new(), None),
            |(mut lowest_precedence_syntax, mut lp_indices, prev_index),
             token| {
                // Increment index
                let this_index = prev_index.map(|x| x + 1).or(Some(0));
                let raw_syntax_of_token = SyntaxTree::from(token);
                let (precedence_of_token, syntax_of_token) = if let Some(c) =
                    self.snap_shot.concept_from_label(&self.delta, token)
                {
                    let syntax_of_token =
                        raw_syntax_of_token.bind_concept(c).into();
                    (
                        context_search
                            .combine(&precedence_syntax, &syntax_of_token),
                        syntax_of_token,
                    )
                } else {
                    (
                        SyntaxTree::new_concept(S::default_id()).into(),
                        raw_syntax_of_token.into(),
                    )
                };
                // Compare current token's precedence with each currently assumed lowest syntax
                for syntax in lowest_precedence_syntax.clone() {
                    let precedence_of_syntax = if syntax.get_concept().is_some()
                    {
                        context_search.combine(&precedence_syntax, &syntax)
                    } else {
                        SyntaxTree::new_concept(S::default_id()).into()
                    };
                    match context_search
                        .compare(&precedence_of_syntax, &precedence_of_token)
                        .0
                    {
                        // syntax of token has an even lower precedence than some previous lowest precendence syntax
                        // reset lowest precedence syntax with just this one
                        Comparison::GreaterThan => {
                            return Ok((
                                vec![syntax_of_token],
                                vec![this_index.unwrap()],
                                this_index,
                            ))
                        },
                        // syntax of token has a higher precedence than some previous lowest precendence syntax
                        // keep existing lowest precedence syntax as-is
                        Comparison::LessThan => {
                            return Ok((
                                lowest_precedence_syntax,
                                lp_indices,
                                this_index,
                            ))
                        },
                        // syntax of token has at least an equal precedence as the previous lowest precedence syntax
                        // include syntax is lowest precedence syntax list
                        Comparison::EqualTo
                        | Comparison::GreaterThanOrEqualTo => {
                            lowest_precedence_syntax.push(syntax_of_token);
                            lp_indices.push(this_index.unwrap());
                            return Ok((
                                lowest_precedence_syntax,
                                lp_indices,
                                this_index,
                            ));
                        },
                        // Cannot determine if token has higher or lower precedence than this syntax
                        // Check other syntax with lowest precedence
                        Comparison::Incomparable
                        | Comparison::LessThanOrEqualTo => (),
                    };
                }
                // syntax of token has neither higher or lower precedence than the lowest precedence syntax
                lowest_precedence_syntax.push(syntax_of_token);
                lp_indices.push(this_index.unwrap());
                Ok((lowest_precedence_syntax, lp_indices, this_index))
            },
        )?;
        let result = Ok(TokenSubsequence {
            syntax,
            positions,
        });
        info!(
            self.logger,
            "lowest_precedence_info({:#?}) -> {:#?}", tokens, result
        );
        result
    }

    fn ast_from_pair(&mut self, left: &str, right: &str) -> ParsingResult {
        let lefthand = self.ast_from_token(left)?;
        let righthand = self.ast_from_token(right)?;
        Ok(self.context_search().combine(&lefthand, &righthand))
    }

    fn ast_from_token(&mut self, t: &str) -> ParsingResult {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(t)
        } else {
            let ast = self.snap_shot.ast_from_symbol(&self.delta, t);
            if is_variable(t) && ast.get_concept().is_none() {
                let concept_id = self.new_default(SpecificPart::variable());
                let definition = self.find_or_insert_definition(
                    S::label_id(),
                    concept_id,
                    true,
                    true,
                )?;
                let string_id = self.new_string(t);
                self.update_reduction(definition, string_id, true)?;
                Ok(SyntaxTree::from(t).bind_concept(concept_id).into())
            } else {
                Ok(ast.into())
            }
        }
    }

    fn commit(&mut self) {
        let mut delta_to_apply = ContextDelta::default();
        swap(&mut delta_to_apply, &mut self.delta);
        self.snap_shot.apply(delta_to_apply);
    }

    fn setup(&mut self) {
        let mut concrete_constructor = || {
            let index =
                self.snap_shot.lowest_unoccupied_concept_id(&self.delta);
            self.delta.insert_concept(
                index,
                (
                    ConceptDelta::Insert(
                        (ConcreteConceptType::from(index), index).into(),
                    ),
                    false,
                ),
                &mut self.cache,
            );
            index
        };
        let labels = vec![
            "label_of",
            ":=",
            "->",
            "let",
            "true",
            "false",
            "assoc",
            "right",
            "left",
            "prec",
            "default",
            ">",
            "=>",
            "exists_such_that",
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
        self.execute("let (true and true) -> true");
        assert_eq!(self.execute("true and true"), "true");
        self.execute("let (false and _y_) -> false");
        // assert_eq!(self.execute("false and false"), "false");
        assert_eq!(self.execute("false and true"), "false");
        self.execute("let (_x_ and false) -> false");
        assert_eq!(self.execute("true and false"), "false");
        self.execute(
            "let (_y_ exists_such_that (_x_ > _y_) and _y_ > _z_) => _x_ > _z_",
        );
        self.execute("let default > prec ->");
        self.execute("let (prec ->) > prec let");
        assert_eq!(self.execute("let 2 > 1"), "");
        assert_eq!(self.execute("let 1 > 0"), "");
        assert_eq!(self.execute("2 > 0"), "true");
    }

    fn reduce_and_call_pair(
        &mut self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ZiaResult<String> {
        info!(
            self.logger,
            "reduce_and_call_pair({}, {})", left, right
        );
        let reduced_left = self.context_search().reduce(left);
        let reduced_right = self.context_search().reduce(right);
        match (reduced_left, reduced_right) {
            (None, None) => Err(ZiaError::CannotReduceFurther),
            (Some((rl, _)), None) => self.call_pair(&rl, right),
            (None, Some((rr, _))) => self.call_pair(left, &rr),
            (Some((rl, _)), Some((rr, _))) => self.call_pair(&rl, &rr),
        }
    }

    /// If the abstract syntax tree can be expanded, then `call` is called with this expansion. If not then an `Err(ZiaError::NotAProgram)` is returned
    fn try_expanding_then_call(
        &mut self,
        ast: &Arc<SyntaxTree>,
    ) -> ZiaResult<String> {
        let expansion = &self.context_search().expand(ast);
        if expansion == ast {
            Err(ZiaError::CannotExpandFurther)
        } else {
            self.call(expansion)
        }
    }

    /// If the abstract syntax tree can be reduced, then `call` is called with this reduction. If not then an `Err(ZiaError::CannotReduceFurther)` is returned
    fn try_reducing_then_call(
        &mut self,
        ast: &Arc<SyntaxTree>,
    ) -> ZiaResult<String> {
        info!(
            self.logger,
            "try_reducing_then_call({})", ast
        );
        let (normal_form, _) = &self.context_search().recursively_reduce(ast);
        if normal_form == ast {
            Err(ZiaError::CannotReduceFurther)
        } else {
            self.call(normal_form)
        }
    }

    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&mut self, ast: &Arc<SyntaxTree>) -> ZiaResult<String> {
        info!(
            self.logger,
            "call({})", ast
        );
        match ast.get_concept().and_then(|c| {
            self.snap_shot.read_concept(&self.delta, c).get_string()
        }) {
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
                                    .context_search()
                                    .contract_pair(left, right)
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
    fn call_pair(
        &mut self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ZiaResult<String> {
        left.get_concept()
            .and_then(|lc| {
                if lc == S::let_id() {
                    right
                        .get_expansion()
                        .and_then(|(left, right)| {
                            self.execute_let(&left, &right).and_then(
                                |x| match x {
                                    Err(ZiaError::CannotReduceFurther)
                                    | Err(ZiaError::UnusedSymbol) => None,
                                    _ => Some(x),
                                },
                            )
                        })
                        .or_else(|| {
                            Some({
                                let true_syntax =
                                    self.context_search().to_ast(S::true_id());
                                self.execute_reduction(right, &true_syntax)
                            })
                        })
                        .map(|r| r.map(|()| "".to_string()))
                } else if lc == S::label_id() {
                    Some(Ok("'".to_string()
                        + &right
                            .get_concept()
                            .and_then(|c| {
                                self.snap_shot.get_label(&self.delta, c)
                            })
                            .unwrap_or_else(|| right.to_string())
                        + "'"))
                } else {
                    None
                }
            })
            .unwrap_or_else(|| {
                if right.get_concept() == Some(S::reduction_id()) {
                    self.try_reducing_then_call(left)
                } else {
                    self.reduce_and_call_pair(left, right)
                }
            })
    }

    /// If the righthand part of the syntax can be expanded, then `match_righthand_pair` is called. If not, `Err(ZiaError::CannotExpandFurther)` is returned.
    fn execute_let(
        &mut self,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> Option<ZiaResult<()>> {
        right.get_expansion().map(|(ref rightleft, ref rightright)| {
            self.match_righthand_pair(left, rightleft, rightright)
        })
    }

    /// If the lefthand of the righthand part of the syntax is `->` then `execute_reduction` is called with the lefthand part and the righthand of the
    /// righthand part of the syntax. Similarly for `:=`, `execute_definition` is called. If the lefthand of the righthand part of the syntax is associated
    /// with a concept which isn't `->` or `:=` then if this concept reduces, `match_righthand_pair` is called with this reduced concept as an abstract syntax tree.
    fn match_righthand_pair(
        &mut self,
        left: &Arc<SyntaxTree>,
        rightleft: &Arc<SyntaxTree>,
        rightright: &Arc<SyntaxTree>,
    ) -> ZiaResult<()> {
        rightleft.get_concept().map_or(Err(ZiaError::UnusedSymbol), |c| {
            if c == S::reduction_id() {
                self.execute_reduction(left, rightright)
            } else if c == S::define_id() {
                self.execute_definition(left, rightright)
            } else {
                let rightleft_reduction =
                    self.snap_shot.read_concept(&self.delta, c).get_reduction();
                if let Some(r) = rightleft_reduction {
                    let ast = self.context_search().to_ast(r);
                    self.match_righthand_pair(left, &ast, rightright)
                } else {
                    Err(ZiaError::CannotReduceFurther)
                }
            }
        })
    }

    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteDefinition)`. Otherwise `define` is called.
    fn execute_definition(
        &mut self,
        new: &Arc<SyntaxTree>,
        old: &Arc<SyntaxTree>,
    ) -> ZiaResult<()> {
        if old.contains(new) {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.define(new, old)
        }
    }

    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadDefinition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(
        &mut self,
        new: &Arc<SyntaxTree>,
        old: &Arc<SyntaxTree>,
    ) -> ZiaResult<()> {
        if new.get_expansion().is_some() {
            Err(ZiaError::BadDefinition)
        } else {
            match (new.get_concept(), old.get_concept(), old.get_expansion()) {
                (_, None, None) => Err(ZiaError::RedundantRefactor),
                (None, Some(b), None) => self.relabel(b, &new.to_string()),
                (None, Some(b), Some(_)) => {
                    if self
                        .snap_shot
                        .get_concept_of_label(&self.delta, b)
                        .is_none()
                    {
                        self.label(b, &new.to_string())
                    } else {
                        self.relabel(b, &new.to_string())
                    }
                },
                (None, None, Some((ref left, ref right))) => {
                    self.define_new_syntax(&new.to_string(), left, right)
                },
                (Some(a), Some(b), None) => {
                    if a == b {
                        self.cleanly_delete_definition(a)
                    } else {
                        Err(ZiaError::DefinitionCollision)
                    }
                },
                (Some(a), Some(b), Some(_)) => {
                    if a == b {
                        Err(ZiaError::RedundantDefinition)
                    } else {
                        Err(ZiaError::DefinitionCollision)
                    }
                },
                (Some(a), None, Some((ref left, ref right))) => {
                    self.redefine(a, left, right)
                },
            }
        }
    }

    fn cleanly_delete_definition(&mut self, concept: usize) -> ZiaResult<()> {
        match self.snap_shot.read_concept(&self.delta, concept).get_definition()
        {
            None => Err(ZiaError::RedundantDefinitionRemoval),
            Some((left, right)) => {
                let concept_delta_array = self
                    .snap_shot
                    .read_concept(&self.delta, concept)
                    .delete_definition(concept);
                let concept_id_array = [concept, left, right];
                (0..3).for_each(|index| {
                    self.delta.update_concept_delta(
                        concept_id_array[index],
                        &concept_delta_array[index],
                        false,
                        &mut self.cache,
                    )
                });
                self.try_delete_concept(concept)?;
                self.try_delete_concept(left)?;
                self.try_delete_concept(right)
            },
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
        if let Some(ref s) =
            self.snap_shot.read_concept(&self.delta, concept).get_string()
        {
            self.remove_string(s);
        }
        self.blindly_remove_concept(concept);
    }

    fn remove_string(&mut self, string: &str) {
        let index = *(self
            .delta
            .string()
            .get(string)
            .and_then(|sd| match sd {
                StringDelta::Update {
                    after,
                    ..
                } => Some(after),
                StringDelta::Insert(index) => Some(index),
                StringDelta::Remove(_) => None,
            })
            .expect("string already removed or doesn't exist"));
        self.delta.insert_string(
            string,
            StringDelta::Remove(index),
            &mut self.cache,
        );
    }

    fn blindly_remove_concept(&mut self, id: usize) {
        let concept = self.snap_shot.read_concept(&self.delta, id);
        self.delta.insert_concept(
            id,
            (ConceptDelta::Remove(concept), false),
            &mut self.cache,
        );
    }

    fn redefine(
        &mut self,
        concept: usize,
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ZiaResult<()> {
        if let Some((left_concept, right_concept)) =
            self.snap_shot.read_concept(&self.delta, concept).get_definition()
        {
            self.relabel(left_concept, &left.to_string())?;
            self.relabel(right_concept, &right.to_string())
        } else {
            let left_concept = self.concept_from_ast(left)?;
            let right_concept = self.concept_from_ast(right)?;
            self.insert_definition(concept, left_concept, right_concept, false)
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
        left: &Arc<SyntaxTree>,
        right: &Arc<SyntaxTree>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_also(&right.get_concept())
            .and_then(|(l, r)| {
                self.context_search().find_definition(*l, *r).map(|concept| {
                    SyntaxTree::from(syntax).bind_concept(concept)
                })
            })
            .unwrap_or_else(|| syntax.into())
            .bind_pair(left.clone(), right.clone());
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
            self.update_reduction(syntax_concept, normal_form_concept, false)
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
                    self.delta.update_concept_delta(
                        *id,
                        concept_delta,
                        false,
                        &mut self.cache,
                    )
                })
            })
    }

    fn concept_from_ast(&mut self, ast: &SyntaxTree) -> ZiaResult<usize> {
        if let Some(c) = ast.get_concept() {
            Ok(c)
        } else if let Some(c) =
            self.snap_shot.concept_from_label(&self.delta, &ast.to_string())
        {
            Ok(c)
        } else {
            let string = &ast.to_string();
            match ast.get_expansion() {
                None => self.new_labelled_default(string),
                Some((ref left, ref right)) => {
                    let leftc = self.concept_from_ast(left)?;
                    let rightc = self.concept_from_ast(right)?;
                    let concept = self.find_or_insert_definition(
                        leftc,
                        rightc,
                        left.is_variable() || right.is_variable(),
                        false,
                    )?;
                    if !string.contains(' ') {
                        self.label(concept, string)?;
                    }
                    Ok(concept)
                },
            }
        }
    }

    fn new_labelled_default(&mut self, string: &str) -> ZiaResult<usize> {
        let new_default = self.new_default(if is_variable(string) {
            SpecificPart::variable()
        } else {
            SpecificPart::default()
        });
        self.label(new_default, string)?;
        Ok(new_default)
    }

    fn label(&mut self, concept: usize, string: &str) -> ZiaResult<()> {
        let variable = is_variable(string);
        let definition = self.find_or_insert_definition(
            S::label_id(),
            concept,
            variable,
            variable,
        )?;
        let string_id = self.new_string(string);
        self.update_reduction(definition, string_id, variable)
    }

    fn new_string(&mut self, string: impl Into<String> + Clone) -> usize {
        let index = self.snap_shot.lowest_unoccupied_concept_id(&self.delta);
        self.delta.insert_string(
            string.clone(),
            StringDelta::Insert(index),
            &mut self.cache,
        );
        self.delta.insert_concept(
            index,
            (
                ConceptDelta::Insert(
                    (SpecificPart::String(string.into()), index).into(),
                ),
                false,
            ),
            &mut self.cache,
        );
        index
    }

    fn context_search(&self) -> ContextSearch<S> {
        ContextSearch::from((&self.snap_shot, &self.delta, &self.cache))
    }

    fn find_or_insert_definition(
        &mut self,
        lefthand: usize,
        righthand: usize,
        variable: bool,
        temporary: bool,
    ) -> ZiaResult<usize> {
        let pair = self.context_search().find_definition(lefthand, righthand);
        match pair {
            None => {
                let definition = self.new_default(if variable {
                    SpecificPart::variable()
                } else {
                    SpecificPart::default()
                });
                self.insert_definition(
                    definition, lefthand, righthand, temporary,
                )?;
                Ok(definition)
            },
            Some(def) => Ok(def),
        }
    }

    fn new_default(&mut self, concept_type: SpecificPart) -> usize {
        let index = self.snap_shot.lowest_unoccupied_concept_id(&self.delta);
        self.delta.insert_concept(
            index,
            (ConceptDelta::Insert((concept_type, index).into()), false),
            &mut self.cache,
        );
        index
    }

    fn insert_definition(
        &mut self,
        definition: usize,
        lefthand: usize,
        righthand: usize,
        temporary: bool,
    ) -> ZiaResult<()> {
        if self.snap_shot.contains(&self.delta, lefthand, definition)
            || self.snap_shot.contains(&self.delta, righthand, definition)
        {
            Err(ZiaError::InfiniteDefinition)
        } else {
            self.snap_shot.check_reductions(
                &self.delta,
                definition,
                lefthand,
            )?;
            self.snap_shot.check_reductions(
                &self.delta,
                definition,
                righthand,
            )?;
            let id_array = [definition, lefthand, righthand];
            let concept_delta_array = self
                .snap_shot
                .read_concept(&self.delta, definition)
                .set_definition(definition, lefthand, righthand)?;
            concept_delta_array.iter().enumerate().for_each(
                |(i, concept_delta)| {
                    self.delta.update_concept_delta(
                        id_array[i],
                        concept_delta,
                        temporary,
                        &mut self.cache,
                    )
                },
            );
            Ok(())
        }
    }

    fn update_reduction(
        &mut self,
        concept: usize,
        reduction: usize,
        temporary: bool,
    ) -> ZiaResult<()> {
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
                            == self.snap_shot.get_reduction_of_composition(
                                &self.delta,
                                concept,
                            )
                        {
                            Err(ZiaError::RedundantReduction)
                        } else {
                            let concept_deltas = self
                                .snap_shot
                                .read_concept(&self.delta, concept)
                                .reduce_to(concept, reduction)?;
                            self.delta.update_concept_delta(
                                concept,
                                &concept_deltas[0],
                                temporary,
                                &mut self.cache,
                            );
                            self.delta.update_concept_delta(
                                reduction,
                                &concept_deltas[1],
                                temporary,
                                &mut self.cache,
                            );
                            Ok(())
                        }
                    })
            })
    }
}

impl<S> Default for Context<S>
where
    S: Default,
{
    #[must_use]
    fn default() -> Self {
        #[cfg(not(target_arch = "wasm32"))]
        let logger = {
            let plain =
                slog_term::PlainSyncDecorator::new(slog_term::TestStdoutWriter);
            Logger::root(slog_term::FullFormat::new(plain).build().fuse(), o!())
        };
        Self {
            snap_shot: S::default(),
            #[cfg(not(target_arch = "wasm32"))]
            logger,
            delta: ContextDelta::default(),
            cache: ContextCache::default(),
        }
    }
}
