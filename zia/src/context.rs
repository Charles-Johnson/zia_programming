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

use nom::InputTakeAtPosition;
use regex::Regex;

use crate::{
    and_also::AndAlso,
    associativity::Associativity,
    ast::SyntaxTree,
    concepts::{ConceptTrait, ConcreteConceptType, Hand},
    context_cache::ContextCache,
    context_delta::{ContextDelta, DirectConceptDelta, NewConceptDelta},
    context_search::{
        Comparison, ContextReferences, ContextSearch,
        Iteration as ContextSearchIteration, SharedSyntax, Syntax,
    },
    context_updater::ContextUpdater,
    delta::Apply,
    errors::{ZiaError, ZiaResult},
    lexer::{Category as LexemeCategory, Lexeme},
    map_err_variant::MapErrVariant,
    parser::parse_line,
    snap_shot::Reader as SnapShotReader,
    variable_mask_list::VariableMaskList,
};
use std::{collections::{HashMap, HashSet}, default::Default, fmt::Debug, marker::PhantomData};

#[derive(Clone)]
pub struct Context<S, C, SDCD, VML>
where
    S: SnapShotReader<SDCD>,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    for<'a> ContextSearch<'a, S, C, SDCD, VML>:
        ContextSearchIteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
    VML: VariableMaskList,
{
    snap_shot: S,
    delta: ContextDelta<S::ConceptId, SDCD>,
    cache: C,
    new_variable_concepts_by_label: HashMap<String, S::ConceptId>,
    bounded_variable_syntax: HashSet<SharedSyntax<C>>,
    _phantom: PhantomData<VML>,
}

#[derive(Debug, PartialEq)]
pub struct TokenSubsequence<SharedSyntax> {
    pub syntax: Vec<SharedSyntax>,
    pub positions: Vec<usize>,
}

impl<S, C, SDCD, VML> Context<S, C, SDCD, VML>
where
    S: SnapShotReader<SDCD> + Default + Sync + Apply<SDCD> + Debug,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    for<'a> ContextSearch<'a, S, C, SDCD, VML>:
        ContextSearchIteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: Default + ContextCache,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
    VML: VariableMaskList<Syntax = Syntax<C>>,
{
    #[must_use]
    pub fn new() -> Self {
        let mut cont = Self::default();
        cont.setup();
        cont
    }

    pub fn lex(&self, command: &str) -> Vec<Lexeme> {
        let mut lexemes = vec![];
        let mut remaining_command = command;
        //TODO convert to lazy static
        let whitespace_regex = Regex::new(r"\s").unwrap();
        while !remaining_command.is_empty() {
           if let Ok((start_of_whitespace, beginning_nonwhitespace)) = remaining_command.split_at_position::<_, ()>(|c| whitespace_regex.is_match(&c.to_string())) {
                remaining_command = start_of_whitespace;
                lexemes.push(self.concept_lexeme_from_string(beginning_nonwhitespace))
            } else {
                lexemes.push(self.concept_lexeme_from_string(remaining_command));
                return lexemes;
            }

            if let Ok((start_of_nonwhitespace, beginning_whitespace)) = remaining_command.split_at_position::<_, ()>(|c| !whitespace_regex.is_match(&c.to_string())) {
                remaining_command = start_of_nonwhitespace;
                lexemes.push(Lexeme{
                    text: beginning_whitespace.into(),
                    category: LexemeCategory::Whitespace
                })
            } else {
                lexemes.push(Lexeme {
                    text: remaining_command.into(),
                    category: LexemeCategory::Whitespace
                });
                return lexemes;
            }
        }
        return lexemes;
    }

    fn concept_lexeme_from_string(&self, concept_string: &str) -> Lexeme {
        let category = match self.snap_shot.concept_from_label(&self.delta, concept_string) {
            None => LexemeCategory::NewConcept,
            Some(id) => if self.snap_shot.read_concept(&self.delta, id).get_concrete_concept_type().is_some() {
                LexemeCategory::ConcreteConcept
            } else {
                LexemeCategory::AbstractConcept
            }
        };
        Lexeme {
            text: concept_string.into(),
            category
        }
    }

    pub fn execute(&mut self, command: &str) -> String {
        let string = self.execute_without_closing_scope(command);
        self.new_variable_concepts_by_label = HashMap::new();
        self.bounded_variable_syntax = HashSet::new();
        string
    }

    fn execute_without_closing_scope(&mut self, command: &str) -> String {
        let string = self
            .ast_from_expression(command)
            .and_then(|mut a| {
                self.create_variable_concepts(Syntax::<C>::make_mut(&mut a));
                self.call(&a)
            })
            .unwrap_or_else(|e| e.to_string());
        self.commit();
        string
    }

    pub fn create_variable_concepts(&mut self, ast: &mut Syntax<C>) {
        if let Some((left, right)) = ast.get_expansion_mut() {
            self.create_variable_concepts(left);
            self.create_variable_concepts(right);
        } else if ast.is_variable() {
            let concept_id = self
                .new_variable_concepts_by_label
                .get(&ast.to_string())
                .copied()
                .unwrap_or_else(|| {
                    let direct_delta = DirectConceptDelta::New(
                        if self.bounded_variable_syntax.contains(ast) {
                            NewConceptDelta::BoundVariable
                        } else {
                            NewConceptDelta::FreeVariable
                        },
                    );
                    let concept_id = self
                        .delta
                        .update_concept_delta(direct_delta, &mut self.cache);
                    self.new_variable_concepts_by_label
                        .insert(ast.to_string(), concept_id);
                    concept_id
                });
            ast.bind_nonquantifier_concept_as_ref(concept_id);
        }
    }

    pub fn ast_from_expression(
        &mut self,
        s: &str,
    ) -> ZiaResult<SharedSyntax<C>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(&tokens)
    }

    fn ast_from_tokens(
        &mut self,
        tokens: &[String],
    ) -> ZiaResult<SharedSyntax<C>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(&tokens[0]),
            2 => self
                .ast_from_pair(&tokens[0], &tokens[1])
                .map(Syntax::<C>::share),
            _ => {
                let TokenSubsequence {
                    syntax: lp_syntax,
                    positions: lp_indices,
                } = self.lowest_precedence_info(tokens)?;
                if lp_indices.is_empty() {
                    return Err(ZiaError::AmbiguousExpression);
                }
                let assoc =
                    lp_syntax.iter().try_fold(None, |assoc, syntax| match (
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
                    })?;
                match assoc {
                    Some(Associativity::Right) => {
                        let tail = lp_indices
                            .iter()
                            .rev()
                            .try_fold(
                                Err(ZiaError::AmbiguousExpression),
                                |state, lp_index| {
                                    Ok(Ok(self
                                        .associativity_try_fold_handler(
                                            tokens,
                                            state.ok(),
                                            *lp_index,
                                            &Associativity::Right,
                                        )?))
                                },
                            )??
                            .0;
                        if lp_indices[0] == 0 {
                            Ok(tail)
                        } else {
                            let head =
                                self.ast_from_tokens(&tokens[..lp_indices[0]])?;
                            Ok(self
                                .context_search()
                                .combine(&head, &tail)
                                .share())
                        }
                    },
                    Some(Associativity::Left) => lp_indices
                        .iter()
                        .try_fold(None, |state, lp_index| {
                            Some(self.associativity_try_fold_handler(
                                tokens,
                                state,
                                *lp_index,
                                &Associativity::Left,
                            ))
                            .transpose()
                        })?
                        .map_or(
                            Err(ZiaError::AmbiguousExpression),
                            |(syntax, _)| Ok(syntax),
                        ),
                    None => Err(ZiaError::AmbiguousExpression),
                }
            },
        }
    }

    fn associativity_try_fold_handler(
        &mut self,
        tokens: &[String],
        state: Option<(SharedSyntax<C>, usize)>,
        lp_index: usize,
        assoc: &Associativity,
    ) -> ZiaResult<(SharedSyntax<C>, usize)> {
        let mut prev_lp_index = None;
        let mut edge = None;
        if let Some((e, pli)) = state {
            edge = Some(e);
            prev_lp_index = Some(pli);
        }
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
                .share()
            }
        } else {
            self.ast_from_tokens(slice)?
        };
        Ok((
            match edge {
                None => lp_with_the_rest,
                Some(e) => match assoc {
                    Associativity::Left => {
                        self.context_search().combine(&e, &lp_with_the_rest)
                    },
                    Associativity::Right => {
                        self.context_search().combine(&lp_with_the_rest, &e)
                    },
                }
                .share(),
            },
            lp_index,
        ))
    }

    /// Determine the syntax and the positions in the token sequence of the concepts with the lowest precedence
    #[allow(clippy::too_many_lines)]
    pub fn lowest_precedence_info(
        &self,
        tokens: &[String],
    ) -> ZiaResult<TokenSubsequence<SharedSyntax<C>>> {
        let context_search = self.context_search();
        let (syntax, positions, _number_of_tokens) = tokens.iter().try_fold(
            // Initially assume no concepts have the lowest precedence
            (Vec::<SharedSyntax<C>>::new(), Vec::<usize>::new(), None),
            |(mut lowest_precedence_syntax, mut lp_indices, prev_index),
             token| {
                // Increment index
                let this_index = prev_index.map(|x| x + 1).or(Some(0));
                let raw_syntax_of_token = Syntax::<C>::from(token).share();
                let (precedence_of_token, syntax_of_token) = self
                    .snap_shot
                    .concept_from_label(&self.delta, token)
                    .map_or_else(
                        || {
                            (
                                context_search
                                    .concrete_ast(ConcreteConceptType::Default),
                                raw_syntax_of_token.clone(),
                            )
                        },
                        |c| {
                            let syntax_of_token = self
                                .snap_shot
                                .bind_concept_to_syntax(
                                    &self.delta,
                                    raw_syntax_of_token.as_ref().clone(),
                                    c,
                                )
                                .share();
                            (
                                context_search
                                    .concrete_ast(
                                        ConcreteConceptType::Precedence,
                                    )
                                    .map(|ast| {
                                        context_search
                                            .combine(&ast, &syntax_of_token)
                                            .share()
                                    }),
                                syntax_of_token,
                            )
                        },
                    );
                // Compare current token's precedence with each currently assumed lowest syntax
                for syntax in &lowest_precedence_syntax {
                    let precedence_of_syntax = if syntax.get_concept().is_some()
                    {
                        context_search
                            .concrete_ast(ConcreteConceptType::Precedence)
                            .map(|ast| {
                                context_search.combine(&ast, syntax).share()
                            })
                    } else {
                        context_search
                            .concrete_ast(ConcreteConceptType::Default)
                    };
                    match precedence_of_syntax
                        .and_also(&precedence_of_token)
                        .map_or(Comparison::Incomparable, |(pos, pot)| {
                            context_search.compare(pos, pot).0
                        }) {
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
        Ok(TokenSubsequence {
            syntax,
            positions,
        })
    }

    fn ast_from_pair(
        &mut self,
        left: &str,
        right: &str,
    ) -> ZiaResult<Syntax<C>> {
        let lefthand = self.ast_from_token(left)?;
        let righthand = self.ast_from_token(right)?;
        if Some(ConcreteConceptType::ExistsSuchThat)
            == self.concrete_type_of_ast(&righthand)
            && lefthand.is_leaf_variable()
        {
            self.bounded_variable_syntax.insert(lefthand.clone());
        }
        Ok(self.context_search().combine(&lefthand, &righthand))
    }

    fn ast_from_token(&mut self, t: &str) -> ZiaResult<SharedSyntax<C>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(t)
        } else {
            let ast =
                self.snap_shot.ast_from_symbol::<Syntax<C>>(&self.delta, t);
            Ok(ast.share())
        }
    }

    fn commit(&mut self) {
        self.snap_shot.apply(std::mem::take(&mut self.delta));
    }

    fn setup(&mut self) {
        let labels = vec![
            (":=", ConcreteConceptType::Define),
            ("->", ConcreteConceptType::Reduction),
            ("let", ConcreteConceptType::Let),
            ("true", ConcreteConceptType::True),
            ("false", ConcreteConceptType::False),
            ("assoc", ConcreteConceptType::Associativity),
            ("right", ConcreteConceptType::Right),
            ("left", ConcreteConceptType::Left),
            ("prec", ConcreteConceptType::Precedence),
            ("default", ConcreteConceptType::Default),
            (">", ConcreteConceptType::GreaterThan),
            ("=>", ConcreteConceptType::Implication),
            ("exists_such_that", ConcreteConceptType::ExistsSuchThat),
        ];
        let mut updater = ContextUpdater {
            snap_shot: &self.snap_shot,
            delta: &mut self.delta,
            cache: &mut self.cache,
        };
        let label_id = updater.new_labelled_concept(
            "label_of",
            Some(ConcreteConceptType::Label),
            None,
        );
        for (label, concrete_type) in labels {
            updater.new_labelled_concept(
                label,
                Some(concrete_type),
                Some(label_id),
            );
        }
        self.commit();
        self.execute("let (true and true) -> true");
        self.execute("let (false and _y_) -> false");
        self.execute("let (_x_ and false) -> false");
        self.execute(
            "let ((_y_ exists_such_that) (_x_ > _y_) and _y_ > _z_) => _x_ > _z_",
        );
        self.execute("let default > prec ->");
        self.execute("let (prec ->) > prec let");
    }

    fn reduce_and_call_pair(
        &mut self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ZiaResult<String> {
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
        ast: &SharedSyntax<C>,
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
        ast: &SharedSyntax<C>,
    ) -> ZiaResult<String> {
        let (normal_form, _) = &self.context_search().recursively_reduce(ast);
        if normal_form == ast {
            Err(ZiaError::CannotReduceFurther)
        } else {
            self.call(normal_form)
        }
    }

    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&mut self, ast: &SharedSyntax<C>) -> ZiaResult<String> {
        ast.get_concept()
            .and_then(|c| {
                self.snap_shot.read_concept(&self.delta, c).get_string()
            })
            .map_or_else(
                || {
                    #[allow(clippy::map_unwrap_or)]
                    // because closures need unique access to self
                    ast.get_expansion()
                        .map(|(ref left, ref right)| {
                            self.call_pair(left, right).map_err_variant(
                                &ZiaError::CannotReduceFurther,
                                || {
                                    self.try_reducing_then_call(ast)
                                        .map_err_variant(
                                            &ZiaError::CannotReduceFurther,
                                            || {
                                                Ok(self
                                                    .context_search()
                                                    .contract_pair(left, right)
                                                    .to_string())
                                            },
                                        )
                                },
                            )
                        })
                        .unwrap_or_else(|| {
                            self.try_reducing_then_call(ast).map_err_variant(
                                &ZiaError::CannotReduceFurther,
                                || {
                                    self.try_expanding_then_call(ast)
                                        .map_err_variant(
                                            &ZiaError::CannotExpandFurther,
                                            || Ok(ast.to_string()),
                                        )
                                },
                            )
                        })
                },
                Ok,
            )
    }

    fn concrete_type(
        &self,
        concept_id: S::ConceptId,
    ) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(&self.delta, concept_id)
    }

    fn concrete_type_of_ast(
        &self,
        ast: &SharedSyntax<C>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(c))
    }

    /// If the associated concept of the lefthand part of the syntax tree is LET then `call_as_righthand` is called with the left and right of the lefthand syntax. Tries to get the concept associated with the righthand part of the syntax. If the associated concept is `->` then `call` is called with the reduction of the lefthand part of the syntax. Otherwise `Err(ZiaError::NotAProgram)` is returned.
    fn call_pair(
        &mut self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ZiaResult<String> {
        self.concrete_type_of_ast(left)
            .and_then(|cct| match cct {
                ConcreteConceptType::Let => right
                    .get_expansion()
                    .and_then(|(left, right)| {
                        self.execute_let(&left, &right).and_then(|x| match x {
                            Err(
                                ZiaError::CannotReduceFurther
                                | ZiaError::UnusedSymbol,
                            ) => None,
                            _ => Some(x),
                        })
                    })
                    .or_else(|| {
                        self.context_search()
                            .concrete_ast(ConcreteConceptType::True)
                            .map(|ast| self.execute_reduction(right, &ast))
                    })
                    .map(|r| r.map(|()| "".to_string())),
                ConcreteConceptType::Label => Some(Ok("'".to_string()
                    + &right
                        .get_concept()
                        .and_then(|c| self.snap_shot.get_label(&self.delta, c))
                        .unwrap_or_else(|| right.to_string())
                    + "'")),
                _ => None,
            })
            .unwrap_or_else(|| {
                if Some(ConcreteConceptType::Reduction)
                    == self.concrete_type_of_ast(right)
                {
                    self.try_reducing_then_call(left)
                } else {
                    self.reduce_and_call_pair(left, right)
                }
            })
    }

    /// If the righthand part of the syntax can be expanded, then `match_righthand_pair` is called. If not, `Err(ZiaError::CannotExpandFurther)` is returned.
    fn execute_let(
        &mut self,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> Option<ZiaResult<()>> {
        right.get_expansion().map(|(ref rightleft, ref rightright)| {
            self.match_righthand_pair(left, rightleft, rightright)
        })
    }

    /// If the lefthand of the righthand part of the syntax is `->` then `execute_reduction` is called with the lefthand part and the righthand of the
    /// righthand part of the syntax. Similarly for `:=`, `execute_composition` is called. If the lefthand of the righthand part of the syntax is associated
    /// with a concept which isn't `->` or `:=` then if this concept reduces, `match_righthand_pair` is called with this reduced concept as an abstract syntax tree.
    fn match_righthand_pair(
        &mut self,
        left: &SharedSyntax<C>,
        rightleft: &SharedSyntax<C>,
        rightright: &SharedSyntax<C>,
    ) -> ZiaResult<()> {
        rightleft.get_concept().map_or(Err(ZiaError::UnusedSymbol), |c| {
            match self.concrete_type(c) {
                Some(ConcreteConceptType::Reduction) => {
                    self.execute_reduction(left, rightright)
                },
                Some(ConcreteConceptType::Define) => {
                    self.execute_composition(left, rightright)
                },
                _ => {
                    let rightleft_reduction = self
                        .snap_shot
                        .read_concept(&self.delta, c)
                        .get_reduction();
                    rightleft_reduction.map_or(
                        Err(ZiaError::CannotReduceFurther),
                        |r| {
                            let ast = self.context_search().to_ast(r);
                            self.match_righthand_pair(left, &ast, rightright)
                        },
                    )
                },
            }
        })
    }

    /// If the new syntax is contained within the old syntax then this returns `Err(ZiaError::InfiniteComposition)`. Otherwise `define` is called.
    fn execute_composition(
        &mut self,
        new: &SharedSyntax<C>,
        old: &SharedSyntax<C>,
    ) -> ZiaResult<()> {
        if old.contains(new) {
            Err(ZiaError::InfiniteComposition)
        } else {
            self.define(new, old)
        }
    }

    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadComposition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(
        &mut self,
        new: &SharedSyntax<C>,
        old: &SharedSyntax<C>,
    ) -> ZiaResult<()> {
        if new.get_expansion().is_some() {
            Err(ZiaError::BadComposition)
        } else {
            let mut updater = ContextUpdater {
                snap_shot: &self.snap_shot,
                delta: &mut self.delta,
                cache: &mut self.cache,
            };
            match (new.get_concept(), old.get_concept(), old.get_expansion()) {
                (_, None, None) => Err(ZiaError::RedundantRefactor),
                (None, Some(b), None) => updater.relabel(b, &new.to_string()),
                (None, Some(b), Some(_)) => {
                    if self
                        .snap_shot
                        .get_concept_of_label(updater.delta, b)
                        .is_none()
                    {
                        updater.label(b, &new.to_string())
                    } else {
                        updater.relabel(b, &new.to_string())
                    }
                },
                (None, None, Some((ref left, ref right))) => {
                    self.define_new_syntax(&new.to_string(), left, right)
                },
                (Some(a), Some(b), None) => {
                    if a == b {
                        self.cleanly_delete_composition(a)
                    } else {
                        Err(ZiaError::CompositionCollision)
                    }
                },
                (Some(a), Some(b), Some(_)) => {
                    if a == b {
                        Err(ZiaError::RedundantComposition)
                    } else {
                        Err(ZiaError::CompositionCollision)
                    }
                },
                (Some(a), None, Some((ref left, ref right))) => {
                    self.redefine(a, left, right)
                },
            }
        }
    }

    fn cleanly_delete_composition(
        &mut self,
        concept: S::ConceptId,
    ) -> ZiaResult<()> {
        match self
            .snap_shot
            .read_concept(&self.delta, concept)
            .get_composition()
        {
            None => Err(ZiaError::RedundantCompositionRemoval),
            Some((left, right)) => {
                let mut updater = ContextUpdater {
                    snap_shot: &self.snap_shot,
                    delta: &mut self.delta,
                    cache: &mut self.cache,
                };
                updater.try_delete_concept(concept)?;
                updater.try_delete_concept(left)?;
                updater.try_delete_concept(right)
            },
        }
    }

    fn redefine(
        &mut self,
        concept: S::ConceptId,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ZiaResult<()> {
        let mut updater = ContextUpdater {
            snap_shot: &self.snap_shot,
            delta: &mut self.delta,
            cache: &mut self.cache,
        };
        if let Some((left_concept, right_concept)) = self
            .snap_shot
            .read_concept(updater.delta, concept)
            .get_composition()
        {
            updater.relabel(left_concept, &left.to_string())?;
            updater.relabel(right_concept, &right.to_string())
        } else {
            let left_concept = updater.concept_from_ast(left)?;
            let right_concept = updater.concept_from_ast(right)?;
            updater.insert_composition(concept, left_concept, right_concept)
        }
    }

    /// Returns the index of a concept labelled by `syntax` and composed of concepts from `left` and `right`.
    fn define_new_syntax(
        &mut self,
        syntax: &str,
        left: &SharedSyntax<C>,
        right: &SharedSyntax<C>,
    ) -> ZiaResult<()> {
        let new_syntax_tree = left
            .get_concept()
            .and_also(&right.get_concept())
            .and_then(|(l, r)| {
                self.snap_shot
                    .read_concept(&self.delta, *l)
                    .find_as_hand_in_composition_with(*r, Hand::Left)
                    .map(|concept| {
                        let syntax = Syntax::<C>::from(syntax);
                        self.snap_shot.bind_concept_to_syntax(
                            &self.delta,
                            syntax,
                            concept,
                        )
                    })
            })
            .unwrap_or_else(|| syntax.into())
            .bind_pair(left.clone(), right.clone());
        ContextUpdater {
            snap_shot: &self.snap_shot,
            delta: &mut self.delta,
            cache: &mut self.cache,
        }
        .concept_from_ast(&new_syntax_tree)?;
        Ok(())
    }

    fn execute_reduction(
        &mut self,
        syntax: &Syntax<C>,
        normal_form: &Syntax<C>,
    ) -> ZiaResult<()> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax == normal_form {
            self.try_removing_reduction(syntax)
        } else {
            let mut updater = ContextUpdater {
                snap_shot: &self.snap_shot,
                delta: &mut self.delta,
                cache: &mut self.cache,
            };
            let syntax_concept = updater.concept_from_ast(syntax)?;
            let normal_form_concept = updater.concept_from_ast(normal_form)?;
            updater.update_reduction(syntax_concept, normal_form_concept)
        }
    }

    fn try_removing_reduction(&mut self, syntax: &Syntax<C>) -> ZiaResult<()> {
        syntax.get_concept().map_or(Err(ZiaError::RedundantReduction), |c| {
            ContextUpdater {
                snap_shot: &self.snap_shot,
                delta: &mut self.delta,
                cache: &mut self.cache,
            }
            .delete_reduction(c)
        })
    }

    fn context_search(&self) -> ContextSearch<S, C, SDCD, VML> {
        ContextSearch::from(ContextReferences {
            snap_shot: &self.snap_shot,
            delta: &self.delta,
            cache: &self.cache,
            bound_variable_syntax: &self.bounded_variable_syntax,
        })
    }
}

impl<S, C: Default, SDCD, VML> Default for Context<S, C, SDCD, VML>
where
    S: Default + SnapShotReader<SDCD>,
    for<'a> ContextSearch<'a, S, C, SDCD, VML>:
        ContextSearchIteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
    VML: VariableMaskList,
{
    #[must_use]
    fn default() -> Self {
        Self {
            snap_shot: S::default(),
            delta: ContextDelta::default(),
            cache: C::default(),
            new_variable_concepts_by_label: HashMap::new(),
            bounded_variable_syntax: HashSet::new(),
            _phantom: PhantomData,
        }
    }
}

impl<S, C: Default, SDCD, VML> From<S> for Context<S, C, SDCD, VML>
where
    S: Default + SnapShotReader<SDCD>,
    S::ConceptId: Default,
    VML: VariableMaskList,
    for<'a> ContextSearch<'a, S, C, SDCD, VML>:
        ContextSearchIteration<ConceptId = S::ConceptId, Syntax = Syntax<C>>,
    C: ContextCache,
    Syntax<C>: SyntaxTree<ConceptId = S::ConceptId>,
    SDCD: Clone
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
{
    fn from(snap_shot: S) -> Self {
        Self {
            snap_shot,
            ..Self::default()
        }
    }
}
