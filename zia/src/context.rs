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
    associativity::Associativity,
    ast::{GenericSyntaxTree, SyntaxKey, SyntaxTree},
    concepts::{ConceptTrait, ConcreteConceptType},
    context_cache::GenericCache,
    context_delta::{
        DirectConceptDelta, NestedDelta, NewConceptDelta, SharedDelta,
    },
    context_search::{Comparison, ContextReferences, ContextSearch},
    context_updater::ContextUpdater,
    delta::Apply,
    errors::{ZiaError, ZiaResult},
    lexer::{Category as LexemeCategory, ConceptKind, Lexeme},
    map_err_variant::MapErrVariant,
    mixed_concept::{ConceptId, MixedConcept},
    nester::{NestedSyntaxTree, SharedReference},
    parser::parse_line,
    reduction_reason::SharedSyntax,
    snap_shot::Reader as SnapShotReader,
};
use std::{
    collections::{HashMap, HashSet},
    default::Default,
    fmt::Debug,
    marker::PhantomData,
};

pub struct Context<S, SDCD, D, CCI: ConceptId, SR: SharedReference>
where
    S: SnapShotReader<SDCD, SR, ConceptId = CCI>
        + Default
        + Sync
        + Apply<SDCD, SR>
        + Debug,
    for<'a> GenericSyntaxTree<CCI, SR>: SyntaxTree<SR, ConceptId = CCI>
        + std::convert::From<&'a std::string::String>,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<CCI>>
        + From<DirectConceptDelta<CCI>>,
    D: SharedDelta<NestedDelta = NestedDelta<CCI, SDCD, D, SR>>,
{
    snap_shot: S,
    delta: D,
    cache: GenericCache<CCI, SR>,
    new_variable_concepts_by_label: HashMap<String, CCI>,
    bounded_variable_syntax: HashSet<SyntaxKey<CCI>>,
    phantom: PhantomData<SDCD>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TokenSubsequence<SharedSyntax> {
    pub syntax: Vec<SharedSyntax>,
    pub positions: Vec<usize>,
}
impl<S, SDCD, D, CCI: ConceptId, SR: SharedReference> Clone
    for Context<S, SDCD, D, CCI, SR>
where
    S: SnapShotReader<SDCD, SR, ConceptId = CCI>
        + Default
        + Sync
        + Apply<SDCD, SR>
        + Debug
        + Clone,
    for<'a> GenericSyntaxTree<CCI, SR>: SyntaxTree<SR, ConceptId = S::ConceptId>
        + std::convert::From<&'a std::string::String>,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<CCI>>
        + From<DirectConceptDelta<CCI>>,
    D: SharedDelta<NestedDelta = NestedDelta<CCI, SDCD, D, SR>>,
{
    fn clone(&self) -> Self {
        Self {
            delta: D::default(),
            phantom: self.phantom,
            bounded_variable_syntax: self.bounded_variable_syntax.clone(),
            cache: self.cache.clone(),
            new_variable_concepts_by_label: self
                .new_variable_concepts_by_label
                .clone(),
            snap_shot: self.snap_shot.clone(),
        }
    }
}

impl<S, SDCD, D, CCI: MixedConcept, SR: SharedReference>
    Context<S, SDCD, D, CCI, SR>
where
    S: SnapShotReader<SDCD, SR, ConceptId = CCI>
        + Default
        + Sync
        + Apply<SDCD, SR>
        + Debug,
    for<'a> GenericSyntaxTree<CCI, SR>: SyntaxTree<SR, ConceptId = CCI>
        + std::convert::From<&'a std::string::String>,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<CCI>>
        + From<DirectConceptDelta<CCI>>,
    D: SharedDelta<NestedDelta = NestedDelta<CCI, SDCD, D, SR>>,
{
    pub fn new() -> ZiaResult<Self> {
        let mut cont = Self::default();
        cont.setup()?;
        Ok(cont)
    }

    pub fn lex(&self, command: &str) -> Vec<Lexeme> {
        let mut lexemes = vec![];
        let mut opening_parentheses_positions = vec![];
        for character in command.chars() {
            match character {
                '(' => {
                    opening_parentheses_positions.push(lexemes.len());
                    lexemes.push(Lexeme {
                        text: "(".into(),
                        category: LexemeCategory::OpeningParenthesis {
                            closing_position: None,
                        },
                    });
                },
                ')' => {
                    let opening_position = opening_parentheses_positions.pop();
                    if let Some(op) = opening_position {
                        let cp = lexemes.len();
                        if let LexemeCategory::OpeningParenthesis {
                            closing_position,
                        } = &mut lexemes[op].category
                        {
                            *closing_position = Some(cp);
                        }
                    }
                    lexemes.push(Lexeme {
                        text: ")".into(),
                        category: LexemeCategory::ClosingParenthesis {
                            opening_position,
                        },
                    });
                },
                _ if character.is_whitespace() => {
                    if let Some(Lexeme {
                        category: LexemeCategory::Whitespace,
                        text,
                    }) = lexemes.last_mut()
                    {
                        text.push(character);
                    } else {
                        lexemes.push(Lexeme {
                            text: character.to_string(),
                            category: LexemeCategory::Whitespace,
                        });
                    }
                },
                _ => {
                    if let Some(
                        lexeme @ Lexeme {
                            category: LexemeCategory::Concept(_),
                            ..
                        },
                    ) = lexemes.last_mut()
                    {
                        lexeme.text.push(character);
                        lexeme.category = LexemeCategory::Concept(
                            self.concept_kind_from_symbol(&lexeme.text),
                        );
                    } else {
                        let text = character.to_string();
                        let concept_kind = self.concept_kind_from_symbol(&text);
                        lexemes.push(Lexeme {
                            text,
                            category: LexemeCategory::Concept(concept_kind),
                        });
                    }
                },
            }
        }
        lexemes
    }

    fn nest(&self, _lexemes: Vec<Lexeme>) -> NestedSyntaxTree<CCI, SR> {
        let _nested_syntax: NestedSyntaxTree<CCI, SR>;
        todo!("Nest lexemes according to grammar");
    }

    fn concept_kind_from_symbol(&self, symbol: &str) -> ConceptKind {
        if symbol.starts_with('_') && symbol.ends_with('_') {
            return ConceptKind::Variable;
        }
        self.snap_shot.concept_from_label(self.delta.as_ref(), symbol).map_or(
            ConceptKind::New,
            |id| {
                if self
                    .snap_shot
                    .read_concept(self.delta.as_ref(), id)
                    .get_concrete_concept_type()
                    .is_some()
                {
                    ConceptKind::Concrete
                } else {
                    ConceptKind::Abstract
                }
            },
        )
    }

    pub fn execute(&mut self, command: &str) -> String {
        let string = self.execute_without_closing_scope(command);
        self.new_variable_concepts_by_label = HashMap::new();
        self.bounded_variable_syntax = HashSet::new();
        string.unwrap()
    }

    fn execute_without_closing_scope(
        &mut self,
        command: &str,
    ) -> ZiaResult<String> {
        let string = self
            .ast_from_expression(command)
            .and_then(|mut a| {
                a = self.context_search().expand(&a);
                self.create_variable_concepts(
                    GenericSyntaxTree::<CCI, SR>::make_mut(&mut a),
                )
                .unwrap();
                self.call(&a)
            })
            .unwrap_or_else(|e| e.to_string());
        self.commit()?;
        Ok(string)
    }

    pub fn create_variable_concepts(
        &mut self,
        ast: &mut GenericSyntaxTree<CCI, SR>,
    ) -> ZiaResult<()> {
        if let Some((left, right)) = ast.get_expansion_mut() {
            self.create_variable_concepts(left)?;
            self.create_variable_concepts(right)?;
        } else if ast.is_variable() {
            let concept_id = self
                .new_variable_concepts_by_label
                .get(&ast.to_string())
                .copied()
                .or_else(|| {
                    let direct_delta = DirectConceptDelta::New(
                        if self.bounded_variable_syntax.contains(&ast.key()) {
                            NewConceptDelta::BoundVariable
                        } else {
                            NewConceptDelta::FreeVariable
                        },
                    );
                    let maybe_inner_delta = self.delta.get_mut();
                    let delta = maybe_inner_delta?;
                    // This might lead to variables being committed to snap shot unnecessarily
                    let concept_id = delta
                        .update_concept_delta(direct_delta, &mut self.cache);
                    self.new_variable_concepts_by_label
                        .insert(ast.to_string(), concept_id);
                    Some(concept_id)
                })
                .ok_or(ZiaError::MultiplePointersToDelta)?;
            ast.bind_nonquantifier_concept_as_ref(concept_id);
        }
        Ok(())
    }

    pub fn ast_from_expression(
        &mut self,
        s: &str,
    ) -> ZiaResult<SharedSyntax<CCI, SR>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(&tokens)
    }

    fn ast_from_tokens(
        &mut self,
        tokens: &[String],
    ) -> ZiaResult<SharedSyntax<CCI, SR>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(&tokens[0]),
            2 => self
                .ast_from_pair(&tokens[0], &tokens[1])
                .map(GenericSyntaxTree::<CCI, SR>::share),
            _ => {
                let TokenSubsequence {
                    syntax: lp_syntax,
                    positions: lp_indices,
                } = self.lowest_precedence_info(tokens)?;
                if lp_indices.is_empty() {
                    return Err(ZiaError::LowestPrecendenceNotFound {
                        tokens: tokens.to_vec(),
                    });
                }
                // TODO: redesign how opposing associativity is handled. Refer to this issue #69
                let assoc =
                    lp_syntax.iter().try_fold(None, |assoc, syntax| match (
                        self.context_search().get_associativity(syntax),
                        assoc,
                    ) {
                        (x, Some(y)) => {
                            if x == y {
                                Ok(Some(x))
                            } else {
                                Err(ZiaError::DifferentAssociativityAmongstLowestPrecendenceTokens{token: syntax.to_string(), associativity: x, other_associativity: y})
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
                                            Associativity::Right,
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
                                Associativity::Left,
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
        state: Option<(SharedSyntax<CCI, SR>, usize)>,
        lp_index: usize,
        assoc: Associativity,
    ) -> ZiaResult<(SharedSyntax<CCI, SR>, usize)> {
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
    ) -> ZiaResult<TokenSubsequence<SharedSyntax<CCI, SR>>> {
        let context_search = self.context_search();
        let (syntax, positions, _number_of_tokens) = tokens.iter().try_fold(
            // Initially assume no concepts have the lowest precedence
            (Vec::<SharedSyntax<CCI, SR>>::new(), Vec::<usize>::new(), None),
            |(mut lowest_precedence_syntax, mut lp_indices, prev_index),
             token| {
                // Increment index
                let this_index = prev_index.map(|x| x + 1).or(Some(0));
                let raw_syntax_of_token =
                    GenericSyntaxTree::<CCI, SR>::from(token).share();
                let (precedence_of_token, syntax_of_token) = self
                    .snap_shot
                    .concept_from_label(self.delta.as_ref(), token)
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
                                    self.delta.as_ref(),
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
                    }
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
    ) -> ZiaResult<GenericSyntaxTree<CCI, SR>> {
        let lefthand = self.ast_from_token(left)?;
        let righthand = self.ast_from_token(right)?;
        if Some(ConcreteConceptType::ExistsSuchThat)
            == self.concrete_type_of_ast(&righthand)
            && lefthand.is_leaf_variable()
        {
            self.bounded_variable_syntax.insert(lefthand.key());
        }
        Ok(self.context_search().combine(&lefthand, &righthand))
    }

    fn ast_from_token(&mut self, t: &str) -> ZiaResult<SharedSyntax<CCI, SR>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(t)
        } else {
            let ast = self
                .snap_shot
                .ast_from_symbol::<GenericSyntaxTree<CCI, SR>, D>(
                    self.delta.as_ref(),
                    t,
                );
            Ok(ast.share())
        }
    }

    fn commit(&mut self) -> ZiaResult<()> {
        let taken_delta = std::mem::take(&mut self.delta);
        let delta = taken_delta.into_nested()?;
        self.snap_shot.apply(delta);
        Ok(())
    }

    fn label_concrete_concepts(&mut self) -> ZiaResult<()> {
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
        let maybe_inner_delta = self.delta.get_mut();
        let Some(delta) = maybe_inner_delta else {
            return Err(ZiaError::MultiplePointersToDelta);
        };
        let mut updater = ContextUpdater {
            snap_shot: &self.snap_shot,
            delta,
            cache: &mut self.cache,
            phantom: PhantomData,
            phantom2: PhantomData,
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
        Ok(())
    }

    fn setup(&mut self) -> ZiaResult<()> {
        self.label_concrete_concepts()?;
        self.commit()?;
        self.execute("let (true and true) -> true");
        self.execute("let (false and _y_) -> false");
        self.execute("let (_x_ and false) -> false");
        self.execute(
            "let ((_y_ exists_such_that) (_x_ > _y_) and _y_ > _z_) => _x_ > _z_",
        );
        self.execute("let default > prec ->");
        self.execute("let (prec ->) > prec :=");
        self.execute("let (prec :=) > prec let");
        Ok(())
    }

    fn reduce_and_call_pair(
        &mut self,
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
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

    /// If the abstract syntax tree can be reduced, then `call` is called with this reduction. If not then an `Err(ZiaError::CannotReduceFurther)` is returned
    fn try_reducing_then_call(
        &mut self,
        ast: &SharedSyntax<CCI, SR>,
    ) -> ZiaResult<String> {
        let context_search = self.context_search();
        let (normal_form, _) = &context_search.recursively_reduce(ast);
        if normal_form.key() == ast.key() {
            context_search
                .find_examples_of_inferred_reduction(ast)
                .map(|(normal_form, _)| normal_form.to_string())
                .ok_or(ZiaError::CannotReduceFurther)
        } else {
            self.call(normal_form)
        }
    }

    /// If the associated concept of the syntax tree is a string concept that that associated string is returned. If not, the function tries to expand the syntax tree. If that's possible, `call_pair` is called with the lefthand and righthand syntax parts. If not `try_expanding_then_call` is called on the tree. If a program cannot be found this way, `Err(ZiaError::NotAProgram)` is returned.
    fn call(&mut self, ast: &SharedSyntax<CCI, SR>) -> ZiaResult<String> {
        ast.get_concept()
            .and_then(|c| {
                self.snap_shot.read_concept(self.delta.as_ref(), c).get_string()
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
                                || Ok(ast.to_string()),
                            )
                        })
                },
                Ok,
            )
    }

    fn concrete_type(&self, concept_id: &CCI) -> Option<ConcreteConceptType> {
        self.snap_shot.concrete_concept_type(self.delta.as_ref(), *concept_id)
    }

    fn concrete_type_of_ast(
        &self,
        ast: &SharedSyntax<CCI, SR>,
    ) -> Option<ConcreteConceptType> {
        ast.get_concept().and_then(|c| self.concrete_type(&c))
    }

    /// If the associated concept of the lefthand part of the syntax tree is LET then `call_as_righthand` is called with the left and right of the lefthand syntax. Tries to get the concept associated with the righthand part of the syntax. If the associated concept is `->` then `call` is called with the reduction of the lefthand part of the syntax. Otherwise `Err(ZiaError::NotAProgram)` is returned.
    fn call_pair(
        &mut self,
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
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
                        let cs = self.context_search();
                        let maybe_ast =
                            cs.concrete_ast(ConcreteConceptType::True);
                        drop(cs);
                        maybe_ast.map(|ast| self.execute_reduction(right, &ast))
                    })
                    .map(|r| r.map(|()| String::new())),
                ConcreteConceptType::Label => Some(Ok("'".to_string()
                    + &right
                        .get_concept()
                        .and_then(|c| {
                            self.snap_shot.get_label(self.delta.as_ref(), c)
                        })
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
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
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
        left: &SharedSyntax<CCI, SR>,
        rightleft: &SharedSyntax<CCI, SR>,
        rightright: &SharedSyntax<CCI, SR>,
    ) -> ZiaResult<()> {
        rightleft.get_concept().map_or(Err(ZiaError::UnusedSymbol), |c| {
            match self.concrete_type(&c) {
                Some(ConcreteConceptType::Reduction) => {
                    self.execute_reduction(left, rightright)
                },
                Some(ConcreteConceptType::Define) => {
                    self.execute_composition(left, rightright)
                },
                _ => {
                    let rightleft_reduction = self
                        .snap_shot
                        .read_concept(self.delta.as_ref(), c)
                        .get_reduction();
                    rightleft_reduction.map_or(
                        Err(ZiaError::CannotReduceFurther),
                        |r| {
                            let ast = self.context_search().to_ast(&r);
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
        new: &SharedSyntax<CCI, SR>,
        old: &SharedSyntax<CCI, SR>,
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
        new: &SharedSyntax<CCI, SR>,
        old: &SharedSyntax<CCI, SR>,
    ) -> ZiaResult<()> {
        let maybe_inner_delta = self.delta.get_mut();
        let Some(delta) = maybe_inner_delta else {
            return Err(ZiaError::MultiplePointersToDelta);
        };
        let mut updater = ContextUpdater {
            snap_shot: &self.snap_shot,
            delta,
            cache: &mut self.cache,
            phantom: PhantomData,
            phantom2: PhantomData,
        };
        match (
            new.get_concept(),
            new.get_expansion(),
            old.get_concept(),
            old.get_expansion(),
        ) {
            (_, Some(_), None, None) => Err(ZiaError::BadComposition),
            (_, None, None, None) => Err(ZiaError::RedundantRefactor),
            (None, _, Some(b), None) => updater.relabel(b, &new.to_string()),
            (None, _, Some(b), Some(_)) => {
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
            (None, _, None, Some((ref left, ref right))) => {
                updater.define_new_syntax(&new.to_string(), left, right)
            },
            (Some(a), _, Some(b), None) => {
                if a == b {
                    updater.cleanly_delete_composition(&a)
                } else {
                    Err(ZiaError::CompositionCollision)
                }
            },
            (Some(a), _, Some(b), Some(_)) => {
                if a == b {
                    Err(ZiaError::RedundantComposition)
                } else {
                    Err(ZiaError::CompositionCollision)
                }
            },
            (Some(a), _, None, Some((ref left, ref right))) => {
                updater.redefine(&a, left, right)
            },
        }
    }

    fn execute_reduction(
        &mut self,
        syntax: &SharedSyntax<CCI, SR>,
        normal_form: &SharedSyntax<CCI, SR>,
    ) -> ZiaResult<()> {
        if normal_form.contains(syntax) {
            Err(ZiaError::ExpandingReduction)
        } else if syntax.key() == normal_form.key() {
            self.try_removing_reduction(syntax)
        } else {
            let maybe_inner_delta = self.delta.get_mut();
            let Some(delta) = maybe_inner_delta else {
                return Err(ZiaError::MultiplePointersToDelta);
            };
            let mut updater = ContextUpdater {
                snap_shot: &self.snap_shot,
                delta,
                cache: &mut self.cache,
                phantom: PhantomData,
                phantom2: PhantomData,
            };
            let syntax_concept = updater.concept_from_ast(syntax)?;
            let normal_form_concept = updater.concept_from_ast(normal_form)?;
            updater.update_reduction(syntax_concept, normal_form_concept)
        }
    }

    fn try_removing_reduction(
        &mut self,
        syntax: &GenericSyntaxTree<CCI, SR>,
    ) -> ZiaResult<()> {
        let maybe_inner_delta = self.delta.get_mut();
        let Some(delta) = maybe_inner_delta else {
            return Err(ZiaError::MultiplePointersToDelta);
        };
        let snap_shot = &self.snap_shot;
        let cache = &mut self.cache;
        syntax.get_concept().map_or(Err(ZiaError::RedundantReduction), |c| {
            ContextUpdater {
                cache,
                delta,
                snap_shot,
                phantom: PhantomData,
                phantom2: PhantomData,
            }
            .delete_reduction(c)
        })
    }

    fn context_search(&self) -> ContextSearch<S, SDCD, D, CCI, SR> {
        ContextSearch::from(ContextReferences {
            snap_shot: &self.snap_shot,
            delta: self.delta.clone(),
            cache: &self.cache,
            bound_variable_syntax: &self.bounded_variable_syntax,
        })
    }
}

impl<S, SDCD, D, CCI: ConceptId, SR: SharedReference> Default
    for Context<S, SDCD, D, CCI, SR>
where
    S: SnapShotReader<SDCD, SR, ConceptId = CCI>
        + Default
        + Sync
        + Apply<SDCD, SR>
        + Debug,
    for<'a> GenericSyntaxTree<CCI, SR>: SyntaxTree<SR, ConceptId = S::ConceptId>
        + std::convert::From<&'a std::string::String>,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
    D: SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D, SR>>,
{
    #[must_use]
    fn default() -> Self {
        Self {
            snap_shot: S::default(),
            delta: D::default(),
            cache: GenericCache::<CCI, SR>::default(),
            new_variable_concepts_by_label: HashMap::new(),
            bounded_variable_syntax: HashSet::new(),
            phantom: PhantomData,
        }
    }
}

impl<S, SDCD, D, CCI: ConceptId, SR: SharedReference> From<S>
    for Context<S, SDCD, D, CCI, SR>
where
    S: SnapShotReader<SDCD, SR, ConceptId = CCI>
        + Default
        + Sync
        + Apply<SDCD, SR>
        + Debug,
    for<'a> GenericSyntaxTree<CCI, SR>: SyntaxTree<SR, ConceptId = S::ConceptId>
        + std::convert::From<&'a std::string::String>,
    SDCD: Clone
        + Debug
        + AsRef<DirectConceptDelta<S::ConceptId>>
        + From<DirectConceptDelta<S::ConceptId>>,
    D: SharedDelta<NestedDelta = NestedDelta<S::ConceptId, SDCD, D, SR>>,
{
    fn from(snap_shot: S) -> Self {
        Self {
            snap_shot,
            ..Self::default()
        }
    }
}
