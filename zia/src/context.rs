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

use maplit::hashset;

use crate::{
    associativity::Associativity,
    ast::{GenericSyntaxTree, SyntaxKey, SyntaxLeaf},
    concepts::{ConceptTrait, ConcreteConceptType, Hand},
    context_cache::{GenericCache, SharedSyntax},
    context_delta::{
        DirectConceptDelta, NestedDelta, NewConceptDelta, SharedDelta,
    },
    context_search::{ContextReferences, ContextSearch},
    context_updater::ContextUpdater,
    delta::Apply,
    errors::{ZiaError, ZiaResult},
    lexer::{Category as LexemeCategory, ConceptKind, Lexeme},
    map_err_variant::MapErrVariant,
    mixed_concept::{ConceptId, MixedConcept},
    nester::{NestedSyntaxTree, Node, SharedReference},
    snap_shot::Reader as SnapShotReader,
};
use std::{
    collections::{BinaryHeap, HashMap, HashSet},
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

pub struct TokenSubsequence<CI: ConceptId, SR: SharedReference> {
    pub syntax: Vec<SR::Share<GenericSyntaxTree<CI, SR>>>,
    pub positions: BinaryHeap<usize>,
}

impl<CI: ConceptId, SR: SharedReference> Debug for TokenSubsequence<CI, SR> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TokenSubsequence")
            .field(
                "syntax",
                &self
                    .syntax
                    .iter()
                    .map(std::convert::AsRef::as_ref)
                    .collect::<Vec<_>>(),
            )
            .field("positions", &self.positions)
            .finish()
    }
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

    pub fn lex(&self, command: &str) -> Vec<Lexeme<CCI>> {
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

    pub(crate) fn nest(
        lexemes: Vec<Lexeme<CCI>>,
    ) -> ZiaResult<NestedSyntaxTree<CCI, SR>> {
        let mut nested_syntax_at_depth =
            HashMap::<usize, NestedSyntaxTree<CCI, SR>>::new();
        let mut nest_depth = 0;
        for lexeme in lexemes {
            match lexeme.category {
                LexemeCategory::Whitespace => continue,
                LexemeCategory::Concept(c) => {
                    let new_nested_syntax =
                        NestedSyntaxTree::from_concept_kind(&c, lexeme.text);
                    let nested_syntax =
                        match nested_syntax_at_depth.remove(&nest_depth) {
                            None => new_nested_syntax,
                            Some(ns) => ns.append_node(new_nested_syntax),
                        };
                    let existing_value = nested_syntax_at_depth
                        .insert(nest_depth, nested_syntax);
                    assert!(existing_value.is_none(), "A value for the nested_syntax at depth {nest_depth} previously exists");
                },
                LexemeCategory::OpeningParenthesis {
                    ..
                } => {
                    nest_depth += 1;
                },
                LexemeCategory::ClosingParenthesis {
                    ..
                } => {
                    let Some(mut nested_syntax) =
                        nested_syntax_at_depth.remove(&nest_depth)
                    else {
                        return Err(ZiaError::EmptyParentheses);
                    };
                    if nest_depth == 0 {
                        return Err(ZiaError::UnmatchedParentheses);
                    }
                    nest_depth -= 1;
                    if let Some(ns) = nested_syntax_at_depth.remove(&nest_depth)
                    {
                        nested_syntax = ns.append_node(nested_syntax.nest());
                    } else {
                        nested_syntax = nested_syntax.nest();
                    };

                    nested_syntax_at_depth.insert(nest_depth, nested_syntax);
                },
            }
        }
        if nest_depth != 0 {
            return Err(ZiaError::UnmatchedParentheses);
        }
        let Some(nested_syntax) = nested_syntax_at_depth.remove(&0) else {
            return Err(ZiaError::EmptyExpression);
        };
        Ok(nested_syntax)
    }

    fn concept_kind_from_symbol(&self, symbol: &str) -> ConceptKind<CCI> {
        if symbol.starts_with('_') && symbol.ends_with('_') {
            return ConceptKind::Variable;
        }
        self.snap_shot.concept_from_label(self.delta.as_ref(), symbol).map_or(
            ConceptKind::New,
            |id| {
                self
                    .snap_shot
                    .read_concept(self.delta.as_ref(), id)
                    .get_concrete_concept_type().map_or(ConceptKind::Abstract {
                        id,
                    } , |concrete_type|
                {
                    ConceptKind::Concrete {
                        id,
                        concrete_type,
                    }
                })
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
        let lexemes = self.lex(s);
        let nested_syntax = Self::nest(lexemes)?;
        let syntax_list = self.syntax_list_for_nested_syntax(&nested_syntax)?;
        self.ast_from_syntax_list(&syntax_list)
    }

    pub fn syntax_list_for_nested_syntax(
        &mut self,
        NestedSyntaxTree {
            node,
            syntax,
            concept,
            ..
        }: &NestedSyntaxTree<CCI, SR>,
    ) -> ZiaResult<Vec<SharedSyntax<CCI, SR>>> {
        match node {
            Node::Parent {
                children,
            } => children
                .iter()
                .map(|n| self.ast_from_nested_syntax(n))
                .collect(),
            Node::Leaf(leaf) => {
                Ok(vec![self.ast_from_syntax_leaf(*leaf, syntax, concept)?])
            },
        }
    }

    /// Panics if `syntax_list.is_empty()`
    pub fn ast_from_syntax_list(
        &mut self,
        syntax_list: &[SharedSyntax<CCI, SR>],
    ) -> ZiaResult<SharedSyntax<CCI, SR>> {
        match syntax_list {
            [] => panic!("ast_from_syntax_list called with empty list!"),
            [s] => return Ok(s.clone()),
            [l, r] => {
                return Ok(GenericSyntaxTree::<CCI, SR>::new_pair(
                    l.clone(),
                    r.clone(),
                )
                .share())
            },
            _ => {},
        };
        let token_subsequence = self.lowest_precedence_info(syntax_list);
        let TokenSubsequence {
            syntax: lp_syntax,
            positions: mut lp_indices,
        } = token_subsequence;
        if dbg!(&lp_indices).is_empty() {
            return Err(ZiaError::LowestPrecendenceNotFound {
                tokens: syntax_list.iter().map(|s| s.to_string()).collect(),
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
                let mut min = None;
                let mut tail: Result<(<SR as SharedReference>::Share<GenericSyntaxTree<CCI, SR>>, usize), _> = Err(ZiaError::AmbiguousExpression);
                while let Some(lp_index) = lp_indices.pop() {
                    if Some(lp_index) == min {continue;}
                    min = Some(lp_index);
                    let tail_result = self.associativity_try_fold_handler(
                        syntax_list,
                        tail.ok(),
                        lp_index,
                        Associativity::Right,
                    );
                    tail = Ok(tail_result?); 
                };
                let tail = tail?.0;
                let Some(min) = min else {return Err(ZiaError::AmbiguousExpression)};
                if min == 0 {
                    Ok(tail)
                } else {
                    let head = self
                        .ast_from_syntax_list(&syntax_list[..min ])?;
                    Ok(self.context_search().combine(&head, &tail).share())
                }
            },
            Some(Associativity::Left) => lp_indices
                .iter()
                .try_fold(None, |state, lp_index| {
                    Some(self.associativity_try_fold_handler(
                        syntax_list,
                        state,
                        *lp_index,
                        Associativity::Left,
                    ))
                    .transpose()
                })?
                .map_or(Err(ZiaError::AmbiguousExpression), |(syntax, _)| {
                    Ok(syntax)
                }), // TODO: taken into account that lp_indices was turned from Vec to BinaryHeap
            None => Err(ZiaError::AmbiguousExpression),
        }
    }

    fn ast_from_syntax_leaf(
        &mut self,
        l: SyntaxLeaf,
        syntax: &str,
        concept: &Option<CCI>,
    ) -> ZiaResult<SharedSyntax<CCI, SR>> {
        let concept_id = concept.map_or_else(
            || {
                let maybe_inner_delta = self.delta.get_mut();
                let Some(delta) = maybe_inner_delta else {
                    return Err(ZiaError::MultiplePointersToDelta);
                };
                if let Some(id) =
                    self.snap_shot.concept_from_label(delta, syntax)
                {
                    return Ok(id);
                }
                let label_id = {
                    self.snap_shot
                        .concrete_concept_id(delta, ConcreteConceptType::Label)
                };
                let mut updater = ContextUpdater {
                    snap_shot: &self.snap_shot,
                    delta,
                    cache: &mut self.cache,
                    phantom: PhantomData,
                    phantom2: PhantomData,
                };
                Ok(updater.new_labelled_concept(syntax, None, label_id))
            },
            Ok,
        )?;
        let mut gst = match l {
            crate::ast::SyntaxLeaf::Variable => {
                GenericSyntaxTree::<CCI, SR>::new_leaf_variable(concept_id)
            },
            crate::ast::SyntaxLeaf::Constant => {
                GenericSyntaxTree::<CCI, SR>::new_constant_concept(concept_id)
            },
            crate::ast::SyntaxLeaf::Quantifier => {
                GenericSyntaxTree::<CCI, SR>::new_quantifier_concept(concept_id)
            },
        };
        gst.syntax = Some(syntax.to_string());
        Ok(gst.share())
    }

    fn ast_from_nested_syntax(
        &mut self,
        nested_syntax: &NestedSyntaxTree<CCI, SR>,
    ) -> ZiaResult<SharedSyntax<CCI, SR>> {
        match &nested_syntax.node {
            crate::nester::Node::Leaf(l) => self.ast_from_syntax_leaf(
                *l,
                &nested_syntax.syntax,
                &nested_syntax.concept,
            ),
            crate::nester::Node::Parent {
                children: nodes,
            } => {
                if let [left, right] = &nodes[..] {
                    let left_syntax =
                        self.ast_from_nested_syntax(left.as_ref())?;
                    let right_syntax =
                        self.ast_from_nested_syntax(right.as_ref())?;
                    Ok(self.ast_from_pair(&left_syntax, &right_syntax).share())
                } else {
                    let syntax_list =
                        self.syntax_list_for_nested_syntax(nested_syntax)?;
                    self.ast_from_syntax_list(&syntax_list)
                }
            },
        }
    }

    fn associativity_try_fold_handler(
        &mut self,
        tokens: &[SharedSyntax<CCI, SR>],
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
        // Required otherwise self.ast_from_tokens will return Err(ZiaError::EmptyParentheses)
        if slice.is_empty() {
            return Err(ZiaError::AmbiguousExpression);
        }
        let edge_index = match assoc {
            Associativity::Left => slice.len() - 1,
            Associativity::Right => 0,
        };
        let lp_with_the_rest = if lp_index == edge_index {
            let edge_syntax = slice[edge_index].clone();
            if slice.len() == 1 {
                edge_syntax
            } else {
                match assoc {
                    Associativity::Left => {
                        let rest_of_syntax = if slice.len() < 3 {
                            &slice[slice.len() - 1]
                        } else {
                            &self.ast_from_syntax_list(
                                &slice[..slice.len() - 1],
                            )?
                        };
                        self.context_search()
                            .combine(rest_of_syntax, &edge_syntax)
                    },
                    Associativity::Right => {
                        let rest_of_syntax = if slice.len() < 3 {
                            &slice[1]
                        } else {
                            &self.ast_from_syntax_list(&slice[1..])?
                        };
                        self.context_search()
                            .combine(&edge_syntax, rest_of_syntax)
                    },
                }
                .share()
            }
        } else {
            self.ast_from_syntax_list(slice)?
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
        syntax_children: &[SR::Share<GenericSyntaxTree<CCI, SR>>],
    ) -> TokenSubsequence<CCI, SR> {
        // TODO: Search for existing precedence relations where either the left and right side
        // are one of the `syntax_children` elements
        //   i) If no precedence relations are found, then just assume that all tokens have the
        //   lowest precedence. No need to save the relative precedence of the syntax children
        //   ii) If any precedence relations are found then out of the concepts that they
        //   reference, find the ones with the lowest precedence and also include any concepts that
        //   weren't referenced and assume that also have the lowest precedence.
        //
        // Can find the existing precendence relations by using `self.snap_shot.read_concept`,
        // combine with ConcreteConceptType::Preceeds,
        // `Concept::iter_hand_of(Hand::Right)` and filter where the left hand ID is one of the
        // syntax children. Then reduce these precedence relations to find the lowest precedence
        // IDs
        let mut lp_indices = BinaryHeap::<usize>::new();
        let mut concept_ids = vec![];
        let mut index_of_concept = HashMap::<CCI, HashSet<usize>>::new();
        let mut lp_indices_without_concepts = vec![];
        for (index, syntax) in syntax_children.iter().enumerate() {
            if let Some(id) = syntax.get_concept() {
                index_of_concept.entry(id).or_default().insert(index);
                concept_ids.push(id);
            } else {
                lp_indices_without_concepts.push(index);
            }
        }
                                    
        let precede_concept_id = self.snap_shot.concrete_concept_id(self.delta.as_ref(), ConcreteConceptType::Preceeds).expect("Precede concept must exist");
        let mut concepts_with_precedence_relations = hashset!{};
        let precede_concept = self.snap_shot.read_concept(self.delta.as_ref(), precede_concept_id);
        for (right_id, comp_id) in precede_concept.iter_hand_of(Hand::Left) {
                if concept_ids.contains(&right_id) {
                    concepts_with_precedence_relations.insert(right_id);
                }

            let comp_concept = self.snap_shot.read_concept(self.delta.as_ref(), comp_id);
            for (left_id, _) in comp_concept.iter_hand_of(Hand::Right) {
                if concept_ids.contains(&left_id) {
                    concepts_with_precedence_relations.insert(left_id);
                }
            }
        }

        let preceeds = |a: CCI, b: CCI| {
            let context_search = self.context_search();
            let preceeds_syntax = context_search.concrete_ast(ConcreteConceptType::Preceeds).unwrap();
            self.concrete_type_of_ast(&context_search.recursively_reduce(
                &context_search.combine(
                    &context_search.to_ast(&a),
                    &context_search.combine(
                        &preceeds_syntax,
                        &context_search.to_ast(&b)
                    ).share()
                ).share()
            ).0)
        };
        let mut lowest_precedence_concepts = hashset! {};
        'a: for concept_id in &concepts_with_precedence_relations {
             'b: for lowest_precedence_concept in &lowest_precedence_concepts {
                match dbg!(preceeds(dbg!(*concept_id), dbg!(*lowest_precedence_concept))) {
                        Some(ConcreteConceptType::True) => {
                            continue 'a;
                        },
                        Some(ConcreteConceptType::False) => {
                            match preceeds(*lowest_precedence_concept, *concept_id) {
                                Some(ConcreteConceptType::True) => {
                                    lowest_precedence_concepts = hashset!{*concept_id};
                                    continue 'a;
                                },
                                Some(ConcreteConceptType::False) => {
                                    lowest_precedence_concepts.insert(*concept_id);
                                    continue 'a;
                                },
                                Some(ConcreteConceptType::Right) | None => {
                                    continue 'b; 
                                },
                                Some(cct) => panic!("{lowest_precedence_concept} precedes {concept_id} reduces to {cct:?}")
                            } 
                        },
                        Some(ConcreteConceptType::Right) | None => {
                            match dbg!(preceeds(dbg!(*lowest_precedence_concept), dbg!(*concept_id))) {
                                Some(ConcreteConceptType::True) => {
                                    lowest_precedence_concepts = hashset!{*concept_id};
                                    continue 'a;
                                },
                                Some(ConcreteConceptType::False|ConcreteConceptType::Right)| None => {
                                    continue 'b;
                                },
                                Some(cct) => panic!("{lowest_precedence_concept} precedes {concept_id} reduces to {cct:?}")
                            }
                        },
                        Some(cct) => panic!("{concept_id} precedes {lowest_precedence_concept} reduces to {cct:?}"),
                    } 
            }
            lowest_precedence_concepts.insert(*concept_id);
        }
        let mut syntax = vec![];
        if lowest_precedence_concepts.is_empty() {
            let (more_lp_indices, more_syntax): (Vec<_>, Vec<_>) = concept_ids.into_iter().filter(|id| {
                !concepts_with_precedence_relations.contains(id)
            }).flat_map(|id| {
                index_of_concept[&id].iter().map(|index| {
                    (*index, syntax_children[*index].clone())
                })
            }).unzip();
            lp_indices.extend(lp_indices_without_concepts);
            lp_indices.extend(more_lp_indices);
            syntax.extend(more_syntax);
        }
        for lowest_precedence_concept in lowest_precedence_concepts {
            lp_indices.extend(index_of_concept[&lowest_precedence_concept].iter().inspect(|&index| {
                syntax.push(syntax_children[*index].clone());
            }));
        }

        TokenSubsequence{syntax, positions: lp_indices}
    }

    fn ast_from_pair(
        &mut self,
        left: &SharedSyntax<CCI, SR>,
        right: &SharedSyntax<CCI, SR>,
    ) -> GenericSyntaxTree<CCI, SR> {
        if Some(ConcreteConceptType::ExistsSuchThat)
            == self.concrete_type_of_ast(right)
            && left.is_leaf_variable()
        {
            self.bounded_variable_syntax.insert(left.key());
        }
        self.context_search().combine(left, right)
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
            (">", ConcreteConceptType::GreaterThan),
            ("=>", ConcreteConceptType::Implication),
            ("exists_such_that", ConcreteConceptType::ExistsSuchThat),
            ("preceeds", ConcreteConceptType::Preceeds),
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
        let result = self.execute("let (true and true) -> true"); 
        debug_assert_eq!(result, "");
        let result =self.execute("let (false and _y_) -> false"); 
        debug_assert_eq!(result, "");
        let result = self.execute("let (_x_ and false) -> false"); 
        debug_assert_eq!(result, "");
        let result = self.execute(
            "let ((_y_ exists_such_that) (_x_ preceeds _y_) and _y_ preceeds _z_) => _x_ preceeds _z_",
        ); 
        debug_assert_eq!(result, "");
        let result = self.execute("let -> preceeds :="); 
        debug_assert_eq!(result, "");
        let result = self.execute("let := preceeds let"); 
        debug_assert_eq!(result, "");
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

    fn updater(&mut self)  -> ZiaResult<ContextUpdater<S, SDCD, D, SR>> {
        let maybe_inner_delta = self.delta.get_mut();
        let Some(delta) = maybe_inner_delta else {
            return Err(ZiaError::MultiplePointersToDelta);
        };
        Ok(ContextUpdater {
            snap_shot: &self.snap_shot,
            delta,
            cache: &mut self.cache,
            phantom: PhantomData,
            phantom2: PhantomData,
        })
    }

    /// If the new syntax is an expanded expression then this returns `Err(ZiaError::BadComposition)`. Otherwise the result depends on whether the new or old syntax is associated with a concept and whether the old syntax is an expanded expression.
    fn define(
        &mut self,
        new: &SharedSyntax<CCI, SR>,
        old: &SharedSyntax<CCI, SR>,
    ) -> ZiaResult<()> {
        match (
            new.get_concept(),
            new.get_expansion(),
            old.get_concept(),
            old.get_expansion(),
        ) {
            (_, Some(_), _, None) => Err(ZiaError::BadComposition),
            (_, None, None, None) => Err(ZiaError::RedundantRefactor),
            (None, _, Some(b), None) => self.updater()?.relabel(b, &old.to_string(), &new.to_string()),
            (None, _, Some(b), Some(_)) => {
                let syntax = {let search = self.context_search();
                    let syntax = search.to_ast(&b).to_string(); 
                    drop(search);
                    syntax};
                if self
                    .snap_shot
                    .get_concept_of_label(self.delta.as_ref(), b)
                    .is_none()
                {
                    self.updater()?.label(b, &syntax, &new.to_string())
                } else {
                    self.updater()?.relabel(b, &old.to_string(), &new.to_string())
                }
            },
            (None, _, None, Some((ref left, ref right))) => {
                self.updater()?.define_new_syntax(&new.to_string(), left, right)
            },
            (Some(a), a_comp, Some(b), None) => {
                if a == b {
                    let composition = self
                        .snap_shot
                        .read_concept(self.delta.as_ref(), a)
                        .get_composition(); 
                    match composition 
                    {
                        None => Err(ZiaError::RedundantCompositionRemoval),
                        Some((left, right)) => {
                            let (left_syntax, right_syntax) = {
                                let context_search = self.context_search();
                                (context_search.to_ast(&left).to_string(), context_search.to_ast(&right).to_string())
                            };
                            let mut updater = self.updater()?;
                            updater.try_delete_concept(a, &a.to_string())?;
                            updater.try_delete_concept(left, &left_syntax)?;
                            updater.try_delete_concept(right, &right_syntax)
                        },
                    }
                } else if a_comp.is_none() {
                    if self.snap_shot.get_concept(b).is_some() {
                        let mut updater = self.updater()?;
                        updater.unlabel(a, &a.to_string())?;
                        updater.relabel(b, &old.to_string(), &new.to_string())
                    } else {
                        Err(ZiaError::RedundantRefactor)
                    }
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
            (Some(a), Some((ref new_left, ref new_right)), None, Some((ref left, ref right))) => {
                self.updater()?.redefine_composition(&a, left, right, &new_left.to_string(), &new_right.to_string() )
            },
            (Some(a), None, None, Some((ref left, ref right))) => {
                self.updater()?.redefine(&a, left, right)
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
            updater.update_reduction(syntax_concept, syntax.to_string(), normal_form_concept)
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
        syntax.get_concept().map_or_else(
            || Err(ZiaError::RedundantReduction {
                syntax: syntax.to_string(),
            }),
            |c| {
                ContextUpdater {
                    cache,
                    delta,
                    snap_shot,
                    phantom: PhantomData,
                    phantom2: PhantomData,
                }
                .delete_reduction(c, syntax.to_string())
            },
        )
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
