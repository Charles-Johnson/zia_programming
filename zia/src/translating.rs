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

use constants::{ASSOC, RIGHT, LEFT, PRECEDENCE, TRUE, FALSE};
use delta::Delta;
use errors::{ZiaError, ZiaResult};
use reading::{
    Combine, DisplayJoint, FindWhatReducesToIt, GetDefinition, GetDefinitionOf, Label,
    MaybeConcept, Pair, SyntaxReader, MaybeString, GetReduction, MightExpand
};
use std::{rc::Rc, fmt::Debug};

pub trait SyntaxConverter<T>
where
    Self: SyntaxFinder<T> + Combine<T> + SyntaxReader<T>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt + Debug + MaybeString + GetReduction,
{
    fn ast_from_expression<
        U: From<(String, Option<usize>)> + Pair<U> + MaybeConcept + DisplayJoint  + Clone + PartialEq + MightExpand<U>,
    >(
        &self,
        deltas: &[Self::Delta],
        s: &str,
    ) -> ZiaResult<Rc<U>> {
        let tokens: Vec<String> = parse_line(s);
        self.ast_from_tokens(deltas, &tokens)
    }
    fn ast_from_tokens<
        U: From<(String, Option<usize>)> + Pair<U> + MaybeConcept + DisplayJoint + Clone + PartialEq + MightExpand<U>,
    >(
        &self,
        deltas: &[Self::Delta],
        tokens: &[String],
    ) -> ZiaResult<Rc<U>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token::<U>(deltas, &tokens[0]),
            2 => self.ast_from_pair::<U>(deltas, &tokens[0], &tokens[1]),
            _ => {
                let precedence_syntax = self.to_ast(deltas, PRECEDENCE);
                let (hp_syntax, hp_indices, _number_of_tokens) = tokens.iter().try_fold(
                    (Vec::<Rc<U>>::new(), Vec::<usize>::new(), None),
                    |(highest_precedence_syntax, hp_indices, prev_index), token| {
                        let this_index = prev_index.map(|x| x + 1).or(Some(0));
                        let syntax_of_token = self.ast_from_token(deltas, token)?;
                        let comparing_precedence_of_token = self.combine(deltas, &precedence_syntax, &syntax_of_token);
                        for syntax in highest_precedence_syntax.clone() {
                            let comparing_between_tokens = self.combine(
                                deltas, &syntax, &comparing_precedence_of_token
                            );
                            match self.reduce::<U>(deltas, &comparing_between_tokens).and_then(|s| s.get_concept()) {
                                Some(TRUE) => return Ok((vec![syntax_of_token], vec![this_index.unwrap()], this_index)),
                                Some(FALSE) => return Ok((highest_precedence_syntax, hp_indices, this_index)),
                                _ => ()
                            };
                        }
                        let mut hps = highest_precedence_syntax.clone();
                        hps.push(syntax_of_token);
                        let mut hi = hp_indices.clone();
                        hi.push(this_index.unwrap());
                        return Ok((hps,hi,this_index))
                    }
                )?;
                let assoc = hp_syntax.iter().try_fold(None, |assoc, syntax| {
                    let assoc_of_hp = self.combine(deltas, &self.to_ast(deltas, ASSOC), &syntax);
                    let value = self.reduce::<U>(deltas, &assoc_of_hp);
                    match (value.and_then(|s| s.get_concept()), assoc) {
                        (Some(x), Some(y)) => if x == y {
                            Ok(Some(x))
                        } else {
                            Err(ZiaError::AmbiguousExpression)
                        },
                        (Some(x), None) => Ok(Some(x)),
                        (None, _) => Err(ZiaError::AmbiguousExpression),
                    }
                });
                match assoc? {
                    Some(RIGHT) => hp_indices.iter().rev().try_fold((None, None), |(tail, prev_hp_index), hp_index| {
                        let slice = match prev_hp_index {
                            Some(i) => &tokens[*hp_index..i],
                            None => &tokens[*hp_index..],
                        };
                        let hp_with_the_rest = self.ast_from_tokens(deltas, slice)?;
                        Ok((Some(match tail {
                            None => hp_with_the_rest,
                            Some(t) => self.combine(deltas, &hp_with_the_rest, &t),
                        }), Some(*hp_index)))
                    })?.0.ok_or(ZiaError::AmbiguousExpression),
                    Some(LEFT) => hp_indices.iter().try_fold((None, None), |(head, prev_hp_index), hp_index| {
                        let slice = match prev_hp_index {
                            Some(i) => &tokens[i..*hp_index],
                            None => &tokens[..*hp_index],
                        };
                        let hp_with_the_rest = self.ast_from_tokens(deltas, slice)?;
                        Ok((Some(match head {
                            None => hp_with_the_rest,
                            Some(h) => self.combine(deltas, &h, &hp_with_the_rest),
                        }), Some(*hp_index)))
                    })?.0.ok_or(ZiaError::AmbiguousExpression),
                    _ => Err(ZiaError::AmbiguousExpression),
                }

            }
        }
    }
    fn ast_from_pair<U: From<(String, Option<usize>)> + DisplayJoint + MaybeConcept + Pair<U>  + Clone + PartialEq + MightExpand<U>>(
        &self,
        deltas: &[Self::Delta],
        left: &str,
        right: &str,
    ) -> ZiaResult<Rc<U>> {
        let lefthand = self.ast_from_token(deltas, left)?;
        let righthand = self.ast_from_token(deltas, right)?;
        Ok(self.combine(deltas, &lefthand, &righthand))
    }
    fn ast_from_token<U: From<(String, Option<usize>)> + MaybeConcept + DisplayJoint + Pair<U>  + Clone + PartialEq + MightExpand<U>>(
        &self,
        deltas: &[Self::Delta],
        t: &str,
    ) -> ZiaResult<Rc<U>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression::<U>(deltas, t)
        } else {
            Ok(Rc::new(self.ast_from_symbol::<U>(deltas, t)))
        }
    }
}

impl<S, T> SyntaxConverter<T> for S
where
    S: SyntaxFinder<T> + Combine<T> + SyntaxReader<T>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt + Debug + MaybeString + GetReduction,
{
}

pub fn parse_line(buffer: &str) -> Vec<String> {
    let mut tokens: Vec<String> = [].to_vec();
    let mut token = String::new();
    let mut parenthesis_level = 0;
    for letter in buffer.chars() {
        parse_letter(letter, &mut parenthesis_level, &mut token, &mut tokens);
    }
    if token != "" {
        tokens.push(token.clone());
    }
    tokens
}

fn parse_letter(
    letter: char,
    parenthesis_level: &mut i8,
    token: &mut String,
    tokens: &mut Vec<String>,
) {
    match letter {
        '(' => {
            push_token(letter, *parenthesis_level, token, tokens);
            *parenthesis_level += 1;
        }
        ')' => {
            *parenthesis_level -= 1;
            push_token(letter, *parenthesis_level, token, tokens);
        }
        ' ' => push_token(letter, *parenthesis_level, token, tokens),
        '\n' | '\r' => (),
        _ => token.push(letter),
    };
}

fn push_token(letter: char, parenthesis_level: i8, token: &mut String, tokens: &mut Vec<String>) {
    if (token != "") & (parenthesis_level == 0) {
        tokens.push(token.clone());
        *token = String::new();
    }
    if parenthesis_level != 0 {
        token.push(letter);
    }
}

pub trait SyntaxFinder<T>
where
    Self: StringConcept + Label<T>,
    T: FindWhatReducesToIt + GetDefinition,
{
    fn concept_from_label(&self, deltas: &[Self::Delta], s: &str) -> Option<usize> {
        self.get_string_concept(deltas, s)
            .and_then(|c| self.get_labellee(deltas, c))
    }
    fn ast_from_symbol<U: From<(String, Option<usize>)>>(
        &self,
        deltas: &[Self::Delta],
        s: &str,
    ) -> U {
        let concept_if_exists = self.concept_from_label(deltas, s);
        U::from((s.to_string(), concept_if_exists))
    }
}

impl<S, T> SyntaxFinder<T> for S
where
    S: StringConcept + Label<T>,
    T: FindWhatReducesToIt + GetDefinition,
{
}

pub trait StringConcept
where
    Self: Delta,
{
    fn get_string_concept(&self, &[Self::Delta], &str) -> Option<usize>;
}
