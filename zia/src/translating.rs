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

use constants::{FALSE, PRECEDENCE, TRUE};
use delta::ApplyDelta;
use errors::{ZiaError, ZiaResult};
use reading::{
    Associativity, BindConcept, BindPair, FindWhatReducesToIt, GetDefinition, GetDefinitionOf,
    GetReduction, Label, MaybeConcept, MaybeString, MightExpand, SyntaxReader,
};
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
    str::FromStr,
};

pub trait SyntaxConverter<T, U>
where
    Self: SyntaxFinder<T> + SyntaxReader<T, U>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt + Debug + MaybeString + GetReduction,
    U: FromStr
        + BindConcept
        + BindPair
        + MaybeConcept
        + Clone
        + PartialEq
        + MightExpand
        + Display
        + Debug,
    <U as FromStr>::Err: Debug,
{
    fn ast_from_expression(&self, deltas: &Self::Delta, s: &str) -> ZiaResult<Rc<U>> {
        let tokens: Vec<String> = parse_line(s)?;
        self.ast_from_tokens(deltas, &tokens)
    }
    fn ast_from_tokens(&self, deltas: &Self::Delta, tokens: &[String]) -> ZiaResult<Rc<U>> {
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token(deltas, &tokens[0]),
            2 => self.ast_from_pair(deltas, &tokens[0], &tokens[1]),
            _ => {
                let precedence_syntax = self.to_ast(deltas, PRECEDENCE);
                let (lp_syntax, lp_indices, _number_of_tokens) = tokens.iter().try_fold(
                    (Vec::<Rc<U>>::new(), Vec::<usize>::new(), None),
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
    fn ast_from_pair(&self, deltas: &Self::Delta, left: &str, right: &str) -> ZiaResult<Rc<U>> {
        let lefthand = self.ast_from_token(deltas, left)?;
        let righthand = self.ast_from_token(deltas, right)?;
        Ok(self.combine(deltas, &lefthand, &righthand))
    }
    fn ast_from_token(&self, deltas: &Self::Delta, t: &str) -> ZiaResult<Rc<U>> {
        if t.contains(' ') || t.contains('(') || t.contains(')') {
            self.ast_from_expression(deltas, t)
        } else {
            Ok(Rc::new(self.ast_from_symbol::<U>(deltas, t)))
        }
    }
}

impl<S, T, U> SyntaxConverter<T, U> for S
where
    S: SyntaxFinder<T> + SyntaxReader<T, U>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt + Debug + MaybeString + GetReduction,
    U: FromStr
        + BindConcept
        + BindPair
        + MaybeConcept
        + Clone
        + PartialEq
        + MightExpand
        + Display
        + Debug,
    <U as FromStr>::Err: Debug,
{
}

pub fn parse_line(buffer: &str) -> ZiaResult<Vec<String>> {
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

pub trait SyntaxFinder<T>
where
    Self: StringConcept + Label<T>,
    T: FindWhatReducesToIt + GetDefinition,
{
    fn concept_from_label(&self, deltas: &Self::Delta, s: &str) -> Option<usize> {
        self.get_string_concept(deltas, s)
            .and_then(|c| self.get_labellee(deltas, c))
    }
    fn ast_from_symbol<U: FromStr + BindConcept>(&self, deltas: &Self::Delta, s: &str) -> U
    where
        <U as FromStr>::Err: Debug,
    {
        self.concept_from_label(deltas, s)
            .map(|concept| s.parse::<U>().unwrap().bind_concept(concept))
            .unwrap_or_else(|| s.parse().unwrap())
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
    Self: ApplyDelta,
{
    fn get_string_concept(&self, &Self::Delta, &str) -> Option<usize>;
}
