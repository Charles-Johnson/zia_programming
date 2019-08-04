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

use delta::Delta;
use errors::{ZiaError, ZiaResult};
use reading::{
    Combine, DisplayJoint, FindWhatReducesToIt, GetDefinition, GetDefinitionOf, Label,
    MaybeConcept, Pair,
};
use std::rc::Rc;

pub trait SyntaxConverter<T>
where
    Self: SyntaxFinder<T> + Combine<T>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt,
{
    fn ast_from_expression<
        U: From<(String, Option<usize>)> + Pair<U> + MaybeConcept + DisplayJoint,
    >(
        &self,
        deltas: &[Self::Delta],
        s: &str,
    ) -> ZiaResult<Rc<U>> {
        let tokens: Vec<String> = parse_line(s);
        match tokens.len() {
            0 => Err(ZiaError::EmptyParentheses),
            1 => self.ast_from_token::<U>(deltas, &tokens[0]),
            2 => self.ast_from_pair::<U>(deltas, &tokens[0], &tokens[1]),
            _ => Err(ZiaError::AmbiguousExpression),
        }
    }
    fn ast_from_pair<U: From<(String, Option<usize>)> + DisplayJoint + MaybeConcept + Pair<U>>(
        &self,
        deltas: &[Self::Delta],
        left: &str,
        right: &str,
    ) -> ZiaResult<Rc<U>> {
        let lefthand = self.ast_from_token(deltas, left)?;
        let righthand = self.ast_from_token(deltas, right)?;
        Ok(self.combine(deltas, &lefthand, &righthand))
    }
    fn ast_from_token<U: From<(String, Option<usize>)> + MaybeConcept + DisplayJoint + Pair<U>>(
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
    S: SyntaxFinder<T> + Combine<T>,
    T: GetDefinitionOf + GetDefinition + FindWhatReducesToIt,
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
