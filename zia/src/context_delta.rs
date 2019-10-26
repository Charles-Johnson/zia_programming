/*  Library for the Zia programming language.
    Copyright (C) 2019 Charles Johnson

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

use concepts::{Concept, ConceptDelta as CD};
use delta::{ApplyDelta, Delta};
use std::collections::{HashMap, hash_map::Entry};

#[derive(Clone, Debug, Default)]
pub struct ContextDelta {
    pub string: HashMap<String, StringDelta>,
    pub concept: HashMap<usize, (ConceptDelta, bool)>,
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


pub fn update_concept_delta(entry: Entry<usize, (ConceptDelta, bool)>, concept_delta: &CD) {
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
        .or_insert((ConceptDelta::Update(concept_delta.clone()), false));
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