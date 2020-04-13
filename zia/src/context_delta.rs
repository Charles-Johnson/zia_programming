//  Library for the Zia programming language.
// Copyright (C) 2019 Charles Johnson
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
    concepts::{Concept, ConceptDelta as CD},
    context_search::ContextCache,
    delta::{Apply, Delta},
};
use log::debug;
use std::{collections::HashMap, fmt::Debug, mem::swap};

#[derive(Clone, Default)]
pub struct ContextDelta {
    string: HashMap<String, StringDelta>,
    concept: HashMap<usize, (ConceptDelta, bool, bool)>,
}

impl ContextDelta {
    pub const fn new(
        string: HashMap<String, StringDelta>,
        concept: HashMap<usize, (ConceptDelta, bool, bool)>,
    ) -> Self {
        Self {
            string,
            concept,
        }
    }

    pub fn update_concept_delta(
        &mut self,
        concept_id: usize,
        concept_delta: &CD,
        temporary: bool,
        cache_to_invalidate: &mut ContextCache,
    ) {
        self.concept
            .entry(concept_id)
            .and_modify(|(cd, _, _)| match cd {
                ConceptDelta::Update(d) => {
                    d.combine(concept_delta.clone());
                    *cd = ConceptDelta::Update(d.clone());
                },
                ConceptDelta::Insert(c) => {
                    c.apply(concept_delta.clone());
                    *cd = ConceptDelta::Insert(c.clone());
                },
                ConceptDelta::Remove(_) => {
                    panic!("Concept will already be removed")
                },
            })
            .or_insert((
                ConceptDelta::Update(concept_delta.clone()),
                false,
                temporary,
            ));
        let mut empty_cache = ContextCache::default();
        swap(cache_to_invalidate, &mut empty_cache);
        debug!("Cache invalidated");
    }

    pub fn insert_string(
        &mut self,
        string: String,
        string_delta: StringDelta,
        cache_to_invalidate: &mut ContextCache,
    ) {
        self.string.insert(string, string_delta);
        let mut empty_cache = ContextCache::default();
        swap(cache_to_invalidate, &mut empty_cache);
        debug!("Cache invalidated");
    }

    pub const fn string(&self) -> &HashMap<String, StringDelta> {
        &self.string
    }

    pub fn insert_concept(
        &mut self,
        concept_id: usize,
        concept_delta: (ConceptDelta, bool, bool),
        cache_to_invalidate: &mut ContextCache,
    ) {
        self.concept.insert(concept_id, concept_delta);
        let mut empty_cache = ContextCache::default();
        swap(cache_to_invalidate, &mut empty_cache);
        debug!("Cache invalidated");
    }

    pub const fn concept(&self) -> &HashMap<usize, (ConceptDelta, bool, bool)> {
        &self.concept
    }

    pub fn combine_and_invalidate_cache(
        &mut self,
        other: Self,
        cache_to_invalidate: &mut ContextCache,
    ) {
        self.combine(other);
        let mut empty_cache = ContextCache::default();
        swap(cache_to_invalidate, &mut empty_cache);
        debug!("Cache invalidated");
    }
}

impl Debug for ContextDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        let mut string = "{\n".to_string();
        if !self.string.is_empty() {
            string += "    string: {\n";
            let mut unsorted_keys: Vec<&String> = self.string.keys().collect();
            unsorted_keys.sort();
            for key in unsorted_keys {
                let sd = self.string.get(key).unwrap();
                string += &format!("\t{}: {:#?},\n", key, sd);
            }
            string += "    },\n";
        }
        if !self.concept.is_empty() {
            string += "    concept: {\n";
            let mut unsorted_keys: Vec<&usize> = self.concept.keys().collect();
            unsorted_keys.sort();
            for key in unsorted_keys {
                let (cd, variable, _) = self.concept.get(key).unwrap();
                string += &format!("\t{}: {:#?}", key, cd);
                if *variable {
                    string += " (variable)";
                }
                string += ",\n";
            }
            string += "    },\n";
        }
        string += "}";
        formatter.write_str(&string)
    }
}

#[derive(Clone)]
pub enum StringDelta {
    Insert(usize),
    Remove(usize),
    Update {
        before: usize,
        after: usize,
    },
}

impl Debug for StringDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Insert(n) => format!("+ {}", n),
            Self::Remove(n) => format!("- {}", n),
            Self::Update {
                before,
                after,
            } => format!("{} -> {}", before, after),
        })
    }
}

#[derive(Clone)]
pub enum ConceptDelta {
    Insert(Concept),
    Remove(Concept),
    Update(CD),
}

impl Debug for ConceptDelta {
    fn fmt(
        &self,
        formatter: &mut std::fmt::Formatter,
    ) -> Result<(), std::fmt::Error> {
        formatter.write_str(&match *self {
            Self::Insert(ref c) => format!("+ {:#?}", c),
            Self::Remove(ref c) => format!("- {:#?}", c),
            Self::Update(ref cd) => format!("{:#?}", cd),
        })
    }
}

impl Delta for HashMap<usize, (ConceptDelta, bool, bool)> {
    fn combine(&mut self, other: Self) {
        for (other_key, (other_value, v2, temporary)) in other {
            let mut remove_key = false;
            let mut update_delta = None;
            self.entry(other_key)
                .and_modify(|(cd, v1, _)| match (cd, &other_value) {
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
                    },
                    (ConceptDelta::Insert(c), ConceptDelta::Update(cd)) => {
                        c.apply(cd.clone());
                        *v1 = v2;
                    },
                    (ConceptDelta::Update(cd1), ConceptDelta::Update(cd2)) => {
                        cd1.combine(cd2.clone());
                        *v1 = v2;
                    },
                    _ => panic!(
                        "Something went wrong when combining concept deltas!"
                    ),
                })
                .or_insert((other_value, v2, temporary));
            if remove_key {
                self.remove(&other_key);
            }
            update_delta.map(|cd| {
                self.insert(
                    other_key,
                    (ConceptDelta::Update(cd), v2, temporary),
                )
            });
        }
    }
}

impl Delta for HashMap<String, StringDelta> {
    fn combine(&mut self, other: Self) {
        for (other_key, other_sd) in other {
            let mut remove_string = false;
            let mut sd_to_update = None;
            self
                .entry(other_key.clone())
                .and_modify(|sd|
                    match (sd, &other_sd) {
                    (StringDelta::Insert(u1), StringDelta::Remove(u2)) => {
                        if u1 == u2 {
                            remove_string = true;
                        } else {
                            panic!("Tried to remove {} as concept {} when it was going to be inserted as concept {}", other_key, u2, u1);
                        }
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
                    },
                    (
                        StringDelta::Insert(u),
                        StringDelta::Update {
                            before,
                            after,
                        },
                    ) => {
                        if u == before {
                            *u = *after;
                        } else {
                            panic!("Tried to update {} from being concept {} to {} even though it was going to be inserted as {}", other_key, before, after, u);
                        }
                    },
                    (
                        StringDelta::Update {
                            after: a1,
                            ..
                        },
                        StringDelta::Update {
                            before: b2,
                            after: a2,
                        },
                    ) => {
                        if a1 == b2 {
                            *a1 = *a2;
                        } else {
                            panic!("Tried to update {} from being concept {} to {} even it was going to be updated to concept {}", other_key, b2, a2, a1);
                        }
                    },
                    (sd, other_sd) => panic!(
                        "Something went wrong when combining string delta {:#?} and {:#?} for {}!", sd, other_sd, other_key
                    ),
                })
                .or_insert(other_sd);
            if remove_string {
                self.remove(&other_key);
            }
            sd_to_update.map(|sd| self.insert(other_key, sd));
        }
    }
}

impl Delta for ContextDelta {
    fn combine(&mut self, other: Self) {
        self.concept.combine(other.concept);
        self.string.combine(other.string);
    }
}
