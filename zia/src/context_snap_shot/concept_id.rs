use std::{
    convert::TryFrom,
    fmt::{Debug, Display},
};

use slotmap::{DefaultKey, Key, KeyData};

#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Committed(DefaultKey);

impl Display for Committed {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Key for Committed {
    fn data(&self) -> slotmap::KeyData {
        self.0.data()
    }
}

impl From<KeyData> for Committed {
    fn from(kd: KeyData) -> Self {
        Self(DefaultKey::from(kd))
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ConceptId {
    Committed(Committed),
    Uncommitted(Uncommitted),
}

pub type Uncommitted = usize;

impl Display for ConceptId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Committed(cci) => {
                Display::fmt("CommittedConceptId: ", f)?;
                Display::fmt(cci, f)
            },
            Self::Uncommitted(uci) => {
                Display::fmt("UncommittedConceptId: ", f)?;
                Display::fmt(uci, f)
            },
        }
    }
}

impl From<Committed> for ConceptId {
    fn from(cci: Committed) -> Self {
        Self::Committed(cci)
    }
}

impl From<Uncommitted> for ConceptId {
    fn from(uci: Uncommitted) -> Self {
        Self::Uncommitted(uci)
    }
}

impl TryFrom<ConceptId> for Committed {
    type Error = ();

    fn try_from(value: ConceptId) -> Result<Self, Self::Error> {
        if let ConceptId::Committed(cci) = value {
            Ok(cci)
        } else {
            Err(())
        }
    }
}

impl TryFrom<ConceptId> for Uncommitted {
    type Error = ();

    fn try_from(value: ConceptId) -> Result<Self, Self::Error> {
        if let ConceptId::Uncommitted(cci) = value {
            Ok(cci)
        } else {
            Err(())
        }
    }
}
