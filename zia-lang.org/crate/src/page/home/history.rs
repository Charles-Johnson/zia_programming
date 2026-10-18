use super::{EntryKind, Model as HomeModel, EDGE_STYLE, TEXT_PADDING};
use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{div, p, prelude::*, style, C};

pub fn view(model: &HomeModel) -> impl IntoNodes<GlobalMsg> {
    div![
        C![C.flex, C.flex_col, C.justify_end],
        model.history.iter().map(|entry| {
            div![
                match entry.kind {
                    EntryKind::Command => C![
                        C.bg_primary,
                        C.text_secondary,
                        C.self_end,
                        TEXT_PADDING,
                        EDGE_STYLE
                    ],
                    EntryKind::Evaluation => C![
                        C.text_primary,
                        C.self_start,
                        TEXT_PADDING,
                        EDGE_STYLE
                    ],
                },
                style!{St::MaxWidth => percent(70), St::OverflowWrap => "break-word"},
                p![&entry.value]
            ]
        })
    ]
}
