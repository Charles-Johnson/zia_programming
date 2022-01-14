use seed::{C, button, div, empty, h1, p, prelude::*, style};
use crate::generated::css_classes::C;

use crate::page::home::tutorials::TUTORIALS;
use crate::{Msg, page::home::{EDGE_STYLE, TEXT_PADDING}};
use super::Msg as HomeMsg;

pub struct Model {
    pub is_open: bool,
}

impl Default for Model {
    fn default() -> Self {
        Self {
            is_open: true
        }
    }
}

pub fn view(model: &Model) -> impl IntoNodes<Msg> {
    if model.is_open {
        div![
            C![
                C.flex,
                C.sm__flex_col,
                C.border_2,
                C.border_primary,
                EDGE_STYLE,
                TEXT_PADDING,
                C.text_center,
                C.text_primary,
                C.self_stretch
            ],
            style!{
                St::Position => "fixed",
                St::Top => vh(25),
                St::Left => vw(25),
                St::Height => vh(50),
                St::Width => vw(50)
            },
            h1![C![C.flex_1], "Zia"],
            p![C![C.flex_1], "A programming language that defines itself"],
            button![
                C![
                    C.flex_1,
                    C.border_2,
                    C.bg_primary,
                    EDGE_STYLE,
                    TEXT_PADDING,
                    C.text_center,
                    C.text_secondary
                ],
                ev(Ev::Click, |_| Msg::Home(HomeMsg::StartTutorial(&TUTORIALS.0.steps))),
                TUTORIALS.0.title
            ],
            button![
                C![
                    C.flex_1,
                    C.border_2,
                    C.bg_primary,
                    EDGE_STYLE,
                    TEXT_PADDING,
                    C.text_center,
                    C.text_secondary
                ],
                ev(Ev::Click, |_| Msg::Home(HomeMsg::StartEmptySession)),
                "Start Empty Session"
            ]
        ]
    } else {
        empty![]
    }
}