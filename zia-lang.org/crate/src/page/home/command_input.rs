use crate::{generated::css_classes::C, Msg as GlobalMsg};
use super::{Msg as HomeMsg, EDGE_STYLE, TEXT_PADDING, Model as HomeModel};
use seed::{attrs, prelude::*, style, textarea, C};

pub fn view(model: &HomeModel) -> impl IntoNodes<GlobalMsg> {
    textarea![
        C![
            C.border_primary,
            C.border_2,
            EDGE_STYLE,
            TEXT_PADDING,
            C.outline_none,
            C.overflow_hidden
        ],
        attrs! {At::Type => "text", At::Name => "input"},
        style! {St::Resize => "none", St::Height => model.command_input.get().map_or_else(
            // flatten textarea on first render to prevent it being
            // too tall on subsequent renders
            || "0".to_owned(),
            // subsequent renders should set the height just enough to fit the text
            |e| px(e.scroll_height())
        )},
        el_ref(&model.command_input),
        input_ev(Ev::Input, |s| GlobalMsg::Home(HomeMsg::Input(s))),
        keyboard_ev("keydown", |ev| (ev.key_code() == 13).then(|| {
            // prevents textarea from retaining the text
            ev.prevent_default();
            ev.stop_propagation();
            GlobalMsg::Home(HomeMsg::Submit)
        })),
        &model.input
    ]
}