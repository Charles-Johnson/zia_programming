use super::{Model as HomeModel, Msg as HomeMsg, EDGE_STYLE, TEXT_PADDING};
use crate::{Msg as GlobalMsg, generated::css_classes::C, page::home::{OUTER_PADDING, tutorials}};
use seed::{C, attrs, div, empty, p, prelude::*, style, textarea};

pub fn view(model: &HomeModel) -> impl IntoNodes<GlobalMsg> {
    let height = model.command_input.get().map_or_else(
        // flatten textarea on first render to prevent it being
        // too tall on subsequent renders
        || "0".to_owned(),
        // subsequent renders should set the height just enough to fit the text
        |e| px(e.scroll_height()),
    );
    // prevents bottom part of history being obscured by command input
    let div = div![style![St::Height => height]];
    let possible_explanation = if let Some(tutorials::Model{
        showing_evaluation: false,
        current_step_index,
        steps
    }) = model.active_tutorial {
        let explanation = steps[current_step_index].explanation;
        p![
            style!{
                St::BackgroundColor => "rgba(255,255,255,0.5)"
            },
            explanation
        ]
    } else {
        empty!()
    };

    let textarea = div![
        style!{
            St::Position => "fixed",
            // required to fix the commend input to the bottom of the screen
            St::Bottom => "0",
            // without this command input is not horizontally centered
            St::Left => "0",
            St::Width => format!("calc(100% - calc(2 * {}))", OUTER_PADDING),
            St::Margin => OUTER_PADDING
        },
        possible_explanation,
        textarea_view(&model, height).into_nodes()
    ];
    vec![div, textarea]
}

fn textarea_view(model: &HomeModel, height: String) -> impl IntoNodes<GlobalMsg> {
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
        style! {
            St::Resize => "none",
            St::Height => height,
            St::Width => percent(100)
        },
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