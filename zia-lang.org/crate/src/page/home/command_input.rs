use super::{Model as HomeModel, Msg as HomeMsg, EDGE_STYLE, TEXT_PADDING};
use crate::{
    generated::css_classes::C,
    page::home::{tutorials, OUTER_PADDING},
    Msg as GlobalMsg,
};
use seed::{attrs, div, empty, p, prelude::*, span, style, textarea, Attrs, C};
use zia::{ConceptKind, LexemeCategory};

pub fn view(model: &HomeModel) -> impl IntoNodes<GlobalMsg> {
    let height = model.command_input.get().map_or_else(
        // flatten textarea on first render to prevent it being
        // too tall on subsequent renders
        || "0".to_owned(),
        // subsequent renders should set the height just enough to fit the text
        |e| px(e.scroll_height()),
    );
    // prevents bottom part of history being obscured by command input
    let bottom_buffer = div![style![St::Height => height],];
    let possible_explanation = if let Some(tutorials::Model {
        showing_evaluation: false,
        current_step_index,
        steps,
    }) = model.active_tutorial
    {
        let explanation = steps[current_step_index].explanation;
        p![
            style! {
                St::BackgroundColor => "rgba(255,255,255,0.5)"
            },
            explanation
        ]
    } else {
        empty!()
    };

    let textarea_class =
        C![EDGE_STYLE, TEXT_PADDING, C.outline_none, C.overflow_hidden];

    let textarea = div![
        style! {
            St::Position => "fixed",
            // required to fix the commend input to the bottom of the screen
            St::Bottom => "0",
            // without this command input is not horizontally centered
            St::Left => "0",
            St::Width => format!("calc(100% - calc(2 * {}))", OUTER_PADDING),
            St::Margin => OUTER_PADDING,
            St::Display => "flex",
            St::FlexDirection => "column"
        },
        possible_explanation,
        textarea_view(model, &height, &textarea_class).into_nodes()
    ];
    let spans: Vec<_> = model
        .context
        .lex(&model.input)
        .into_iter()
        .map(|lexeme| {
            let colour = match lexeme.category {
                LexemeCategory::Concept(ConceptKind::New) => {
                    Some(C.text_new_concept)
                },
                LexemeCategory::Concept(ConceptKind::Variable) => {
                    Some(C.text_variable_concept)
                },
                LexemeCategory::Concept(ConceptKind::Concrete) => {
                    Some(C.text_concrete_concept)
                },
                LexemeCategory::Concept(ConceptKind::Abstract) => {
                    Some(C.text_abstract_concept)
                },
                LexemeCategory::OpeningParenthesis {
                    closing_position: None,
                }
                | LexemeCategory::ClosingParenthesis {
                    opening_position: None,
                } => Some(C.text_unmatched_parenthesis),
                _ => None,
            };
            span![C![colour, C.whitespace_pre_wrap], lexeme.text]
        })
        .collect();
    let syntax_colouring = div![
        style! {
            St::Height => height,
            St::Position => "fixed",
            // required to fix the commend input to the bottom of the screen
            St::Bottom => "0",
            // without this command input is not horizontally centered
            St::Left => "0",
            St::Width => format!("calc(100% - calc(2 * {}))", OUTER_PADDING),
            St::Margin => OUTER_PADDING
        },
        &textarea_class,
        C![
            // prevents history from being visible inside command input when scrolling
            C.bg_secondary,
            // only render border of syntax colouring, not invisible text area
            C.border_primary,
            C.border_2,
        ],
        spans
    ];
    vec![bottom_buffer, syntax_colouring, textarea]
}

fn textarea_view(
    model: &HomeModel,
    height: &str,
    class: &Attrs,
) -> impl IntoNodes<GlobalMsg> {
    textarea![
        &class,
        attrs! {At::Type => "text", At::Name => "input"},
        style! {
            St::Resize => "none",
            St::Height => height,
            St::Width => percent(100),
            St::WebkitTextFillColor => "rgba(0,0,0,0)",
            St::BackgroundColor => "rgba(0,0,0,0)"
            St::AlignSelf => "flex-end"
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
