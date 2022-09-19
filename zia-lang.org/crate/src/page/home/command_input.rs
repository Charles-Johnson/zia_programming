use super::{Model as HomeModel, Msg as HomeMsg, EDGE_STYLE, TEXT_PADDING};
use crate::{
    generated::css_classes::C,
    page::home::{tutorials, OUTER_PADDING},
    Msg as GlobalMsg,
};
use seed::{
    attrs, button, div, empty, p, prelude::*, span, style, textarea, Attrs, C,
};
use zia::{ConceptKind, LexemeCategory};

pub fn view(model: &HomeModel) -> impl IntoNodes<GlobalMsg> {
    let height = model.command_input.get().map_or(
        // flatten textarea on first render to prevent it being
        // too tall on subsequent renders
        0,
        // subsequent renders should set the height just enough to fit the text
        |e| e.scroll_height(),
    );
    let top_of_explanation_scroll_height =
        px(model.possible_explanation.get().map_or(
            // render the bottom of the history at the bottom of the view port
            0,
            // subsequent renders should set the height just enough to see the latest command's result
            // after execution
            |e| e.scroll_height(),
        ) + height);
    let height = px(height);
    // prevents bottom part of history being obscured by command input
    let bottom_buffer =
        div![style![St::Height => top_of_explanation_scroll_height],];
    let possible_explanation = div![
        el_ref(&model.possible_explanation),
        match model.active_tutorial {
            Some(tutorials::Model {
                showing_evaluation: false,
                current_step_index,
                steps,
            }) => {
                let explanation = steps[current_step_index].explanation;
                p![explanation]
            },
            Some(tutorials::Model {
                showing_evaluation: true,
                current_step_index,
                steps,
            }) if current_step_index + 1 < steps.len() => {
                let onclick = ev(Ev::Click, move |_| {
                    GlobalMsg::Home(HomeMsg::SkipToTutorialStep {
                        steps,
                        current_step_index,
                    })
                });
                button![
                    onclick,
                    p!["Click here for next step or type and enter a command"]
                ]
            },
            _ => empty!(),
        }
    ];

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
    vec![
        bottom_buffer,
        syntax_colouring(&height, spans, &textarea_class),
        textarea,
    ]
}

fn syntax_colouring<T>(
    height: &str,
    spans: Vec<Node<T>>,
    textarea_class: &Attrs,
) -> Node<T> {
    div![
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
    ]
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
