mod command_input;
mod history;
mod menu;
mod tutorials;

use std::mem::swap;

use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{div, log, prelude::*, style, window, C};
use web_sys::HtmlTextAreaElement;
use zia::single_threaded::Context;

use self::tutorials::TutorialStep;

pub struct Model {
    context: Context,
    input: String,
    history: Vec<InterpreterHistoryEntry>,
    command_input: ElRef<HtmlTextAreaElement>,
    menu: menu::Model,
    active_tutorial: Option<tutorials::Model>,
}

impl Default for Model {
    fn default() -> Self {
        Self {
            context: Context::new(),
            input: String::new(),
            history: Vec::new(),
            command_input: ElRef::new(),
            menu: menu::Model::default(),
            active_tutorial: None,
        }
    }
}

struct InterpreterHistoryEntry {
    kind: EntryKind,
    value: String,
}

enum EntryKind {
    Command,
    Evaluation,
}

#[derive(Clone)]
pub enum Msg {
    Input(String),
    Submit,
    StartEmptySession,
    StartTutorial(&'static [TutorialStep]),
    SetCommandInput(String),
}

pub fn update(
    msg: Msg,
    model: &mut Model,
    orders: &mut impl Orders<GlobalMsg>,
) {
    match msg {
        Msg::SetCommandInput(s) => {
            let textarea_element = model.command_input.get().unwrap();
            textarea_element.set_value(&s);
            textarea_element.focus().unwrap();
            model.input = s;
        },
        Msg::Input(s) => model.input = s,
        Msg::StartEmptySession => model.menu.is_open = false,
        Msg::StartTutorial(steps) => {
            model.menu.is_open = false;
            let new_input = steps[0].command.to_string();
            model.active_tutorial = Some(tutorials::Model {
                current_step_index: 0,
                steps,
                showing_evaluation: false,
            });
            orders.after_next_render(|_| {
                GlobalMsg::Home(Msg::SetCommandInput(new_input))
            });
        },
        Msg::Submit => {
            let mut input = String::new();
            swap(&mut input, &mut model.input);
            let output = model.context.execute(&input);
            let new_input = if let Some(tutorial_model) =
                &mut model.active_tutorial
            {
                if output.is_empty() {
                    tutorial_model.current_step_index += 1;
                    if (tutorial_model.current_step_index
                        < tutorial_model.steps.len())
                    {
                        tutorial_model.steps[tutorial_model.current_step_index]
                            .command
                    } else {
                        ""
                    }
                } else {
                    tutorial_model.showing_evaluation = true;
                    ""
                }
            } else {
                ""
            }
            .into();
            model.history.push(InterpreterHistoryEntry {
                value: input,
                kind: EntryKind::Command,
            });
            model.history.push(InterpreterHistoryEntry {
                value: output,
                kind: EntryKind::Evaluation,
            });
            log!(&model.input);
            orders
                .after_next_render(|_| {
                    GlobalMsg::Home(Msg::SetCommandInput(new_input))
                })
                .after_next_render(|_| {
                    let app = window()
                        .document()
                        .unwrap()
                        .get_element_by_id("app")
                        .unwrap();
                    let scroll_height = app.scroll_height();
                    // Prevents latest history from being displayed underneath command input
                    // Doesn't seem to work in android browsers but works in android PWA
                    app.set_scroll_top(scroll_height);
                });
        },
    };
}

pub static EDGE_STYLE: &str = C.rounded_28px;
pub static TEXT_PADDING: &str = C.p_2;
pub static OUTER_PADDING: &str = "1rem";

pub fn view(model: &Model) -> impl IntoNodes<GlobalMsg> {
    div![
        C![C.flex, C.flex_col, C.flex_1, C.justify_end],
        style! {St::Padding => OUTER_PADDING},
        history::view(model).into_nodes(),
        command_input::view(model).into_nodes(),
        menu::view(&model.menu).into_nodes()
    ]
}
