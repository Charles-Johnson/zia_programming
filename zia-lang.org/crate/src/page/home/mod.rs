mod command_input;
mod history;
mod menu;
mod tutorials;

use std::mem::swap;

use crate::{generated::css_classes::C, Msg as GlobalMsg};
use js_sys::Array;
use seed::{div, log, prelude::*, style, window, C};
use web_sys::{console::error, HtmlDivElement, HtmlTextAreaElement};
use zia::single_threaded::Context;

use self::tutorials::TutorialStep;

pub struct Model {
    context: Context,
    input: String,
    history: Vec<InterpreterHistoryEntry>,
    command_input: ElRef<HtmlTextAreaElement>,
    possible_explanation: ElRef<HtmlDivElement>,
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
            possible_explanation: ElRef::new(),
            menu: menu::Model::default(),
            active_tutorial: None,
        }
    }
}

impl Model {
    fn reset(&mut self) {
        self.context = Context::new();
        self.history = Vec::new();
        self.menu.is_open = false;
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
    ToggleMenu,
    Input(String),
    Submit,
    StartEmptySession,
    StartTutorial(&'static [TutorialStep]),
    SetCommandInput(String),
    SkipToTutorialStep {
        steps: &'static [TutorialStep],
        current_step_index: usize,
    },
}

pub fn update(
    msg: Msg,
    model: &mut Model,
    orders: &mut impl Orders<GlobalMsg>,
) {
    match msg {
        Msg::ToggleMenu => {
            model.menu.is_open = !model.menu.is_open;
        },
        Msg::SkipToTutorialStep {
            steps,
            current_step_index,
        } => {
            let next_step_index = current_step_index + 1;
            model.active_tutorial = Some(tutorials::Model {
                steps,
                current_step_index: next_step_index,
                showing_evaluation: false,
            });
            let new_input = steps[next_step_index].command.to_string();
            orders.after_next_render(|_| {
                GlobalMsg::Home(Msg::SetCommandInput(new_input))
            });
        },
        Msg::SetCommandInput(s) => {
            if let Some(textarea_element) = model.command_input.get() {
                textarea_element.set_value(&s);
                textarea_element.focus().unwrap();
                model.input = s;
                orders.after_next_render(|_| {
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
            } else {
                let array = Array::new();
                array.push(&JsValue::from_str(
                    "No command input text area element found!",
                ));
                error(&array);
            }
        },
        Msg::Input(s) => model.input = s,
        Msg::StartEmptySession => {
            model.reset();
        },
        Msg::StartTutorial(steps) => {
            model.reset();
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
                    if tutorial_model.current_step_index
                        < tutorial_model.steps.len()
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
            orders.after_next_render(|_| {
                GlobalMsg::Home(Msg::SetCommandInput(new_input))
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
