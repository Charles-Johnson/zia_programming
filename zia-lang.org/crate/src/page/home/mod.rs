mod command_input;
mod history;

use std::mem::swap;

use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{div, log, prelude::*, style, window, C};
use web_sys::HtmlTextAreaElement;
use zia::single_threaded::Context;

pub struct Model {
    context: Context,
    input: String,
    history: Vec<InterpreterHistoryEntry>,
    command_input: ElRef<HtmlTextAreaElement>,
}

impl Model {
    pub fn clear_command_input(&self) {
        let textarea_element = self.command_input.get().unwrap();
        textarea_element.set_value("");
        textarea_element.focus().unwrap();
    }
}

impl Default for Model {
    fn default() -> Self {
        Self {
            context: Context::new(),
            input: String::new(),
            history: Vec::new(),
            command_input: ElRef::new(),
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
}

pub fn update(
    msg: Msg,
    model: &mut Model,
    orders: &mut impl Orders<GlobalMsg>,
) {
    match msg {
        Msg::Input(s) => model.input = s,
        Msg::Submit => {
            let mut input = String::new();
            swap(&mut input, &mut model.input);
            let output = model.context.execute(&input);
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
                .after_next_render(|_| GlobalMsg::ClearCommandInput)
                .after_next_render(|_| {
                    let app = window()
                        .document()
                        .unwrap()
                        .get_element_by_id("app")
                        .unwrap();
                    let scroll_height = app.scroll_height();
                    // Prevents latest history from being displayed underneath command input
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
    ]
}
