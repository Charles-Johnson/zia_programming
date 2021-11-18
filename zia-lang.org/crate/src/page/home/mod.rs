mod command_input;

use std::mem::swap;

use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{div, p, prelude::*, style, C};
use web_sys::HtmlElement;
use zia::single_threaded::Context;

pub struct Model {
    context: Context,
    input: String,
    history: Vec<InterpreterHistoryEntry>,
    command_input: ElRef<HtmlElement>,
}

impl Model {
    pub fn focus_on_command_input(&self) {
        self.command_input.get().unwrap().focus().unwrap();
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
            orders.after_next_render(|_| GlobalMsg::FocusOnCommandInput);
        },
    };
}

pub static EDGE_STYLE: &str = C.rounded_28px;
pub static TEXT_PADDING: &str = C.p_2;

pub fn view(model: &Model) -> impl IntoNodes<GlobalMsg> {
    div![
        C![C.flex, C.flex_col, C.flex_1, C.justify_end, C.p_4],
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
        }),
        command_input::view(model).into_nodes(),
    ]
}
