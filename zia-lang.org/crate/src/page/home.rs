use std::mem::swap;

use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{attrs, div, p, prelude::*, style, textarea, C};
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

static EDGE_STYLE: &str = C.rounded_28px;
static TEXT_PADDING: &str = C.p_2;

pub fn view(model: &Model) -> impl IntoNodes<GlobalMsg> {
    let command_input = textarea![
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
        input_ev(Ev::Input, |s| GlobalMsg::Home(Msg::Input(s))),
        keyboard_ev("keydown", |ev| (ev.key_code() == 13).then(|| {
            // prevents textarea from retaining the text
            ev.prevent_default();
            ev.stop_propagation();
            GlobalMsg::Home(Msg::Submit)
        })),
        &model.input
    ];
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
        command_input,
    ]
}
