use crate::{generated::css_classes::C, Msg as GlobalMsg};
use seed::{attrs, class, div, input, p, prelude::*};
use zia::single_threaded::Context;

pub struct Model {
    context: Context,
    input: String,
    output: String,
}

impl Default for Model {
    fn default() -> Self {
        Self {
            context: Context::new(),
            input: String::new(),
            output: String::new(),
        }
    }
}

#[derive(Clone)]
pub enum Msg {
    Input(String),
    Submit,
    Nothing,
}

pub fn update(msg: Msg, model: &mut Model) {
    match msg {
        Msg::Input(s) => model.input = s,
        Msg::Submit => {
            model.output = model.context.execute(&model.input);
            model.input = String::new();
        },
        Msg::Nothing => {},
    };
}

pub fn view(model: &Model) -> impl View<GlobalMsg> {
    div![
        class![C.flex, C.flex_col, C.justify_center, C.flex_1,],
        vec![
            input![
                class![C.border_primary, C.border_2],
                attrs! {At::Type => "text", At::Name => "input", At::Value => model.input},
                input_ev(Ev::Input, |s| GlobalMsg::Home(Msg::Input(s))),
                keyboard_ev("keydown", |ev| if ev.key_code() == 13 {
                    GlobalMsg::Home(Msg::Submit)
                } else {
                    GlobalMsg::Home(Msg::Nothing)
                }),
            ],
            p![model.output]
        ]
    ]
}
