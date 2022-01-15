// @TODO: uncomment once https://github.com/rust-lang/rust/issues/54726 stable
//#![rustfmt::skip::macros(class)]

#![allow(clippy::used_underscore_binding)]
#![allow(clippy::non_ascii_literal)]
#![allow(clippy::enum_glob_use)]

mod generated;
mod page;

use console_error_panic_hook::set_once;
use generated::css_classes::C;
use seed::{div, document, log, prelude::*, C};

use page::home;

const TITLE_SUFFIX: &str = "Zia";
const IMAGES_PATH: &str = "static/images";

// ------ ------
//     Model
// ------ ------

pub struct Model {
    pub page: Page,
    pub home_page_model: home::Model,
}

// ------ Page ------

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Page {
    Home,
    NotFound,
}

impl Page {
    #[must_use]
    pub const fn to_href(self) -> &'static str {
        match self {
            Self::Home => "/",
            Self::NotFound => "/404",
        }
    }
}

impl From<Url> for Page {
    #[must_use]
    fn from(url: Url) -> Self {
        let (page, title) = match url.path().first().map(String::as_str) {
            None | Some("") => (Self::Home, TITLE_SUFFIX.to_owned()),
            _ => (Self::NotFound, format!("404 - {}", TITLE_SUFFIX)),
        };
        document().set_title(&title);
        page
    }
}

// ------ ------
//  After Mount
// ------ ------

fn init(url: Url, orders: &mut impl Orders<Msg>) -> Model {
    orders.subscribe(Msg::UrlChanged).after_next_render(|_| {
        Msg::Home(home::Msg::SetCommandInput("".into()))
    });

    Model {
        page: url.into(),
        home_page_model: home::Model::default(),
    }
}

// ------ ------
//    Routes
// ------ ------
#[must_use]
// ------ ------
//    Update
// ------ ------
#[derive(Clone)]
pub enum Msg {
    UrlChanged(subs::UrlChanged),
    Home(home::Msg),
}

pub fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::UrlChanged(subs::UrlChanged(url)) => {
            model.page = url.into();
        },
        Msg::Home(hm) => home::update(hm, &mut model.home_page_model, orders),
    }
}

// ------ ------
//     View
// ------ ------

// Notes:
// - \u{00A0} is the non-breaking space
//   - https://codepoints.net/U+00A0
//
// - "▶\u{fe0e}" - \u{fe0e} is the variation selector, it prevents ▶ to change to emoji in some browsers
//   - https://codepoints.net/U+FE0E
#[must_use]
pub fn view(model: &Model) -> impl IntoNodes<Msg> {
    // @TODO: Setup `prerendered` properly once https://github.com/David-OConnor/seed/issues/223 is resolved
    let prerendered = true;
    div![
        C![
            (!prerendered).then(|| C.fade_in),
            C.h_screen,
            C.flex,
            C.flex_col,
            C.font_monospace
        ],
        match model.page {
            Page::Home => page::home::view(&model.home_page_model).into_nodes(),
            Page::NotFound => page::not_found::view().into_nodes(),
        },
        page::partial::header::view().into_nodes(),
    ]
}

#[must_use]
pub fn image_src(image: &str) -> String {
    format!("{}/{}", IMAGES_PATH, image)
}

// ------ ------
//     Start
// ------ ------

#[wasm_bindgen(start)]
pub fn run() {
    set_once();
    log!("Starting app...");
    App::start("app", init, update, view);

    log!("App started.");
}
