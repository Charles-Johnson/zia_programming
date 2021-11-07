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
    orders
        .subscribe(Msg::UrlChanged)
        .after_next_render(|_| Msg::FocusOnCommandInput);

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
    FocusOnCommandInput,
}

pub fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::UrlChanged(subs::UrlChanged(url)) => {
            model.page = url.into();
        },
        Msg::Home(hm) => home::update(hm, &mut model.home_page_model, orders),
        Msg::FocusOnCommandInput => {
            model.home_page_model.focus_on_command_input();
        },
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
        page::partial::header::view().into_nodes(),
        match model.page {
            Page::Home => page::home::view(&model.home_page_model).into_nodes(),
            Page::NotFound => page::not_found::view().into_nodes(),
        },
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

#[cfg(test)]
mod test {
    extern crate wasm_bindgen_test;
    use super::run;
    use seed::window;
    use wasm_bindgen_test::*;

    wasm_bindgen_test_configure!(run_in_browser);

    fn load_page() {
        // Need to insert an element with id="app" to mimic the loading screen
        // defined in ../../entries/index.hbs
        let document = window().document().unwrap();
        let section = document.create_element("section").unwrap();
        section.set_id("app");
        document.body().unwrap().append_with_node_1(&section).unwrap();

        // Now we can run the WASM part
        run();
    }

    #[wasm_bindgen_test]
    fn keyboard_input_is_displayed_textarea_element() {
        load_page()
        // TODO: simulate keyboard input

        // TODO: assert that text is displayed in textarea element
    }

    #[wasm_bindgen_test]
    fn submitted_command_moves_from_textarea_to_history() {
        load_page()
        // TODO: focus textarea

        // TODO: simulate typing text

        // TODO: simulate pressing "Enter"

        // TODO: assert textarea is empty

        // TODO: assert text submitted appears in an element above textarea
    }

    #[wasm_bindgen_test]
    fn height_of_textarea_does_not_change_when_typing_one_line() {
        load_page()
        // TODO: get the height of the textarea element

        // TODO: simulate typing text

        // TODO: get the height of the textarea element

        // TODO: assert height of textarea element has not changed
    }
}
