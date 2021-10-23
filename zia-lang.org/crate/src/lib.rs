// @TODO: uncomment once https://github.com/rust-lang/rust/issues/54726 stable
//#![rustfmt::skip::macros(class)]

#![allow(clippy::used_underscore_binding)]
#![allow(clippy::non_ascii_literal)]
#![allow(clippy::enum_glob_use)]

mod generated;
mod page;

use console_error_panic_hook::set_once;
use fixed_vec_deque::FixedVecDeque;
use generated::css_classes::C;
use seed::{class, div, document, log, prelude::*, window};
use Visibility::*;

use page::home;

const TITLE_SUFFIX: &str = "Zia";
// https://mailtolink.me/
const MAIL_TO_CHARLES: &str = "mailto:charlesthomasjohnson0@gmail.com";
const CHARLES_EMAIL: &str = "Contact";
const TWEET_TO_CHARLES: &str = "https://twitter.com/Charles40189535?s=03";

const USER_AGENT_FOR_PRERENDERING: &str = "ReactSnap";
const IMAGES_PATH: &str = "static/images";

// ------ ------
//     Model
// ------ ------

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Visibility {
    Visible,
    Hidden,
}

impl Visibility {
    pub fn toggle(&mut self) {
        *self = match self {
            Visible => Hidden,
            Hidden => Visible,
        }
    }
}

// We need at least 3 last values to detect scroll direction,
// because neighboring ones are sometimes equal.
type ScrollHistory = FixedVecDeque<[i32; 3]>;

pub struct Model {
    pub page: Page,
    pub home_page_model: home::Model,
    pub scroll_history: ScrollHistory,
    pub menu_visibility: Visibility,
    pub in_prerendering: bool,
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
        .stream(streams::window_event(Ev::Scroll, |_| Msg::Scrolled));

    Model {
        page: url.into(),
        home_page_model: home::Model::default(),
        scroll_history: ScrollHistory::new(),
        menu_visibility: Hidden,
        in_prerendering: is_in_prerendering(),
    }
}

fn is_in_prerendering() -> bool {
    let user_agent =
        window().navigator().user_agent().expect("cannot get user agent");

    user_agent == USER_AGENT_FOR_PRERENDERING
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
    ScrollToTop,
    Scrolled(i32),
    ToggleMenu,
    HideMenu,
    Home(home::Msg),
}

pub fn update(msg: Msg, model: &mut Model, orders: &mut impl Orders<Msg>) {
    match msg {
        Msg::UrlChanged(subs::UrlChanged(url)) => {
            model.page = url.into();
        },
        Msg::ScrollToTop => window().scroll_to_with_scroll_to_options(
            web_sys::ScrollToOptions::new().top(0.),
        ),
        Msg::Scrolled(position) => {
            *model.scroll_history.push_back() = position;
        },
        Msg::ToggleMenu => model.menu_visibility.toggle(),
        Msg::HideMenu => {
            model.menu_visibility = Hidden;
        },
        Msg::Home(hm) => home::update(hm, &mut model.home_page_model),
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
        class![
            C.fade_in => !prerendered,
            C.min_h_screen,
            C.flex,
            C.flex_col,
        ],
        match model.page {
            Page::Home => page::home::view(&model.home_page_model).into_nodes(),
            Page::NotFound => page::not_found::view().into_nodes(),
        },
        page::partial::header::view(model).into_nodes(),
        page::partial::footer::view().into_nodes(),
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
