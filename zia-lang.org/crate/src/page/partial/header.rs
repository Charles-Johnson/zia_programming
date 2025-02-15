use crate::{
    generated::css_classes::C,
    image_src,
    page::home::{Msg, OUTER_PADDING},
    Page,
};
use seed::{a, attrs, header, img, prelude::*, style, C};

#[allow(clippy::too_many_lines)]
pub fn view() -> impl IntoNodes<crate::Msg> {
    let image_class = C![C.h_10, C.w_10];
    header![
        C![C.flex, C.justify_between, C.items_center, C.h_10],
        style![St::Position => "fixed", St::Top => "0", St::Left => "0", St::Width => format!("calc(100% - calc(2 * {OUTER_PADDING}))"), St::Margin => OUTER_PADDING],
        // Logo
        a![
            attrs! {
                At::Href => Page::Home.to_href()
            },
            img![
                &image_class,
                attrs! {
                    At::Src => image_src("zia.svg"),
                }
            ],
            ev(Ev::Click, |_| crate::Msg::Home(Msg::ToggleMenu)),
        ],
    ]
}
