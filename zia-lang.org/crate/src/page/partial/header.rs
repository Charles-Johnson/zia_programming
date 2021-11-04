use crate::{generated::css_classes::C, image_src, Msg, Page};
use seed::{a, attrs, header, img, prelude::*, C};

#[allow(clippy::too_many_lines)]
pub fn view() -> impl IntoNodes<Msg> {
    header![
        C![
            C.mx_8
            C.flex,
            C.justify_between,
            C.items_center,
            C.h_24,
            C.p_4
        ],
        // Logo
        a![
            attrs! {
                At::Href => Page::Home.to_href()
            },
            img![
                C![C.h_10, C.w_70px,],
                attrs! {
                    At::Src => image_src("zia.svg"),
                }
            ],
        ],
        // GitHub link
        a![
            attrs! {
                At::Href => "https://github.com/Charles-Johnson/zia_programming"
            },
            img![
                C![C.h_10, C.w_70px],
                attrs! {
                    At::Src => image_src("기트헙.svg")
                },
            ]
        ]
    ]
}
