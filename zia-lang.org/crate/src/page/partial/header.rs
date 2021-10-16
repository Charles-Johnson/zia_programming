use crate::{
    generated::css_classes::C,
    image_src, Model, Msg, Page, ScrollHistory,
    Visibility::{self, *},
};
use seed::{a, attrs, class, div, empty, header, id, img, li, prelude::*, ul};

fn header_visibility(
    menu_visibility: Visibility,
    scroll_history: &ScrollHistory,
) -> Visibility {
    let menu_is_visible = menu_visibility == Visible;
    // You can go higher on the mobile phones.
    let at_the_top_or_higher = *scroll_history.back().unwrap_or(&0) <= 0;
    let scrolling_up = scroll_history.front() >= scroll_history.back();

    if menu_is_visible || at_the_top_or_higher || scrolling_up {
        Visible
    } else {
        Hidden
    }
}

#[allow(clippy::too_many_lines)]
pub fn view(model: &Model) -> impl View<Msg> {
    let show_header =
        header_visibility(model.menu_visibility, &model.scroll_history)
            == Visible;
    vec![
        // Header background and line container
        if show_header {
            div![
                class![
                    C.fixed,
                    C.top_0
                    C.inset_x_0,
                    C.h_16,
                    // sm__
                    C.sm__h_24,
                ],
                // Header background
                div![class![C.absolute, C.inset_0, C.opacity_90]],
            ]
        } else {
            empty![]
        },
        // Menu
        if model.menu_visibility == Visible {
            div![
                class![
                    C.fixed,
                    C.w_screen,
                    C.opacity_90,
                    C.h_screen,
                    // md__
                    C.md__hidden,
                ],
                div![
                    class![C.w_56, C.mx_auto, C.flex, C.max_h_full,],
                    ul![
                        class![
                            C.mt_20,
                            C.w_full,
                            C.font_semibold,
                            C.text_primary,
                            C.flex,
                            C.flex_col,
                            C.mb_12,
                            C.overflow_y_auto,
                            // sm__
                            C.sm__mt_24,
                            C.sm__text_21,
                        ],
                        li![
                            class![
                                C.block,
                                C.h_full,
                                C.border_l_4,
                                C.border_r_4,
                                C.border_primary,
                                C.w_full,
                                // sm__
                                C.sm__hidden,
                            ],
                            a![
                                class![
                                    C.pl_8,
                                    C.h_full,
                                    C.flex,
                                    C.items_center,
                                    C.hover__text_primary,
                                    C.outline_none,
                                    C.py_6,
                                ],
                                attrs! {
                                    At::Href => Page::Home.to_href()
                                },
                                simple_ev(Ev::Click, Msg::ScrollToTop),
                                simple_ev(Ev::Click, Msg::HideMenu),
                                "Home"
                            ]
                        ],
                        li![
                            class![C.block, C.h_full, C.w_full,],
                            a![
                                class![
                                    C.pl_8,
                                    C.h_full,
                                    C.flex,
                                    C.items_center,
                                    C.hover__text_primary,
                                    C.py_6,
                                    // sm__
                                    C.sm__py_8,
                                    C.sm__pl_0,
                                    C.sm__justify_center,
                                ],
                                attrs! {
                                    At::Href => "https://github.com/Charles-Johnson/zia_programming"
                                },
                                simple_ev(Ev::Click, Msg::HideMenu),
                                "GitHub",
                                img![
                                    class![
                                        C.inline
                                        C.mb_3,
                                        C.w_3,
                                        // sm__
                                        C.sm__mb_5,
                                        C.sm__ml_px,
                                        C.sm__w_4,
                                    ],
                                    attrs! {
                                        At::Src => image_src("기트헙.svg")
                                    }
                                ]
                            ]
                        ],
                    ],
                ]
            ]
        } else {
            empty![]
        },
        // Header
        if show_header {
            header![
                class![
                    C.fixed,
                    C.top_0
                    C.inset_x_0,
                ],
                // Header controls container
                div![
                    class![
                        C.mx_8
                        C.h_16,
                        C.flex,
                        C.justify_between,
                        C.items_center,
                        // sm__
                        C.sm__h_24,
                    ],
                    // Logo
                    a![
                        attrs! {
                            At::Href => Page::Home.to_href()
                        },
                        simple_ev(Ev::Click, Msg::ScrollToTop),
                        simple_ev(Ev::Click, Msg::HideMenu),
                        img![
                            class![C.h_6, C.sm__h_10, C.sm__w_70px,],
                            attrs! {
                                At::Src => image_src("zia.svg"),
                            }
                        ],
                    ],
                    // Links
                    ul![
                        class![
                            C.hidden,
                            // sm__
                            C.sm___mt_px,
                            C.sm__text_21,
                            C.sm__font_semibold,
                            C.sm__text_primary,
                            C.sm__flex,
                            C.sm__items_center,
                            C.justify_between,
                            C.sm__h_full,
                        ],
                        li![
                            class![
                                // sm__
                                C.sm__block,
                                C.sm__h_full,
                            ],
                            a![
                                class![
                                    // sm__
                                    C.sm__h_full,
                                    C.sm__flex,
                                    C.sm__items_center,
                                    C.sm__hover__text_primary,
                                    C.sm__outline_none,
                                ],
                                attrs! {
                                    At::Href => Page::Home.to_href()
                                },
                                simple_ev(Ev::Click, Msg::ScrollToTop),
                                simple_ev(Ev::Click, Msg::HideMenu),
                                "Home"
                            ]
                        ],
                        li![
                            class![
                                C.hidden,
                                // md__
                                C.md__block,
                                C.md__ml_8,
                                C.md__h_full,
                            ],
                            a![
                                class![
                                    // md__
                                    C.md__h_full,
                                    C.md__flex,
                                    C.md__items_center,
                                    C.md__hover__text_primary,
                                ],
                                attrs! {
                                    At::Href => "https://github.com/Charles-Johnson/zia_programming"
                                },
                                "GitHub",
                                img![
                                    class![
                                        // md__
                                        C.md__inline
                                        C.md__mb_5,
                                        C.md__ml_px,
                                        C.md__w_4,
                                    ],
                                    attrs! {
                                        At::Src => image_src("기트헙.svg")
                                    }
                                ]
                            ]
                        ],
                    ],
                    // Hamburger
                    div![
                        class![
                            C.cursor_pointer => !model.in_prerendering,
                            // md__
                            C.md__hidden,
                        ],
                        simple_ev(Ev::Click, Msg::ToggleMenu),
                        img![
                            id!("hamburger"),
                            class![
                                C.h_8,
                                C.w_12,
                                C.sm__h_10
                                C.sm__w_16,
                            ],
                            if model.in_prerendering {
                                attrs! {
                                    At::Src => image_src("loading.svg")
                                }
                            } else {
                                attrs! {
                                    At::Src => if model.menu_visibility == Visible {
                                        image_src("cross.svg")
                                    } else {
                                        image_src("hamburger.svg")
                                    }
                                }
                            }
                        ]
                    ],
                    // Spacer
                    div![class![
                        C.hidden,
                        // md__
                        C.md__block,
                    ],],
                ],
            ]
        } else {
            empty![]
        },
    ]
}
