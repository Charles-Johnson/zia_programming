# zia-lang.org web app

[![Netlify Status](https://api.netlify.com/api/v1/badges/9ab27d1a-b3f4-4253-8621-8689b21a8c92/deploy-status)](https://app.netlify.com/projects/thirsty-minsky-e69f74/deploys)

<!-- prettier-ignore -->
> Try using Zia in a web browser!

---

[**LIVE DEMO**: zia-lang.org](https://zia-lang.org)

Main components:

- **Seed** - Rust framework, inspired by Elm. [Seed's Awesome list](https://github.com/seed-rs/awesome-seed-rs).
- **[Tailwind CSS](https://tailwindcss.com/)** - CSS framework. All CSS classes in your project are typed for safe use in Rust code. Unused classes are automatically deleted for much smaller bundle size.
- **[Webpack](https://webpack.js.org/)** - Bundler. Auto-reload on code change, dev-server accessible from mobile phones, prerendering for static websites... and many more useful features are prepared for you in this quickstart.
  - Why Webpack instead of Rust-only build pipeline? - [Wiki](https://github.com/seed-rs/seed-quickstart-webpack/wiki/Why-Webpack)
- **Production-ready configuration** - Project is linted, compiled, prerendered and deployed in CI pipeline (see `.github/workflows/main.yml` and `netlify.toml`). Linters are very strict.

---

# Setup

## 1. Install / check required tools

> You can use docker compose to automatically install and run tooling

1. Make sure you have basic tools installed:

   - [Yarn](https://yarnpkg.com/lang/en/docs/install) - run `$ yarn -v` in terminal. It should output something like `1.22.22`
   - [Node.js](https://nodejs.org) - `$ node -v` => `v18.20.8`
   - [Rust](https://www.rust-lang.org/tools/install) - `$ rustc -V` => `rustc 1.81.0 (eeb90cda1 2024-09-04)`
   - Rust target `wasm` - `$ rustup target list` => `.. wasm32-unknown-unknown (installed) ..`
     - Install: `$ rustup target add wasm32-unknown-unknown`

1. Platform-specific tools like `ssl` and `pkg-config`:

   - Follow recommendations in build errors (during the next chapter).
   - _Note_: Don't hesitate to write a tutorial and create PR or write a Wiki page for your platform.

1. These tools are required by some commands:

   - [wasm-pack](https://rustwasm.github.io/wasm-pack/)

     - Check: `$ wasm-pack -V` => `wasm-pack 0.13.0`
     - Install: `$ cargo install --force wasm-pack --version 0.13.0`

   - [cargo-make](https://sagiegurari.github.io/cargo-make/)

     - Check: `$ cargo make -V` => `cargo-make 0.35.0`
     - Install: `$ cargo install --force cargo-make --version 0.35.0`

   - [nightly rustfmt](https://github.com/rust-lang/rustfmt#on-the-nightly-toolchain)
     - Install:
       1. `$ rustup toolchain install nightly-2024-07-07`
       2. `$ rustup component add rustfmt --toolchain nightly-2024-07-07`

## 2. Test changes

1. Open project in your favorite IDE. I use neovim which you can run with `docker compose run --rm nvim`
1. Run `$ yarn start` or `docker compose run --rm web`
1. Open [localhost:8000](http://localhost:8000) in a browser.
1. You can open it also in your mobile phone:
   1. Make sure your dev-server aka computer is in the same network as your phone.
   1. Find out your local IP address. Use `hostname -I` on linux or this online tool: https://www.whatismybrowser.com/detect/what-is-my-local-ip-address.
   1. Open URL with found IP address and default port (e.g. `192.168.1.5:8000`) on your phone.
1. Stop server (try `Ctrl+c`).
1. Try to lint any code changes - `$ cargo make --no-workspace verify` - you shouldn't see any errors.
> `docker compose run --rm verify` also works

### TailwindCSS

1. Tailwind config is defined [here](configs/tailwind.config.js).
1. All possible tailwind classes can be imported into the Rust code using `use generated::css_classes::C;`

### Custom CSS

1. Custom CSS is defined [here](css/custom.css).
1. It can be referenced in the handlebars templates e.g [entries/templates/loading_page.hbs](entries/templates/loading_page.hbs)


### Prerendering

1. If you want to test prerendering the website:
   1. `$ yarn build:prerender` (or `docker compose up web-release`)
   1. Open [localhost:8000](http://localhost:8000) in a browser.

> Currently prerendering causes issues with tutorials which rely on the text-area element reference being set which doesn't happen when prerendering the website

# Contributing

- Improved documentation, fixed typos, updated dependencies, ... - create Issue or PR.
- Ideas, bugs, questions, ... - create Issue.
