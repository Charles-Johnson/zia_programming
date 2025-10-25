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

## 2. Test local changes in a browser

1. Open terminal in your project and go to directory `crate` - `$ cd crate`
1. Install Webpack and other dependencies - `$ yarn`
1. Try to start dev-server - `$ yarn start` - and then open [localhost:8000](http://localhost:8000) in a browser.
1. Stop server (try `Ctrl+c`).
1. Try to lint any code changes - `$ cargo make --no-workspace verify` - you shouldn't see any errors.

## 4. Write your website

1. Open project in your favorite IDE. I use [IntelliJ](https://www.jetbrains.com/idea/download) or [VS Code](https://code.visualstudio.com/).
1. Run `$ yarn start` in terminal. I use `bash` integrated into IDE (it's installed with Git on Windows).
   - _Note_: Yarn commands can be run from project root or from directory `crate`, but Cargo commands can be run **only from `crate`**.
1. Open [localhost:8000](http://localhost:8000) in a browser.
1. You can open it also in your mobile phone:
   1. Make sure your dev-server aka computer is in the same network as your phone.
   1. Find out your local IP address. Use e.g. this online tool: https://www.whatismybrowser.com/detect/what-is-my-local-ip-address.
   1. Open URL with found IP address and default port (e.g. `192.168.1.5:8000`) on your phone.

_Note_: You don't have to follow all steps below - reuse starter project code as you need.

### Header

1. Open `/crate/src/page/partial/header.rs` in your IDE.
1. Delete function `header_visibility`.
1. Write a new body for function `view`.

### Footer

1. Open `/crate/src/page/partial/footer.rs` in your IDE.
1. Write a new body for function `view`.

### 404

1. Open `/crate/src/page/not_found.rs` in your IDE.
1. Write a new body for function `view`.

### Home page

1. Open `/crate/src/page/home.rs` in your IDE.
1. Write a new body for function `view`.

### About page

1. Open `/crate/src/page/about.rs` in your IDE.
1. Write a new body for function `view`.

### App core

1. Open `/crate/src/lib.rs` in your IDE.
1. Change `TITLE_SUFFIX` value.
1. Delete `MAIL_TO_KAVIK` and `MAIL_TO_HELLWEB`.
1. Write a new body for function `view`.

### Favicons

1. Delete or replace files in `/favicons`.
1. Open `/entries/templates/favicons.hbs` in your IDE.
1. Delete content or update it.
   - _Note_: Templates are written in [Handlebars](https://handlebarsjs.com/).

### Loading page

1. Open `/entries/templates/loading_page.hbs` in your IDE.
1. Delete content or update it.

### Social media & basic settings

1. Open `/entries/templates/social_media.hbs` in your IDE.
1. Delete content or update it.

### Basic HTML

1. Open `/entries/index.hbs` in your IDE.
1. Update content.

### Fonts

1. Delete or replace files and directories in `/static/fonts`.
1. Open `/css/fonts.css` in your IDE.
1. Delete content or update it.

### Images & other files

1. Delete or replace files in `/static/images`.
1. Delete `/static/Martin_Kavik_resume.pdf`.

### TailwindCSS

1. Tailwind config is defined [here](configs/tailwind.config.js).
1. All possible tailwind classes can be imported into the Rust code using `use generated::css_classes::C;`

### Custom CSS

1. Custom CSS is defined [here](css/custom.css).
1. It can be referenced in the handlebars templates e.g [entries/templates/loading_page.hbs](entries/templates/loading_page.hbs)

## 5. Prepare your project for deploy

How to format, lint and test your project.

And how to setup Github Actions with deploy into Netlify.

### Formatter & Linter

1. Format: `$ cargo make fmt` (it overwrites files) or only `$ cargo make fmt_check`
1. You can modify format settings in:
   - `/crate/rustfmt.toml`
   - `/crate/Makefile.toml` - tasks `fmt` and `fmt_check`
1. Lint: `$ cargo make clippy`
1. You can modify linter settings in:
   - `/crate/Makefile.toml` - task `clippy`

### Testing

1. Run `$ cargo make test_h firefox` for headless testing in Firefox.
   - There are more similar commands - see `/crate/Makefile.toml`
   - _Note_: There is only one test in this project (`crate/tests/test.rs`), see [seed-rs-realworld](https://github.com/seed-rs/seed-rs-realworld) for more examples.
1. If you want to test prerendered website:
   1. `$ yarn build:prerender`
   1. `$ serve:dist`
   1. Open [localhost:8000](http://localhost:8000) in a browser.
   1. _Tip_: Always test it also in production environment because e.g. routing is a little bit different among servers.
1. **Always run `$ cargo make verify`** before push to make sure CI pipeline will accept your code.
   - It'll format your code, lint it and start headless tests in Firefox.
   - You can change its behaviour in `/crate/Makefile.tom` - task `verify` (similar task `verify_only` is used in CI).

# Contributing

- Improved documentation, fixed typos, updated dependencies, ... - create Issue or PR.
- Ideas, bugs, questions, ... - create Issue.
