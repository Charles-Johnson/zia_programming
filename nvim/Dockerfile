FROM node:18-slim
RUN apt update
RUN apt install -y curl
RUN apt-get install ripgrep
USER node
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- --default-toolchain=1.81 -y
ENV PATH="/home/node/.cargo/bin:$PATH"
RUN rustup component add rust-analyzer
RUN mkdir -p /home/node/.local/share/nvim/site/autoload
RUN curl -o /home/node/.local/share/nvim/site/autoload/plug.vim \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
RUN mkdir -p /home/node/.local/share/nvim/site/pack/tpope/start
WORKDIR /home/node/.local/share/nvim/site/pack/tpope/start
USER root
RUN apt install -y git
USER node
RUN git clone https://tpope.io/vim/fugitive.git
RUN mkdir -p /home/node/.config/nvim
COPY init.vim /home/node/.config/nvim/init.vim
USER root
RUN apt update && apt install -y neovim
USER node
RUN nvim -c "helptags fugitive/doc" -c ":PlugInstall | :q | :q"
RUN git config --global --add safe.directory /usr/src
# install coc.nvim extensions
RUN mkdir -p "/home/node/.config/coc/extensions"
WORKDIR "/home/node/.config/coc/extensions"
RUN npm install \
  coc-rust-analyzer \
  coc-json \
  coc-prettier \
  --install-strategy=shallow --ignore-scripts --no-bin-links --no-package-lock --omit=dev
ENV NPM_CONFIG_PREFIX=/home/node/.npm-global
ENV PATH=$PATH:/home/node/.npm-global/bin
USER node
RUN mkdir -p /home/node/.config/nvim/plugin
COPY init.lua /home/node/.config/nvim/plugin
RUN rustc -V
RUN rustup target add wasm32-unknown-unknown
USER root
RUN apt install -y build-essential
RUN apt install -y rsync
USER node
RUN cargo install --force wasm-pack --version 0.13.0
RUN cargo install --force cargo-make --version "0.35.1"
RUN rustup toolchain install nightly-2024-07-07
RUN rustup component add rustfmt --toolchain nightly-2024-07-07
RUN cargo install --force cargo-readme
RUN git config --global user.email "charlesthomasjohnson0@gmail.com"
RUN git config --global user.name "Charle Johnson"

