# zia_programming

This is the project workspace. Please refer to individual members for details:

- web interface with tutorials: [zia-lang.org](zia-lang.org/README.md)
- interpreter library: [zia](zia/README.md)
- interactive shell: [izia](izia/README.md)

## Building from Source

### Installing Rust

The Rust compiler is required in order to build from source. Please follow the official [Rust installation instructions](https://www.rust-lang.org/en-US/install.html).

The rustc v1.81 is required.

```
rustup default 1.81
```

You should also install the following to check whether the CI will pass

- [cargo-make](https://sagiegurari.github.io/cargo-make/)

  - Check: `$ cargo make -V` => `cargo-make 0.35.1`
  - Install: `$ cargo install --force cargo-make --version "0.35.1"`

- [nightly rustfmt](https://github.com/rust-lang/rustfmt#on-the-nightly-toolchain)
  - Install:
    1.  `$ rustup toolchain install nightly-2024-07-07
    2.  `$ rustup component add rustfmt --toolchain nightly-2024-07-07

### Downloading Source Code

The repository can be downloaded using Git:

```
git clone https://github.com/Charles-Johnson/zia_programming.git
```

# Testing

To test all non-ignored tests:

```bash
cargo +nightly test
```

To test all tests in the documentation:

```bash
cargo test --doc
```

To test a specific test:

```bash
cargo +nightly test specific_test
```

To run a set of integration tests:

```bash
cargo +nightly test --test integration_test_filename
```

## License

This software is licensed under the General Public License (GPL), version 3 ([LICENSE](LICENSE) http://www.gnu.org/licenses/gpl-3.0.en.html).
