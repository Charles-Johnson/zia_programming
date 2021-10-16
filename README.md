# zia_programming

This is the project workspace. Please refer to individual members for details:

- interpreter library: [zia](zia/README.md)
- interactive shell: [izia](izia/README.md)

## Building from Source

### Installing Rust

The Rust compiler is required in order to build from source. Please follow the official [Rust installation instructions](https://www.rust-lang.org/en-US/install.html).

The latest **Stable** version of Rust is required.

If you already have Rust installed, you may need to upgrade to the latest stable:

```
rustup update stable
```

You should also install the following to check whether the CI will pass

   - [cargo-make](https://sagiegurari.github.io/cargo-make/)

     - Check: `$ cargo make -V` => `cargo-make 0.24.1`
     - Install: `$ cargo install --force cargo-make`

   - [nightly rustfmt](https://github.com/rust-lang/rustfmt#on-the-nightly-toolchain)
     - Check: `$ cargo +nightly fmt -- -V` => `rustfmt 1.4.11-nightly (1838235 2019-12-03)`
     - Install:
       1. `$ rustup toolchain install nightly`
       2. `$ rustup component add rustfmt --toolchain nightly`

### Downloading Source Code

The repository can be downloaded using Git:

```
git clone https://github.com/Charles-Johnson/zia_programming.git
```


# Testing

To test all non-ignored tests:
```bash
cargo test
```

To test all tests in the documentation:
```bash
cargo test --doc
```

To test a specific test:
```bash
cargo test specific_test
```

To run a set of integration tests:
```bash
cargo test --test integration_test_filename
```

# Documentation

To generate API documentation:
```bash
cargo doc
```
You can then view `./target/doc/zia/index.html` in a web browser

To generate internal documentation:
```bash
cargo doc --document-private-items
```

# Releasing New Versions
In `./Cargo.toml`:
```toml
[package]
version = x.y.z
```
Then run in the terminal the following
```bash
git commit -a -m "Releasing zia-x.y.z"
git tag -a zia-x.y.z HEAD
cargo package
cargo publish
```

## License

This software is licensed under the General Public License (GPL), version 3 ([LICENSE](LICENSE) http://www.gnu.org/licenses/gpl-3.0.en.html).
