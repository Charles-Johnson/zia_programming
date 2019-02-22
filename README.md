# zia_programming

This is the project workspace. Please refer to individual members for details:

- interpreter library: [zia](zia/README.md)
- interactive shell: [izia](izia/README.md)
- testing library: [test_zia](test_zia/lib.rs)

## Building from Source

### Installing Rust

The Rust compiler is required in order to build from source. Please follow the official [Rust installation instructions](https://www.rust-lang.org/en-US/install.html).

The latest **Stable** version of Rust is required.

If you already have Rust installed, you may need to upgrade to the latest stable:

```
rustup update stable
```

### Downloading Source Code

The repository can be downloaded using Git:

```
git clone https://github.com/Charles-Johnson/zia_programming.git
```

### Building The Libraries

To build all members in debug mode, simply use the command

```
cargo build
```

To run tests:

```
cargo test
```

To optimise runtime performance at the cost of compilation time, append `--release` to the above commands.

## License

This software is licensed under the General Public License (GPL), version 3 ([LICENSE](LICENSE) http://www.gnu.org/licenses/gpl-3.0.en.html).
