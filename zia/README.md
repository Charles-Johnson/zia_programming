# `zia`: Interpreter for the Zia programming language.

The Zia project aims to develop a programming language that can be used to program itself. 
Instead of storing the source code as plain text and editing the raw text (which can easily break 
the program), the runtime environment of the interpreter (the `Context`) can be saved to disk and 
used in other programs. All the programming is done using an interactive shell such as
[IZia](https://github.com/Charles-Johnson/zia_programming/tree/master/izia). The commands sent are
interpreted based on the `Context`. They are used to incrementally modify, test and debug the 
`Context`.  

Expressions for Zia commands represent a binary tree where parentheses group a pair of 
expressions and a space separates a pair of expressions.

e.g.
```
(ll lr) (rl rr)
```    
represents the following binary tree:
```
    / \
   /   \
  /     \
 / \   / \
ll lr rl rr
```

The leaves of the tree can be any unicode string without spaces or parentheses. These symbols may 
be recognised by the intepreter as concepts or if not used to label new concepts.

Currently, only the lowest-level functionality has been implemented. It's important that programs
are represented consistently and transparently within the `Context` in order to achieve a 
self-describing system. The syntax shown below may appear awkward but more convenient syntax will 
be possible once more functionality is added. For example, the need to group pairs of expressions 
in parentheses will be alleviated by functionality to set the relative precedence and associativity
of concepts. 

So far there are 4 built-in concepts. A new `Context` labels these with the symbols below but the 
labels can be changed to different symbols (e.g. for different languages or disciplines).  

Let concept, symbol: `let`

Label concept, symbol: `label_of`

Reduction concept, symbol: `->`

Definition concept, symbol: `:=`

# Public API  

The current implementation exposes the `Context` type that can be used in an interface such as 
[IZia](https://github.com/Charles-Johnson/izia). Importing the following traits allows the 
corresponding methods to be called with `Context`.

```rust
trait ContextMaker<T> {
	fn new() -> Self { 
		// Constructs a new Context and labels the 4 built-in concepts.
    }
}
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

To generate documentation for Public API:
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