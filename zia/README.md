![Maintenance](https://img.shields.io/badge/maintenance-activly--developed-brightgreen.svg)

# zia

[![Crates.io](https://img.shields.io/crates/v/zia.svg)](https://crates.io/crates/zia)

## Interpreter for the Zia programming language

> I gave a talk about this at the Rust Seoul meetup: <https://www.youtube.com/watch?v=LbFTP3pITWU>

The Zia project aims to develop a programming language that can be used to program itself.
You can try out interactive tutorials via the [online editor](https://zia-lang.org) or install
a command line REPL, (`izia`)[<https://github.com/Charles-Johnson/zia_programming/tree/master/izia>].
The commands sent are interpreted based on the `Context`. They are used to incrementally modify, test
and debug the `Context`.

Expressions for Zia commands represent a binary tree where parentheses group a pair of expressions
and a space separates a pair of expressions. For example `"(ll lr) (rl rr)"` represents a perfect
binary tree of height 2 with leaves `"ll"`, `"lr"`, `"rl"`, `"rr"` going from left to right.

The leaves of the tree can be any unicode string without spaces or parentheses. These symbols may
be recognised by the intepreter as concepts or if not used to label new concepts.

So far there are 10 built-in concepts. A new `Context` labels these with the symbols, `label_of`,
`->`, `:=`, `let`, `true`, `false`, `assoc`, `right`, `left`, `precedes`, `>`, `=>` and
`exists_such_that` but the concepts can be renamed (via the `:=` concept, as shown below) to
different symbols for different languages or disciplines.

## Examples

```rust
extern crate zia;
use zia::{multi_threaded::Context, ZiaError};

// Construct a new `Context` using the `new` method
let mut context = Context::new().expect("Context should be created successfully ");

// Specify the rule that the concept "a b" reduces to concept "c"
assert_eq!(context.execute("let a b -> c"), "");
assert_eq!(context.execute("a b"), "c");

// Change the rule so that concept "a b" instead reduces to concept "d"
assert_eq!(context.execute("let a b -> d"), "");
assert_eq!(context.execute("a b"), "d");

// Change the rule so "a b" doesn't reduce any further
assert_eq!(context.execute("let a b -> a b"), "");
assert_eq!(context.execute("a b"), "a b");

// Try to specify a rule that already exists
assert_eq!(context.execute("let a b -> a b"), ZiaError::RedundantReduction{syntax: "a b".to_string()}.to_string());
assert_eq!(context.execute("let a b -> c"), "");
assert_eq!(context.execute("let a b -> c"), ZiaError::RedundantReduction{syntax: "a b".to_string()}.to_string());

// Relabel "label_of" to "표시"
assert_eq!(context.execute("let 표시 := label_of"), "");
assert_eq!(context.execute("표시 a b"), "\'c\'");

// You can reduce a labelled concept
assert_eq!(context.execute("let a -> d"), "");

// Try to specify the composition of a concept in terms of itself
assert_eq!(context.execute("let b := a b"), ZiaError::InfiniteComposition.to_string());

// Try to specify the reduction of concept in terms of itself
assert_eq!(context.execute("let c d -> (c d) e"), ZiaError::ExpandingReduction.to_string());

// Determine the truth of a reduction
assert_eq!(context.execute("a -> d"), "true");
assert_eq!(context.execute("d -> a"), "false");

// A concept never reduces to itself
assert_eq!(context.execute("a -> a"), "false");

// Cannot reduce a reduction expression between unrelated concepts
assert_eq!(context.execute("d -> f"), "d -> f");

// Can ask whether a reduction is true or false
assert_eq!(context.execute("(a -> d) -> true"), "true");
assert_eq!(context.execute("(d -> a) -> false"), "true");

// Let an arbitary symbol be true
assert_eq!(context.execute("let g"), "");
assert_eq!(context.execute("g"), "true");

// Let an arbitary expression be true
assert_eq!(context.execute("let h i j"), "");
assert_eq!(context.execute("h i j"), "true");

// Determine associativity of symbol
assert_eq!(context.execute("assoc a"), "right");

// Define patterns
assert_eq!(context.execute("let _x_ or true -> true"), "");
assert_eq!(context.execute("false or true"), "true");
```

License: GPL-3.0
