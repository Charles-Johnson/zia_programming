extern crate zia;

use zia::Context;

const PRESENTATION: &str = "
# Zia programming language
A programming language to program itself.

# Context {.fragile}

[columns]

[column=0.5]

+ The context determines the semantics of the syntax used in the commands it receives.
\newline
+ Can be modelled as a category of concepts which are related to each other through composition and reduction.

[column=0.5]

![  \newline
    \textcolor{blue}{->} lefthand part \newline
    \textcolor{red}{->} righthand part \newline
    -> reduction
](example.pdf){ height=50% }

[/columns]

# Concepts

Different types of concepts:

+ **Abstract** concepts can reduce to any other concept and be composed of any other concepts.
+ **Concrete** concepts can't reduce to another concept and cannot be composed of any other concepts. In terms of category theory they are terminal objects.
+ Also concepts that refer to data e.g. `String`.

# Concrete Concepts {.fragile}

[columns]

[column=0.5]

ID \newline

0 \newline
1 \newline
2 \newline
3

[column=0.5]

Label \newline

`label_of` \newline
`:=` \newline
`->` \newline
`let`

[/columns]

# Initialisation

```rust
let mut cont = Context::new();
```

![](new.pdf)

# Defining a reduction

```rust
cont.execute(\"let ((a b) (-> c))\");
assert_eq!(context.execute(\"(label_of (a b)) ->\"), \"c\");
```

![](new_reduction.pdf)

# Changing a reduction

```rust
cont.execute(\"let ((a b) (-> d))\");
assert_eq!(context.execute(\"(label_of (a b)) ->\"), \"d\");
```

![Deallocating concepts 19, 20 & 21 doesn't affect semantics.](change_reduction.pdf)

# Removing a reduction

```rust
cont.execute(\"let ((a b) (-> (a b)))\");
assert_eq!(context.execute(\"(label_of (a b)) ->\"), \"a b\");
```

![Deallocating concepts 12-18 & 22-24 doesn't affect semantics](remove_reduction.pdf)

# Defining a composition

```rust
context.execute(\"let (c (:= (a b)))\");
```

![](new_composition.pdf)

# Breaking coherence

The following will return an error message.
```rust
context.execute(\"let ((a b) (-> d))\")
```

![Concept 19 either reduces to concept 20 or 23 depending on evaluation strategy](breaking_coherence.pdf)

# Removing a composition

```rust
context.execute(\"let (c (:= c))\");
```

![Deallocating concepts 12-20 doesn't affect semantics](remove_composition.pdf)

# Infinite composition

The following will return an error message
```rust
context.execute(\"let (b (:= (a b)))\")
```

![](infinite_composition.pdf)

# Relabelling concepts

```rust
context.execute(\"let (\u{D45C}\u{C2DC} (:= label_of))\");
```

![](relabel.pdf)";

fn main() {
    let contents =
        include_str!("../../zia/src/lib.rs").split("//! ```\n").nth(1).unwrap();
    let code_blocks = contents.split("//!\n");
    let mut blocks = Vec::<String>::new();
    let mut commands = Vec::<String>::new();
    for code_block in code_blocks {
        let mut block = String::new();
        block += "```rust\n";
        for line in code_block.split("//! ") {
            block += line;
            line.split("context.execute(\"").nth(1).map(|s| {
                s.split("\")").next().map(|s| commands.push(s.to_string()))
            });
        }
        block += "```\n";
        blocks.push(block);
    }
    let mut context = Context::new();
    for command in commands {
        context.execute(&command);
    }
    println!("{}", PRESENTATION);
}
