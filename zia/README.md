# `zia`: Interpreter for the Zia programming language.

The Zia project aims to develop a programming language that can be used to program itself. 
Instead of storing the source code as plain text and editing the raw text (which can easily break 
the program), the runtime environment of the interpreter (the `Context`) can be saved to disk and 
used in other programs. All the programming is done using an interactive shell such as
[IZia](https://github.com/Charles-Johnson/zia_programming/tree/master/izia). The commands sent are interpreted based on the `Context`. They are used to 
incrementally modify, test and debug the `Context`.  

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

`let` is used to set a relation as true. It can be further explained in its use below.

Label concept, symbol: `label_of`

`label_of a` is the expression for the label of the concept which is labelled with the symbol `a`.
It can be further explained in its use below.

Reduction concept, symbol: `->`

`->` can be used to specify reduction rules for concepts given by expressions. `let (a (-> b))`
 represents the command to specify the rule that the concept labelled by `a` reduces to the 
concept labelled by `b`.

`->` is also used to express the reduction of a concept. `a ->` expresses the concept `b` because 
of the previous command but `b ->` expresses `b` because no reduction rule exists for `b`. 

Labels of concepts reduce to a command to print the symbol that the concept is labelled with.
`(label_of a) ->` expresses the command to print `a` as output. `(label_of (a ->)) ->` expresses
the command to print `b` as output.

You can modify existing reduction rules. For example you can change the reduction rule for `a` by 
`let (a (-> c))`; `(label_of (a ->)) ->` will now print `c`. You could also execute 
`let (a (-> a))` and so `(label_of (a ->)) ->` now prints `a`.

The intepreter will let you know if reduction rule commands are redundant. `let (a (-> a))` is 
redundant because `a` already reduces to itself. Executing `let (a (-> b))` twice will print an 
error on the second time.

Definition concept, symbol: `:=`

`:=` can be used to label a binary tree of concepts or relabel a concept. For example 
`let (c (:= (a b)))` means graphically:
```
 c
/ \
a b
```
The command `(label_of (c :=)) ->` then prints `a b`. The command `(label_of (a :=)) ->` prints 
`a`. We can change the symbol of `b` to `e` using `let (e (:= b))`. `(label_of (c :=)) ->` would 
then print `a e` and `(label_of (a ->)) ->` would print `e`. Because `a` reduces to `e`, 
`(label_of (c ->)) ->` prints `e e`. If `let (c (-> f))` is executed, an error message is printed 
explaining some of the components of `c` (`a` and `e`) reduce. This would break the coherence 
between the reduction of a concept and its composition's reductions. Should `c` reduce to `f` or 
`e e`? Maintaining this coherence is important for a consistent lazy reduction of syntax trees.

To make sure concepts can be fully reduced, commands like `let (i (:= (i j)))` are not accepted by
the interpreter nor are commands like `let (i (-> (i j)))`. More subtley `let (e (:= (a d)))` is 
not accepted because the reduction of `a d` is `e d` and so `(label_of (e ->)) ->` would print 
`e d`, `(label_of ((e d) ->)) ->` would print `(e d) d` etc. Successive reductions could never 
terminate if this kind of command was accepted. 

# API  

The current implementation exposes the `Context` type that can be used in an interface such as 
[IZia](https://github.com/Charles-Johnson/izia). Importing the following traits allows the 
corresponding methods to be called with `Context`.

```rust
trait ContextMaker<T> {
	fn new() -> Self { 
		// Constructs a new Context and labels the 4 built-in concepts.
    }
}

trait Execute<T> {
    fn execute(&mut self, command: &str) -> String { 
		// Executes the commands given by the user that may modify the `Context` and outputs a 
		// `String` that maybe empty, an error message or the answer to a query.
	}
}
```
