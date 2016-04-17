This repository contains a fork of [Menhir](http://gallium.inria.fr/~fpottier/menhir/).
Menhir is an LR(1) parser generator originally designed for the [OCaml]
(https://ocaml.org) language and is able to produce OCaml and Coq code. This
repository extends and modifies Menhir to add it the ability to produce [Rust]
(http://rust-lang.org) code.

This project is still a work-in-progress.
The Rust backend is quite simple for now : it's encoded in tables and run by
a simple LR(1) automaton loop. Most Menhir advanced features such as
error-handling and recording of token positions are not yet implemented.

### Usage

Menhir can be compiled an installed with:

```
make PREFIX=/somewhere
make PREFIX=/somewhere install
```

Omitting `PREFIX` will install Menhir in `/usr/local` (requires root privileges).

To use the generator you must then use a `build.rs` script to instruct Cargo
how to call the generator on your grammar. Let's assume you have a `parser.rsy`
file in the `src` folder. Use a `build.rs` script like this:

```rust
use std::env;
use std::process::{Command, Stdio};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let menhir  = "/path/to/menhir";
    let s = Command::new(menhir).args(&["--rust", "--no-stdlib", "--base"])
                                .arg(&format!("{}/parser", out_dir)).arg("src/parser.rsy")
                                .stdout(Stdio::inherit()).stderr(Stdio::inherit())
                                .status().unwrap();
    assert!(s.success());
    println!("cargo:rerun-if-changed=src/parser.rsy");
}
```

The path to Menhir may vary according to your installation. If you installed
Menhir so that its binary is in PATH, you may use just `menhir`. Note that we
use the `--no-stdlib` option here since the Menhir stdlib has OCaml semantic
actions in it and a Rust replacement has not yet been written.

The `rerun-if-changed` line is there to ensure that the build script is run
every time `parser.rsy` is changed. You may want to omit it since Cargo's
default is to run it every time any file that is not part of the crate is
changed but it's useful if you have in your tree other files that you modify
and you do not want the script to be run when you do it.

Use then the following configuration in your `Cargo.toml` file:

```toml
[package]
# ...
build = "build.rs"

[dependencies]
# ...
menhir_runtime = { path = "/path/to/menhir/src/rust_runtime/" }
```

This time change the path to point to where the Menhir source tree is located.
This is required because the `menhir_runtime` package is not yet available on
crates.io.

### Writing parser generators

Writing parser generators is done pretty much just like normal Menhir parser
with the following expections:
* The type of every single non-terminal must be specified with `%type <Type> symbol`,
  just like when writing Coq parsers, since the Rust type inference is not powerful enough.
* If you use parameterized non-terminals, you must specify types for the instances
  as well with for example `%type <Type> symbol list` for the `list` non-terminal.
  Note that this is an undocumented feature of Menhir and as well is probably not
  really stable.

### The parser interface

A generated parser contain a function for each start non-terminal with the
following signature (with here a `main` non-terminal):

```rust
fn main<Lexer>(lexer: &mut Lexer) -> Result<T, ()>
    where Lexer: Iterator<Item == Token>
```

Where T is the type of the semantic action for `main` as specified in the
grammar description. The second argument to `Result` is unit since error
handling is not yet implemented. Note that his interface is not stable at all
and will very probably change soon when error handling is implemented.

This interface should be compatible with the [RustLex]
(https://github.com/naominitel/RustLex) lexer generator.
