//! # The Menhir LR(1) parser generator
//!
//! Menhir is an LR(1) parser generator. It compiles LR(1) grammar specifications
//! down to executable code. It offers a lot of advanced features such as:
//!
//! * Full LR(1) parsing, not just LALR
//! * Parameterized non-terminals
//! * Inlining of grammar productions
//! * Conflict explaining in terms of the grammar
//! * Powerful error reporting
//!
//! This crate is a wrapper that contains a version of Menhir that can produce
//! Rust parsers as well as Rust bindings to make use of the generator easier
//! from a Cargo `build.rs` build script.
//!
//! This is the reference documentation of the wrapper API. It explains how to
//! invoke the generator to generate Rust code and how to link that code with
//! existing Rust programs with a simple complete example. To learn more on how
//! to actually write complex Menhir grammars, look at the [Menhir manual].
//! Writing Rust parsers is the same as writing OCaml parsers, except that
//! semantic actions contain Rust code. To learn more on how to interact with
//! the generated code from the rest of your Rust code, see the documentation of
//! the [menhir_runtime] crate.
//!
//! [Menhir manual]: http://gallium.inria.fr/~fpottier/menhir/manual.pdf
//! [menhir_runtime]: 
//!
//! # How to generate a Rust parser
//!
//! ## A simple Rust parser
//!
//! Below is a simple example of a Menhir parser containing Rust semantic
//! actions and which can be compiled into a Rust parser:
//!
//! ```
//! %token <i32> CONST
//! %token PLUS MINUS TIMES DIV LPAR RPAR EOL
//!
//! %left PLUS MINUS        /* lowest precedence */
//! %left TIMES DIV         /* medium precedence */
//! %nonassoc UMINUS        /* highest precedence */
//!
//! /* Rust's type inference is not as good as OCaml's...
//!  * All non-terminal types must be declared. */
//! %start <i32> main
//! %type  <i32> expr
//!
//! %%
//!
//! main:
//!     | e = expr EOL                  { e }
//!
//! expr:
//!     | i = INT                       { i }
//!     | OP e = expr CL                { e }
//!     | e1 = expr PLUS e2 = expr      { e1 + e2 }
//!     | e1 = expr MINUS e2 = expr     { e1 - e2 }
//!     | e1 = expr TIMES e2 = expr     { e1 * e2 }
//!     | e1 = expr DIV e2 = expr       { e1 / e2 }
//!     | MINUS e = expr %prec UMINUS   { - e }
//! ```
//!
//! This should look familiar to users of Yacc-like parser generators.
//! Otherwise, see the [Menhir manual].
//!
//! ## Building the parser from Cargo
//!
//! Let's assume we wrote the above parser into a `src/calc.rsy` file (Menhir
//! grammars that contain Rust code must be named in `.rsy`).
//!
//! The first step is instructing Cargo that we will need the generator at
//! compile-time using `build-dependencies` in the Cargo.toml file. We will also
//! need to tell Cargo that we will be using a custom build script:
//!
//! ```toml
//! [package]
//! # ...
//! build = "build.rs"
//!
//! [build-dependencies]
//! menhir = "*"
//! ```
//!
//! Then, we write a `build.rs` build script at the root of the Cargo project
//! that uses the `menhir` wrapper crate to run the generator:
//!
//! ```rust
//! extern crate menhir;
//!
//! fn main() {
//!     menhir::process_file("src/calc.rsy", &[]);
//!     menhir::cargo_rustc_flags().unwrap();
//! }
//! ```
//!
//! The first line runs Menhir on the grammar file and produces a Rust file in
//! our package `OUT_DIR`. The second argument to this function is an array of
//! additionnal flags to be passed to Menhir as [`MenhirOption`] values.
//! The second line prints output that Cargo will interpret and that will
//! instruct it where to find the [`menhir_runtime`] crate when compiling the
//! generated code.
//!
//! [`MenhirOption`]: enum.MenhirOption.html
//!
//! ## Using the generated code
//!
//! We can then include the generated code wherever we want in our Rust project,
//! by using the `include!` macro. In our case, the parser file was named
//! `parser.rsy`, so the generated Rust file will be named `parser.rs` by
//! default. The generated code contains a lot of items, so it's recommanded to
//! wrap them in a module to avoid polluting the namespace:
//!
//! ```rust
//! mod parser {
//!     include!(concat!(env!("OUT_DIR"), "/parser.rs"))
//! }
//! ```
//!
//! We can then use the generated code from the `parser` module we just created.
//! For each entry point in the grammar, Menhir generates a type that exposes a
//! `run` function. This is an example of how to use it to run the generated
//! parser, whose entry point non-terminal is called `main`:
//!
//! ```rust
//! use parser::Token::*;
//! fn main() {
//!     let input = vec![INT(1), MINUS, INT(2), PLUS, INT(3)].into_iter().enumerate();
//!     let lexer = menhir_runtime::IteratorLexer::new(input);
//!     match parser::main::run(lexer) {
//!         Ok(value) => println!("successful parsing of {}", value),
//!         Err(_) => println!("syntax error")
//!     }
//! }
//! ```
//!
//! See the documentation of the [`menhir_runtime`] crate for more information
//! on the lexer interface or the way to use the reported error values.
//!
//! # Fast iteration over the grammar
//!
//! When actually developing a Menhir grammar, it can be useful to call directly
//! the Menhir executable in several cases: for example, when trying to solve a
//! conflict in the grammar, one might want to try to recompile the grammar each
//! time it changes, or adding or removing several flags, without having to
//! modify the `build.rs` script and running the whole `cargo build` process
//! again everytime. It is also very useful when working with the new error
//! reporting system, since `--list-errors` can take tens of seconds to
//! complete, and should thus not be invoked automatically from the `build.rs`
//! script but rather manually, when the grammar changes. For this purpose, the
//! wrapper crate exposes the [`link_binary`] function. We call it this way from
//! your build script:
//!
//! ```rust
//! menhir::link_binary().unwrap();
//! ```
//!
//! It will create a symlink (soft link) to the menhir binary in the root of the
//! Cargo project (the directory the `MANIFEST_DIR` environment variable points
//! to). We can then use this symlink just like the Menhir binary. Don't forget
//! the `--rust` and `--no-stdlib` flags, you will probably need them to compile
//! a Rust parser:
//!
//! ```
//! ./menhir --rust --no-stdlib --some-other-flags src/parser.rsy
//! ```
//!
//! [`link_binary`]: fn.link_binary.html

pub use MenhirOption::*;
pub use OnlyPreprocessOption::*;
pub use SuggestOption::*;

use std::fs::File;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// An option (flag) to be passed to the Menhir generator
pub enum MenhirOption {
    /// The base path (without the `.rs` extension) of the output file
    Base(PathBuf),
    /// Construct a canonical Knuth LR(1) automaton
    Canonical,
    /// Compile a `.messages` error file to Rust code
    CompileErrors(PathBuf),
    /// Describe the automaton in `<basename>.automaton`
    Dump,
    /// Explain conflicts in `<basename>.conflicts`
    Explain,
    /// Write grammar's dependency graph to `<basename>.dot`
    Graph,
    /// Construct an LALR(1) automaton
    Lalr,
    /// Log information about the automaton
    LogAutomaton(usize),
    /// Log information about the generated code
    LogCode(usize),
    /// Log information about the grammar
    LogGrammar(usize),
    /// Ignore the `%inline` keyword
    NoInline,
    /// Do not load the standard library
    ///
    /// This is currently the default for the Rust backend when using
    /// [`process_file`] and [`compile_errors`] since the standard library has
    /// not been ported to Rust yet. You may want to use `Stdlib` if you want to
    /// load your own standard library.
    /// [`process_file`]: fn.process_file.html
    /// [`compile_errors`]: fn.compile_errors.html
    NoStdlib,
    /// Only process the given stages and exit ; see [`OnlyPreprocessOption`]
    /// [`OnlyPreprocessOption`]: enum.OnlyPreprocessOption.html
    OnlyPreprocess(OnlyPreprocessOption),
    /// Specify where the standard library lies
    Stdlib(PathBuf),
    /// Warnings about the grammar are errors
    Strict,
    /// Suggest where various files are located ; see [`SuggestOption`]
    /// [`SuggestOption`]: enum.SuggestOption.html
    Suggest(SuggestOption),
    /// Display internal timings
    Timings,
    /// Do not warn that the given token is unused
    UnusedToken(String),
    /// Do not warn about any unused token
    UnusedTokens,
    /// Show version number and exit
    Version,
    /// Synonymous with `Dump` and `Explain`
    Verbose
}

/// Argument to the [`OnlyPreprocess`] flag
/// [`OnlyPreprocess`]: enum.MenhirOption.html
pub enum OnlyPreprocessOption {
    /// Print grammar and exit
    Grammar,
    /// Print grammar with unit actions and exit
    Actions,
    /// Print grammar with unit actions & tokens and exit
    Tokens
}

/// Argument to the [`Suggest`] flag
/// [`Suggest`]: enum.MenhirOption.html
pub enum SuggestOption {
    /// Suggest where is `MenhirLib`.
    /// This is where the [`menhir_runtime`] crate is located.
    /// [`menhir_runtime`]: 
    MenhirLib
}

/// Add a `MenhirOption` to a Menhir command
pub fn add_option<'a>(cmd: &'a mut Command, opt: &MenhirOption) -> &'a mut Command {
    match *opt {
        Base(ref pth)           => cmd.args(&["--base", &format!("{}", pth.display())]),
        Canonical               => cmd.arg("--canonical"),
        CompileErrors(ref pth)  => cmd.args(&["--compile-errors", &format!("{}", pth.display())]),
        Dump                    => cmd.arg("--dump"),
        Explain                 => cmd.arg("--explain"),
        Graph                   => cmd.arg("--graph"),
        Lalr                    => cmd.arg("--lalr"),
        LogAutomaton(ref level) => cmd.args(&["--log-automaton", &format!("{}", level)]),
        LogCode(ref level)      => cmd.args(&["--log-code", &format!("{}", level)]),
        LogGrammar(ref level)   => cmd.args(&["--log-grammar", &format!("{}", level)]),
        NoInline                => cmd.arg("--no-inline"),
        NoStdlib                => cmd.arg("--no-stdlib"),
        OnlyPreprocess(Grammar) => cmd.arg("--only-preprocess"),
        OnlyPreprocess(Actions) => cmd.arg("--only-preprocess-u"),
        OnlyPreprocess(Tokens)  => cmd.arg("--only-preprocess-u"),
        Stdlib(ref pth)         => cmd.args(&["--stdlib", &format!("{}", pth.display())]),
        Strict                  => cmd.arg("--strict"),
        Suggest(MenhirLib)      => cmd.arg("--suggest-menhirLib"),
        Timings                 => cmd.arg("--timings"),
        UnusedToken(ref tok)    => cmd.args(&["--unused-token", tok]),
        UnusedTokens            => cmd.arg("--unused-tokens"),
        Version                 => cmd.arg("--version"),
        Verbose                 => cmd.arg("--canonical"),
    }
}

/// The location of the Menhir binary
pub const MENHIR_BINARY: &'static str = concat!(env!("OUT_DIR"), "/bin/menhir");

/// Run Menhir with the given options and the given grammar file
pub fn run(grammar: Option<&str>, args: &[MenhirOption]) -> Command {
    let mut command = Command::new(MENHIR_BINARY);
    args.iter().fold(&mut command, |acc, opt| add_option(acc, opt));
    grammar.map(|pth| command.arg(pth));
    command
}

/// Convenience function over `run` that just compiles the grammar with the
/// default options
///
/// The default options include `--rust --nostdlib` and generate an output file
/// in the `OUT_DIR` directory of the package that is currently being built.
/// This function should be invoked from a Cargo `build.rs` build script.
pub fn process_file(file: &Path, args: &[MenhirOption]) {
    let out_file = match file.file_stem() {
        Some(stem) => {
            let mut pth = PathBuf::from(match ::std::env::var("OUT_DIR") {
                Ok(dir) => dir,
                Err(_) =>
                    panic!("OUT_DIR is not set. menhir::process_file \
                            should be called from a Cargo build script.")
            });
            pth.push(stem);
            pth
        }
        None => panic!("invalid input file: {}", file.display())
    };
    let mut command = run(Some(&format!("{}", file.display())), args);
    command.arg("--rust");
    // Add --no-stdlib by default since we do not have a Rust sdlib yet.
    add_option(&mut command, &NoStdlib);
    add_option(&mut command, &Base(out_file));
    assert!(command.status().unwrap().success());
}

/// Convenience function over `run` to compiles error files
///
/// Generate an output file in the `OUT_DIR` directory of the package that is
/// currently being built.
/// This function should be invoked from a Cargo `build.rs` build script.
pub fn compile_errors(file: &Path, grammar: &Path, args: &[MenhirOption]) {
    let out_file = match file.file_stem() {
        Some(stem) => {
            let mut pth = PathBuf::from(match ::std::env::var("OUT_DIR") {
                Ok(dir) => dir,

                Err(_) =>
                    panic!("OUT_DIR is not set. menhir::process_file \
                            should be called from a Cargo build script.")
            });
            pth.push(stem);
            pth.set_extension("rs");
            pth
        }
        None => panic!("invalid input file: {}", file.display())
    };
    let mut command = run(Some(&format!("{}", grammar.display())), args);
    command.arg("--rust");
    // Add --no-stdlib by default since we do not have a Rust sdlib yet.
    add_option(&mut command, &NoStdlib);
    add_option(&mut command, &CompileErrors(PathBuf::from(file)));
    let errors = File::create(&format!("{}", out_file.display()))
        .unwrap().into_raw_fd();
    unsafe { command.stdout(Stdio::from_raw_fd(errors)); }
    assert!(command.status().unwrap().success());
}

#[cfg(windows)]
/// Links the Menhir binary to the `MANIFEST` directory of the calling crate
///
/// This is useful when developing a grammar to be able to quickly iterator over
/// changes made to the parser file without having to run the whole `cargo build`
/// process each time or trying new options without having to modify the
/// `build.rs` script.
pub fn link_binary() -> ::std::io::Result<()> {
    let mut out = match ::std::env::var("CARGO_MANIFEST_DIR") {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => panic!("CARGO_MANIFEST_DIR is not set. menhir::link_binary \
                        should be called from a Cargo build script.")
    };
    out.push("menhir");
    if !out.exists() { ::std::os::windows::fs::symlink_file(MENHIR_BINARY, out) }
    else { Ok(()) }
}

#[cfg(not(windows))]
/// Links the Menhir binary to the `MANIFEST` directory of the calling crate
///
/// This is useful when developing a grammar to be able to quickly iterator over
/// changes made to the parser file without having to run the whole `cargo build`
/// process each time or trying new options without having to modify the
/// `build.rs` script.
pub fn link_binary() -> ::std::io::Result<()> {
    let mut out = match ::std::env::var("CARGO_MANIFEST_DIR") {
        Ok(dir) => PathBuf::from(dir),
        Err(_) => panic!("CARGO_MANIFEST_DIR is not set. menhir::link_binary \
                        should be called from a Cargo build script.")
    };
    out.push("menhir");
    if !out.exists() { ::std::os::unix::fs::symlink(MENHIR_BINARY, out) }
    else { Ok(()) }
}

/// Instructs Cargo where to find the menhir_runtime
///
/// This will input a line of the form `cargo:rustc-link-search=...` to the
/// standard output. If this function is run from a `build.rs` script, this will
/// have the effect of making `rustc` search for libraries in that location when
/// compiling the generated code.
pub fn cargo_rustc_flags() -> ::std::io::Result<()> {
    use std::io::Write;
    print!("cargo:rustc-link-search=");
    let stdout = ::std::io::stdout();
    let path = try!(run(None, &[Suggest(MenhirLib)]).output()).stdout;
    try!(stdout.lock().write_all(&path));
    println!("");
    Ok(())
}

#[test]
fn test() {
    assert!(std::str::from_utf8(&run(None, &[Version]).output().unwrap().stdout)
            .unwrap().starts_with("menhir, version"));
}
