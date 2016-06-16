pub use MenhirOption::*;
pub use OnlyPreprocessOption::*;
pub use SuggestOption::*;

use std::fs::File;
use std::os::unix::io::{FromRawFd, IntoRawFd};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

pub enum MenhirOption {
    Base(PathBuf),
    Canonical,
    CompileErrors(PathBuf),
    Dump,
    Explain,
    Graph,
    Lalr,
    LogAutomaton(usize),
    LogCode(usize),
    LogGrammar(usize),
    NoInline,
    NoStdlib,
    OnlyPreprocess(OnlyPreprocessOption),
    Stdlib(PathBuf),
    Strict,
    Suggest(SuggestOption),
    Timings,
    UnusedToken(String),
    UnusedTokens,
    Version,
    Verbose
}

pub enum OnlyPreprocessOption {
    Grammar,
    Actions,
    Tokens
}

pub enum SuggestOption {
    MenhirLib
}

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

/// The location of the Menhir binary.
pub const MENHIR_BINARY: &'static str = concat!(env!("OUT_DIR"), "/bin/menhir");

/// Runs Menhir with the given options and the given grammar file.
pub fn run(grammar: Option<&str>, args: &[MenhirOption]) -> Command {
    let mut command = Command::new(MENHIR_BINARY);
    args.iter().fold(&mut command, |acc, opt| add_option(acc, opt));
    grammar.map(|pth| command.arg(pth));
    command
}

/// Convenience function over `run` that just compiles the grammar with the default options.
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
/// Links the Menhir binary to the MANIFEST directory of the calling crate.
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

/// Links the Menhir binary to the MANIFEST directory of the calling crate.
#[cfg(not(windows))]
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
