extern crate menhir_runtime;
mod parser {
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
    include!(concat!(env!("OUT_DIR"), "/errors.rs"));
}

use menhir_runtime::*;
use menhir_runtime::lexing::*;

macro_rules! run(
    ($tokens:expr) => {
        parser::main::run(IteratorLexer::new(&mut $tokens.enumerate()))
    }
);

fn main() {
    use parser::Token::*;

    let mut input = vec![
        OP, LAMBDA, ID(format!("x")), DOT, ID(format!("x")), CL,
        OP, LAMBDA, ID(format!("x")), DOT, LAMBDA, ID(format!("y")), DOT,
           ID(format!("x")), OP, LAMBDA, ID(format!("x")), DOT,
                                  ID(format!("x")), ID(format!("y")), CL,
        CL,
        EOF
    ].into_iter();
    match run!(&mut input) {
        Ok(res) => println!("res = {:?}", res),
        Err(Error::SyntaxError(err)) =>
            panic!("syntax error at {}: {}",
                   err.location(),
                   err.as_str().unwrap_or("")),
        Err(Error::LexerError(err)) => panic!("lexer error: {:?}", err)
    }
    assert!(if let None = input.next() { true } else { false });

    let input = vec![CL];
    assert!(match run!(&mut input.into_iter()) {
        Err(Error::SyntaxError(err)) =>
           err.location() == &0 &&
           err.as_str() == Some("Expected the beginning of an expression.\n"),
        _ => false
    });

    let input: Vec<parser::Token> = vec![];
    assert!(match run!(&mut input.into_iter()) {
        Err(Error::LexerError(UnexpectedEof(0))) => true,
        _ => false
    });
}

#[test]
fn test() { main() }
