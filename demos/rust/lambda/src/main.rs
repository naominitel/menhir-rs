extern crate menhir_runtime;
mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

use menhir_runtime::*;

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
        Err(ParserError::SyntaxError(pos)) => panic!("syntax error at {}", pos),
        Err(ParserError::LexerError(err)) => panic!("lexer error: {:?}", err)
    }
    assert!(if let None = input.next() { true } else { false });

    let input = vec![CL];
    assert!(if let Err(ParserError::SyntaxError(0)) =
            run!(&mut input.into_iter()) { true } else { false });

    let input: Vec<parser::Token> = vec![];
    assert!(if let Err(ParserError::LexerError(UnexpectedEof(0))) =
            run!(&mut input.into_iter()) { true } else { false });
}

#[test]
fn test() { main() }
