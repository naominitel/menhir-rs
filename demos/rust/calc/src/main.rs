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
        INT(5), TIMES, INT(3), MINUS, OP, INT(2),
        PLUS, INT(4), CL, MINUS, INT(2), EOL
    ].into_iter();
    match run!(&mut input) {
        Ok(res) => println!("res = {:?}", res),
        Err(ParserError::SyntaxError(pos)) => panic!("syntax error at {}", pos),
        Err(ParserError::LexerError(err)) => panic!("lexer error: {:?}", err)
    }
    assert!(if let None = input.next() { true } else { false });

    let input = vec![TIMES];
    assert!(if let Err(ParserError::SyntaxError(0)) =
            run!(&mut input.into_iter()) { true } else { false });

    let input: Vec<parser::Token> = vec![];
    assert!(if let Err(ParserError::LexerError(UnexpectedEof(0))) =
            run!(&mut input.into_iter()) { true } else { false });
}

#[test]
fn test() { main() }
