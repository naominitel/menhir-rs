extern crate menhir_runtime;
mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

fn main() {
    use parser::Token::*;
    let mut input = vec![
        INT(5), TIMES, INT(3), MINUS, OP, INT(2),
        PLUS, INT(4), CL, MINUS, INT(2), EOL
    ].into_iter();
    match parser::main(&mut menhir_runtime::IteratorLexer::new(&mut input)) {
        Ok(res) => println!("res = {}", res),
        Err(menhir_runtime::ParserError::SyntaxError)  => panic!("syntax error"),
        Err(menhir_runtime::ParserError::LexerError(err))  => panic!("lexer error: {:?}", err)
    }
    assert!(if let None = input.next() { true } else { false });
}

#[test]
fn test() { main() }
