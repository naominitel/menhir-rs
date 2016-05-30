extern crate menhir_runtime;
mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

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
    match parser::main(&mut menhir_runtime::IteratorLexer::new(&mut input)) {
        Ok(res) => println!("res = {:?}", res),
        Err(menhir_runtime::ParserError::SyntaxError) => panic!("syntax error"),
        Err(menhir_runtime::ParserError::LexerError(err)) => panic!("lexer error: {:?}", err)
    }
    assert!(if let None = input.next() { true } else { false });
}

#[test]
fn test() { main() }
