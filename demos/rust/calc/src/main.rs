mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

fn main() {
    use parser::Token::*;
    let mut input = vec![
        INT(5), TIMES, INT(3), MINUS, OP, INT(2),
        PLUS, INT(4), CL, MINUS, INT(2), EOL
    ].into_iter();
    match parser::main(&mut input) {
        Ok(res) => println!("res = {}", res),
        Err(_)  => panic!("syntax error")
    }
    assert!(if let None = input.next() { true } else { false });
}

#[test]
fn test() { main() }
