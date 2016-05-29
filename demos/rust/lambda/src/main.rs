extern crate env_logger;
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
    env_logger::init().unwrap();
    match parser::main(&mut input) {
        Ok(res) => println!("res = {:?}", res),
        Err(_)  => panic!("syntax error")
    }
    assert!(if let None = input.next() { true } else { false });
}

#[test]
fn test() { main() }
