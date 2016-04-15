extern crate env_logger;
mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

fn main() {
    use parser::Token::*;
    let input = vec![INT(5), TIMES, INT(3), MINUS, OP, INT(2), PLUS, INT(4), CL, MINUS, INT(2), EOL];
    env_logger::init().unwrap();
    match parser::main(&mut input.into_iter()) {
        Ok(res) => println!("res = {}", res),
	Err(_)  => println!("syntax error")
    }
}
