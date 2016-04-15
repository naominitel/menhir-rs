extern crate env_logger;
mod parser { include!(concat!(env!("OUT_DIR"), "/parser.rs")); }

fn main() {
    use parser::Token::*;
    let input = vec![
        OP, LAMBDA, ID(format!("x")), DOT, ID(format!("x")), CL,
        OP, LAMBDA, ID(format!("x")), DOT, LAMBDA, ID(format!("y")), DOT,
            ID(format!("x")), OP, LAMBDA, ID(format!("x")), DOT,
                                  ID(format!("x")), ID(format!("y")), CL,
        CL,
        EOF
    ];
    env_logger::init().unwrap();
    match parser::main(&mut input.into_iter()) {
        Ok(res) => println!("res = {:?}", res),
        Err(_)  => println!("syntax error")
    }
}
