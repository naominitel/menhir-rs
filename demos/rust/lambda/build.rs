extern crate menhir;

fn main() {
    menhir::process_file(::std::path::Path::new("src/parser.rsy"), &[]);
    menhir::cargo_rustc_flags().unwrap();
}
