use std::env;
use std::process::{Command, Stdio};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let menhir  = "../../../src/_stage1/menhir.native";
    let s = Command::new(menhir).args(&["--rust", "-v", "--no-stdlib", "--base"])
                                .arg(&format!("{}/parser", out_dir)).arg("src/parser.rsy")
                                .stdout(Stdio::inherit()).stderr(Stdio::inherit())
                                .status().unwrap();
    assert!(s.success());
    println!("cargo:rerun-if-changed=src/parser.rsy");
    println!("cargo:rerun-if-changed={}", menhir);
}
