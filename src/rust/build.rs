use std::env;
use std::process::{Command, Stdio};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    env::remove_var("TARGET");
    env::set_var("USE_OCAMLFIND", "false");
    assert!(Command::new("make").arg(format!("PREFIX={}", out_dir))
                                .stdout(Stdio::inherit()).stderr(Stdio::inherit())
                                .status().unwrap().success());
    assert!(Command::new("make").arg(&format!("PREFIX={}", out_dir)).arg("install")
                                .stdout(Stdio::inherit()).stderr(Stdio::inherit())
                                .status().unwrap().success());
}
