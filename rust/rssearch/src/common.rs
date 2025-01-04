// logging
pub fn log(message: &str) {
    println!("{}", message);
}

pub fn log_err(message: &str) {
    eprintln!("ERROR: {}", message);
}
