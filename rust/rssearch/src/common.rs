use rsfind::consolecolor::{BOLD_RED, CONSOLE_RESET};

// logging
pub fn log(message: &str) {
    println!("{}", message);
}

pub fn log_err(message: &str) {
    log_err_color(message, true)
}

pub fn log_err_color(message: &str, colorize: bool) {
    if colorize {
        eprintln!("{}ERROR: {}{}", BOLD_RED, message, CONSOLE_RESET);
    } else {
        eprintln!("ERROR: {}", message);
    }
}
