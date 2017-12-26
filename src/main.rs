#[macro_use]
extern crate conrod;
extern crate getopts;
#[macro_use]
extern crate bitflags;

use getopts::Options;
use std::env;

mod core;
mod nesapod;
pub mod tests;

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();
    let mut opts = Options::new();
    opts.optopt("r", "rom", "select a rom", ".nes"); 
    opts.optopt("l", "log", "log messages to a file", "FILE");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string())
    };
    
    if matches.opt_present("h") {
        print!("{}", opts.usage(&format!("Usage: {} [options]", program)));
        return;
    }

    let log = if matches.opt_present("l") {
        matches.opt_str("l")
    } else {
        None
    };

    let rom = if matches.opt_present("r") {
        matches.opt_str("r")
    } else {
        None
    };

    nesapod::main(log, rom)
}

