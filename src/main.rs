#![crate_type= "bin"]

extern crate time;
extern crate getopts;
extern crate find_folder;
extern crate pancurses;

extern crate sdl2;
#[macro_use] extern crate bitflags;

use getopts::Options;
use std::env;

pub mod debug;
pub mod core;
pub mod nesapod;
pub mod tests;

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();
    let mut opts = Options::new();
    opts.optopt("r", "rom", "select a rom", ".nes");
    opts.optflag("d", "debug", "debug CPU");
    opts.optflag("l", "log", "log messages to a file");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string())
    };
    
    if matches.opt_present("h") {
        print!("{}", opts.usage(&format!("Usage: {} [options]", program)));
        return;
    }

    let log = matches.opt_present("l");
    let debug = matches.opt_present("d");
    let rom = if matches.opt_present("r") {
        matches.opt_str("r")
    } else {
        None
    };

    nesapod::main(rom, debug, log)
}

