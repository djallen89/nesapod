use find_folder;
use std;
use std::io;
use core;
use core::cpu::{CPU};
use core::ines::INES;
#[cfg(feature = "debug")]
use super::debug::Debug;

#[cfg(feature = "debug")]
pub struct Nesapod {
    pub debugger: Debug,
    core: CPU,
}

#[cfg(feature = "debug")]
impl Nesapod {
    pub fn new(rom: Option<String>, logging: bool) -> Nesapod {
        let romname = match rom {
            Some(r) => r,
            None => format!("assets/instr_test-v5/official_only.nes")
        };
        
        let ines = match INES::new(&romname) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        let core = match CPU::power_up(ines) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        Nesapod {
            debugger: Debug::new(32, logging),
            core: core
        }
    }

    pub fn run(&mut self, n: usize) {
        for s in 0 .. n {
            self.core.exec();
            self.debugger.input(&format!("{}", self.core));
        }
        self.debugger.print();
    }
}

#[cfg(not(feature = "debug"))]
pub struct Nesapod {
    core: CPU
}

#[cfg(not(feature = "debug"))]
impl Nesapod {
    pub fn new(rom: Option<String>, logging: bool) -> Nesapod {
        let romname = match rom {
            Some(r) => r,
            None => format!("assets/instr_test-v5/official_only.nes")
        };

        let ines = match INES::new(&romname) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        let core = match CPU::power_up(ines) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };
        
        Nesapod {
            core: core
        }
    }

    
    pub fn run(&mut self, n: usize) {
        for s in 0 .. n {
            self.core.exec();
        }
    }
}

pub fn main(rom: Option<String>, logging: bool) {
    let mut nesapod = Nesapod::new(rom, logging);
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {},
            Err(e) => {
                println!("error: {}", e);
                #[cfg(feature = "debug")]
                nesapod.debugger.input(&format!("{}", e));
                continue;
            }                
        }
        
        match input.trim() {
            "h" => println!("print help"),
            "q" => break,
            "1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" => {
                let n = input.trim().parse::<usize>().unwrap();
                nesapod.run(n);
            },
            "F" => nesapod.run(16),
            "H" => nesapod.run(100),
            "K" => nesapod.run(1000),
            "T" => nesapod.run(10000),
            "U" => nesapod.run(100000),
            "M" => nesapod.run(1000000),
            _ => {}
        }
    }
}


