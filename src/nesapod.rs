use std::io;
use core::cpu::CPU;
use core::ines::INES;
#[cfg(feature = "debug")]
use super::debug::Debug;

#[cfg(feature = "debug")]
pub struct Nesapod {
    debugger: Debug,
    cpu: CPU,
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

        let cpu = match CPU::power_up(ines) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        Nesapod {
            debugger: Debug::new(32, logging),
            cpu: cpu,
        }
    }

    pub fn run(&mut self, n: usize) {
        for s in 0 .. n {
            self.cpu.exec();
            self.debugger.input(&format!("{}", self.cpu));
        }
        self.debugger.print();
    }
}

#[cfg(not(feature = "debug"))]
pub struct Nesapod {
    cpu: CPU,
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

        let cpu = match CPU::power_up(ines) {
            Ok(r) => r,
            Err(f) => panic!(f)
        };

        Nesapod {
            cpu: cpu,
        }
    }

    
    pub fn run(&mut self, n: usize) {
        for s in 0 .. n {
            self.cpu.exec();
        }
    }
}

pub fn main(rom: Option<String>, logging: bool) {
    let mut nesapod = Nesapod::new(rom, logging);
    let help = "n @ 1 ... 9 => run n times
H => 16, H => 100, K => 1000, T => 10,000,
U => 100,000, => M = 1,000,000, G => 100,000,000
q => quit
h => print this message";
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
            "h" => println!("{}", help),
            "d" => println!("{}", nesapod.cpu.dump_ram()),
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
            "G" => nesapod.run(100000000),
            _ => {}
        }
    }
}


