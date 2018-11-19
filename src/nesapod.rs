use std::io;
use core::NESCore;
use core::ines::INES;

struct Nesapod {
    core: NESCore
}

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

        let core = NESCore::power_up(ines);

        Nesapod {
            core: core,
        }
    }

    pub fn run(&mut self, n: usize) {
        for _ in 0 .. n {
            self.core.step()
        }
    }

    pub fn check_ram(&self) -> String {
        self.core.check_ram()
    }

    pub fn print_stack(&mut self) {
        self.core.print_stack();
    }

    pub fn print_cpu(&self) {
        self.core.print_cpu()
    }

    pub fn reset(&mut self) {
        self.core.reset()
    }
}

pub fn main(rom: Option<String>, logging: bool) {
    let mut nesapod = Nesapod::new(rom, logging);
    nesapod.reset();
    let help =
        "n @ 1 ... 9 => run n times
d => Print status of debug rom
p => Print registers
S => Print stack
F => 16, H => 100, K => 1000, T => 10,000,
U => 100,000, M = 1,000,000, G => 100,000,000
q => Quit
h => Print this message";
    println!("{}", help);
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
            "d" => println!("{}", nesapod.check_ram()),
            "p" => nesapod.print_cpu(),
            "r" => nesapod.reset(),
            "S" => nesapod.print_stack(),
            "q" => break,
            "1" |
            "2" |
            "3" |
            "4" |
            "5" |
            "6" |
            "7" |
            "8" |
            "9" => {
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

