use conrod;
use conrod::backend::glium::glium;
use conrod::backend::glium::glium::texture::{Texture2d, RawImage2d, ClientFormat};
use conrod::backend::glium::glium::Surface;
use conrod::image::Map;
use std::{i8, u8, u16, fmt, iter};
use std::io;
use core::ppu::{IMAGE_SIZE, PPU};
use core::ines::INES;
use core::debug::Debug;
use core::addressing::{OPCODE_TABLE, Address, AddressType, SingleType, DoubleType};

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const FRAME_CYCLES: isize = 29781;
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const NMI_VECTOR: u16 = 0xFFFA;
pub const IRQ_VECTOR: u16 = 0xFFFE;
pub const STACK_REGION: u16 = 0x0100;
pub const A: usize = 0;
pub const X: usize = 1;
pub const Y: usize = 2;

#[derive(Debug)]
pub enum EmuError {
    IllegalInstruction,
    IOError(io::Error),
    BadROM(String),
}

pub type CPUResult<T> = Result<T, EmuError>;

bitflags! {
    struct StatusFlags: u8 {
        const C = 0b0000_0001;
        const Z = 0b0000_0010;
        const I = 0b0000_0100;
        const D = 0b0000_1000;
        const B = 0b0001_0000;
        const S = 0b0010_0000;
        const V = 0b0100_0000;
        const N = 0b1000_0000;
    }
}

impl StatusFlags {
    pub fn get_flag(&self, flag: StatusFlags) -> StatusFlags {
        *self & flag
    }

    pub fn flag_position(flag: StatusFlags) -> u8 {
        match flag {
            StatusFlags::C => 0,
            StatusFlags::Z => 1,
            StatusFlags::I => 2,
            StatusFlags::D => 3,
            StatusFlags::B => 4,
            StatusFlags::S => 5,
            StatusFlags::V => 6,
            StatusFlags::N => 7,
            _ => panic!(format!("Flag position of {:?} doesn't make sense!", flag))
        }
    }

    pub fn get_flag_bit(&self, flag: StatusFlags) -> u8 {
        (*self & flag).bits
    }
    
    pub fn status(&self, flag: StatusFlags) -> bool {
        self.get_flag(flag) == flag
    }
}

#[inline(always)]
fn split(u: u16) -> (u8, u8) {
    let lower = (u & 0xFF) as u8;
    let upper = (u >> 8) as u8;
    (lower, upper)
}

#[inline(always)]
fn combine(lower: u8, upper: u8) -> u16 {
    ((upper as u16) << 8) + (lower as u16)
}

#[inline(always)]
pub fn counter_inc(x: u16, y: u8) -> isize {
    //x + y > c  | x + y < c
    //c < x + y  | c > x + y
    //c - x < y  | c - x > y
    if u8::MAX - (((x & 0xFF) >> 8) as u8) > y {
        0
    } else {
        1
    }        
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Code {
    LDA, LDX, LDY, LAX,
    STA, STX, STY, ASX,
    ADC, SBC, _SB,
    INC, INX, INY, INS,
    DEC, DEX, DEY, DCM,
    ASL, LSR, ALR,
    ROL, ROR, 
    AND, ORA, EOR, 
    CMP, CPX, CPY, 
    BIT, 
    BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS, 
    TAX, TXA, TAY, TYA, TSX, TXS, 
    PHA, PLA, PHP, PLP,
    JMP, JSR, RTS, RTI,
    SEC, SED, SEI,
    CLC, CLD, CLI, CLV, 
    NOP, _NP, BRK,
    SLO, RLA, LSE, RRA,
    ARR, XAA, OAL, SAX,
    TAS, SAY, XAS, AXA,
    ILL
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let code = format!("{:?}", self);
        match *self {
            Code::LDA | Code::LDX | Code::LDY | Code::STA | Code::STX | Code::STY |
            Code::ADC | Code::SBC | Code::INC | Code::INX | Code::INY | Code::DEC | Code::DEX | Code::DEY |
            Code::ASL | Code::LSR | Code::ROL | Code::ROR | Code::AND | Code::EOR | Code::ORA |
            Code::CMP | Code::CPX | Code::CPY | Code::BIT |
            Code::BCC | Code::BCS | Code::BEQ | Code::BNE | Code::BPL | Code::BVC | Code::BVS | Code::BMI |
            Code::TAX | Code::TXA | Code::TAY | Code::TYA | Code::TSX | Code::TXS |
            Code::PHA | Code::PLA | Code::PHP | Code::PLP | Code::JMP | Code::JSR | Code::RTS | Code::RTI |
            Code::SEC | Code::SED | Code::SEI | Code::CLC | Code::CLD | Code::CLI | Code::CLV |
            Code::NOP | Code::BRK | Code::ILL => write!(f, " {}", code),
            Code::LAX | Code::SAX | Code::ALR | Code::SLO | 
            Code::RLA | Code::RRA | Code::ARR | Code::XAA | Code::OAL | Code::TAS | Code::SAY |
            Code::XAS | Code::AXA => write!(f, "*{}", code),
            Code::_SB => write!(f, "*SBC"),
            Code::_NP => write!(f, "*NOP"),
            Code::ASX => write!(f, "*SAX"),
            Code::DCM => write!(f, "*DCP"),
            Code::INS => write!(f, "*ISB"),
            Code::LSE => write!(f, "*SRE"),
        }
    }
}

pub struct CPU {
    counter: isize,
    pc: u16,
    stack_pointer: u8,
    axy: Vec<u8>,
    aio_registers: Vec<u8>,
    status_register: StatusFlags,
    ram: [u8; 2048],
    pub ppu: PPU,
    nmi: bool,
    irq: bool,
    cartridge: INES,
    last_read_bytes: String,
    last_instr: String,
    last_registers: String,
    debug: bool
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let spaces1: String = if self.last_read_bytes.len() >= 15 {
            format!("")
        } else {
            iter::repeat(' ').take(15 - self.last_read_bytes.len()).collect()
        };
        let spaces2: String = if self.last_instr.len() >= 33 {
            format!("")
        } else {
            iter::repeat(' ').take(33 - self.last_instr.len()).collect()
        };
        write!(f, "{}{}{}{}", self.last_read_bytes, spaces1, self.last_instr, spaces2)?;
        write!(f, "{}", self.last_registers)
    }
}

impl CPU {
    pub fn toggle_debug(&mut self, d: bool) {
        self.debug = d
    }

    #[inline(always)]
    pub fn run_ppu(&mut self, s: isize) -> bool {
        for _ in 0 .. 341 {
            self.ppu.step(&mut self.cartridge);
        }
        for _ in 1 .. s {
            for _ in 0 .. 341 {
                self.ppu.step(&mut self.cartridge);
            }
        }
            
        if self.ppu.signal_nmi() {
            self.nmi = true;
            self.ppu.clear_nmi_signal();
        }

        if self.ppu.signal_new_frame() {
            self.ppu.clear_frame_signal();
            true
        } else {
            false
        }
    }

    pub fn run_scanline(&mut self, s: isize) -> bool {
        if self.nmi {
            self.handle_nmi();
        } else if self.irq && !self.status_register.status(StatusFlags::I) {
            self.handle_irq()
        }
        
        while self.counter > (s * 341) / 3 {
            self.step();
        }

        self.run_ppu(341)
    }

    pub fn run_frame<'a>(&mut self, image_map: &mut Map<Texture2d>,
                         game_screen: &conrod::image::Id,
                         display: &glium::Display) {
        self.counter += FRAME_CYCLES;
        for s in 0 .. 262 {
            if self.counter <= 0 {
                break
            }
            let new_frame = self.run_scanline(s);
            if new_frame {
                let screen: Vec<u32> = self.print_screen().to_vec();            
                let mut real_screen = RawImage2d::from_raw_rgb_reversed(&screen, (256, 240));
                real_screen.format = ClientFormat::U32;
                let texture = Texture2d::new(display, real_screen).unwrap();

                if let Some(t) = image_map.get_mut(*game_screen) {
                    *t = texture;
                } else {
                    panic!("BAD IMG ID: This should be impossible.")
                }
            }
        }
    }

    pub fn print_screen(&self) -> &[u32; IMAGE_SIZE] {
        self.ppu.image()
    }

    pub fn signal_new_frame(&self) -> bool {
        self.ppu.signal_new_frame()
    }
    
    pub fn power_up(ines: INES, debug: bool) -> CPUResult<CPU> {
        Ok(CPU {
            counter: 0,
            pc: RESET_VECTOR,
            stack_pointer: POWERUP_S,
            axy: vec![0, 0, 0],
            aio_registers: vec![0; 32],
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 2048],
            ppu: PPU::init(ines.mirroring()),
            nmi: false,
            irq: false,
            cartridge: ines,
            last_read_bytes: format!(""),
            last_instr: format!(""),
            last_registers: if debug {
                format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                        0, 0, 0, (StatusFlags::I | StatusFlags::S).bits, POWERUP_S)
            } else {
                format!("")
            },
            debug: debug
        })
    }

    pub fn reset(&mut self) {
        self.stack_pointer -= 3;
        self.pc = RESET_VECTOR;
        if self.debug {
            self.last_read_bytes = format!("{:04X}  {:02X} ", RESET_VECTOR, 0x4C);
            self.last_instr = format!(" JMP ");
        }
        self.status_register = self.status_register | StatusFlags::I;
        let (jmp, absolute, cycles) = OPCODE_TABLE[0x4C];
        self.execute(jmp, absolute, cycles)
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
    }

    pub fn dump_ram(&self) -> String {
        self.cartridge.dump_ram()
    }

    fn handle_nmi(&mut self) {
        let pc = self.pc + 1;
        self.stack_push_double(pc);
        let flags = self.status_register.bits;
        self.stack_push(flags);
        self.status_register |= StatusFlags::I;
        self.pc = NMI_VECTOR;
        self.nmi = false;
    }

    fn handle_irq(&mut self) {
        let pc = self.pc + 1;
        self.stack_push_double(pc);
        let flags = self.status_register.bits;
        self.status_register |= StatusFlags::I;
        self.stack_push(flags);
        self.pc = IRQ_VECTOR;
    }

    fn elapsed(&self) -> isize {
        FRAME_CYCLES - self.counter
    }

    pub fn step(&mut self) {
        //println!("{}", self);
        let addr = self.pc;
        let opcode = self.read(addr);
        let (code, address, cycles) = OPCODE_TABLE[opcode as usize];

        if self.debug { 
            self.last_instr = format!("");
            let last_registers = format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                         self.axy[A], self.axy[X],
                                         self.axy[Y], self.status_register.bits,
                                         self.stack_pointer);
            self.last_registers = last_registers;
            self.last_read_bytes = format!("{:04X}  {:02X} ", addr, opcode);
            self.last_instr = format!("{} ", code);
        }

        let val = self.pc;
        self.pc = val.wrapping_add(1);
        self.execute(code, address, cycles)
    }

    #[inline(always)]
    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000 ... 0x1FFF => self.ram[(address % 2048) as usize],
            0x2000 ... 0x3FFF => self.ppu.read(address, &mut self.cartridge),
            0x4014            => self.ppu.oam_dma_read(),
            0x4000 ... 0x401F => self.aio_registers[(address - 0x4000) as usize],
            0x4020 ... 0xFFFF | _ => self.cartridge.read(address),
        }
    }
    
    #[inline(always)]
    pub fn read_two_bytes(&mut self, addr: u16) -> u16 {
        let lower = self.read(addr);
        let upper = self.read(addr + 1);
        combine(lower, upper)
    }

    #[inline(always)]
    pub fn write(&mut self, address: u16, val: u8) {
        match address as usize {
            0x0000 ... 0x1FFF => {
                let pre = self.ram[(address % 2048) as usize];
                if self.debug {
                    self.last_instr.push_str(&format!("= {:02X}", pre));
                }
                self.ram[(address % 2048) as usize] = val;
            },
            0x2000 ... 0x3FFF => {
                let pre = self.ppu.read(address, &mut self.cartridge);
                if self.debug {
                    self.last_instr.push_str(&format!("= {:02X}", pre));
                }
                self.ppu.write(address, val, &mut self.cartridge);
            },
            0x4014 => {
                //read from val*0x0100 ... val*0x0100 + 255
                let page = (val as u16) << 8;
                for b in page .. page + 256 {
                    let val = self.read(b);
                    self.ppu.oam_dma_write(val);
                }
                self.counter -= 514;
            }
            0x4000 ... 0x4013 | 0x4015 ... 0x401F => {
                let pre = self.aio_registers[(address - 0x4000) as usize];
                if self.debug { 
                    self.last_instr.push_str(&format!("= {:02X}", pre));
                }
                self.aio_registers[(address - 0x4000) as usize] = val;
            },
            0x4020 ... 0xFFFF | _ => {
                let pre = self.cartridge.read(address);
                if self.debug {
                    self.last_instr.push_str(&format!("= {:02X}", pre));
                }
                self.cartridge.write(address, val)
            },
        }
    }

    #[inline(always)]
    fn decode_single_byte(&mut self, s: SingleType) -> (isize, u16) {
        let pc = self.pc;
        let addr = self.read(pc);
        if self.debug {
            self.last_read_bytes.push_str(&format!("{:02X} ", addr));
        }
        match s {
            SingleType::ZeroPg => {
                if self.debug { 
                    self.last_instr.push_str(&format!("${:02X} ", addr));
                }
                (0, addr as u16)
            },
            SingleType::ZeroPgX => {
                let real_addr = self.axy[X].wrapping_add(addr) as u16;
                if self.debug { 
                    self.last_instr.push_str(&format!("${:02X},X @ {:02X} ", addr, real_addr));
                }
                (0, real_addr)
            },
            SingleType::ZeroPgY => {
                let real_addr = self.axy[Y].wrapping_add(addr) as u16;
                if self.debug { 
                    self.last_instr.push_str(&format!("${:02X},Y @ {:02X} ", addr, real_addr));
                }
                (0, real_addr)
            },
            SingleType::IndirectX => {
                let lsb = self.axy[X].wrapping_add(addr);
                let real_addr_low = self.ram[(lsb as usize)];
                let real_addr_high = self.ram[(lsb.wrapping_add(1) as usize)];
                let real_addr = combine(real_addr_low, real_addr_high);
                if self.debug { 
                    self.last_instr.push_str(&format!("(${:02X},X) @ {:02X} = {:04X} ", addr,
                                                      addr.wrapping_add(self.axy[X]),
                                                      real_addr));
                }
                (0, real_addr)
            },
            SingleType::IndirectY => {
                let low = self.ram[addr as usize];
                let high = self.ram[(addr as u8).wrapping_add(1) as usize];
                let pre_addr = combine(low, high);
                let extra = counter_inc(pre_addr, self.axy[Y]);
                let real_addr = (self.axy[Y] as u16).wrapping_add(pre_addr);
                if self.debug { 
                    self.last_instr.push_str(&format!("(${:02X}),Y = {:04X} @ {:04X} ",
                                                      addr, pre_addr, real_addr));
                }
                (extra, real_addr)
            },
            _ => panic!(format!("{:?} is not supported in decode_singlebyte", s))
        }
    }

    #[inline(always)]
    fn decode_double_byte(&mut self, d: DoubleType) -> (isize, u16) {
        let pc = self.pc;
        let addr = self.read_two_bytes(pc);
        if self.debug {
            self.last_read_bytes.push_str(&format!("{:02X} {:02X} ", addr & 0xFF, addr >> 8));
        }
        match d {
            DoubleType::Absolute => {
                if self.debug {
                    self.last_instr.push_str(&format!("${:04X} ", addr));
                }
                (0, addr)
            },
            DoubleType::AbsoluteX => {
                let real_addr = (self.axy[X] as u16) + addr;
                let extra = counter_inc(addr, self.axy[Y]);
                if self.debug {
                    self.last_instr.push_str(&format!("${:04X},X @ {:04X} ", addr, real_addr));
                }
                (extra, real_addr)
            },
            DoubleType::AbsoluteY => {
                let extra = counter_inc(addr, self.axy[X]);
                let real_addr = (self.axy[Y] as u16).wrapping_add(addr);
                if self.debug {
                    self.last_instr.push_str(&format!("${:04X},Y @ {:04X} ", addr, real_addr));
                }
                (extra, real_addr)
            },
            DoubleType::Indirect => panic!("No indirect in decode double byte!")
        }
    }

    /// Returns the number of bytes of an instruction, if it takes an extra cycle by
    /// crossing a page and the value stored at that location (excepting immediate and
    /// relative instructions).
    #[inline(always)]
    fn address_read(&mut self, a: Address, implied: Option<usize>) -> (u16, isize, u8) {
        let pc = self.pc;
        match a {
            Address::Invalid => panic!("Improper use of address_read: Invalid"),
            Address::Implied => if let Some(idx) = implied {
                (0, 0, self.axy[idx])
            } else {
                panic!("Improper use of address read: None Implied")
            },                
            Address::Acc => (0, 0, self.axy[A]),
            Address::Specified(AddressType::SingleByte(SingleType::Relative)) => {
                let val = self.read(pc);
                if self.debug { 
                    self.last_read_bytes.push_str(&format!("{:02X} ", val));
                }
                (1, 0, val)
            },
            Address::Specified(AddressType::SingleByte(SingleType::Immediate)) => {
                let val = self.read(pc);
                if self.debug {
                    self.last_read_bytes.push_str(&format!("{:02X} ", val));
                    self.last_instr.push_str(&format!("#${:02X} ", val));
                }
                (1, 0, val)
            },
            Address::Specified(AddressType::SingleByte(s)) => {
                let (extra_cycles, addr) = self.decode_single_byte(s);
                let val = self.read(addr);
                (1, extra_cycles, val)
            },
            Address::Specified(AddressType::DoubleByte(d)) => {
                let (extra_cycles, addr) = self.decode_double_byte(d);
                let val = self.read(addr);
                (2, extra_cycles, val)
            }
        }
    }
    
    #[inline(always)]
    fn address_rmw(&mut self, a: Address, min_cycles: isize, implied: Option<usize>, op: &Fn(&mut CPU, u8) -> u8) {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;

        self.counter -= min_cycles;

        match a {
            Invalid | 
            Specified(DoubleByte(Indirect)) |
            Specified(SingleByte(Relative)) |
            Specified(SingleByte(Immediate)) => panic!("Don't use read modify write for {:?}", a),
            Implied => if let Some(idx) = implied {
                let val = self.axy[idx];
                let result = op(self, val);
                self.axy[idx] = result;
            } else {
                panic!("Improper use of rmw for None Implied")
            },
            Acc => {
                let val = self.axy[A];
                let result = op(self, val);
                self.axy[A] = result;
            },
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s);
                let val = self.read(addr);
                let result = op(self, val);
                self.write(addr, result);
                self.pc += 1;
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d);
                let val = self.read(addr);
                let result = op(self, val);
                self.write(addr, result);
                self.pc += 2;
            }
        }
    }

    #[inline(always)]
    fn address_write(&mut self, a: Address, min_cycles: isize, val: u8, implied: Option<usize>) {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;

        let bytes = match a {
            Acc |
            Invalid | 
            Specified(SingleByte(Immediate)) |
            Specified(SingleByte(Relative)) |
            Specified(DoubleByte(Indirect)) => panic!("Improper use of address_write!"),
            Implied => if let Some(idx) = implied {
                self.axy[idx] = val;
                0
            } else {
                panic!("Improper use of adress write for None Implied")
            },
            Specified(SingleByte(s)) => {
                let (_extra_cyles, addr) = self.decode_single_byte(s);
                self.write(addr, val);
                1
            },
            Specified(DoubleByte(d)) => {
                let (_extra_cycles, addr) = self.decode_double_byte(d);
                self.write(addr, val);
                2
            },
        };
        self.modify_pc_counter(bytes, min_cycles);
    }
    
    #[inline(always)]
    fn execute(&mut self, c: Code, a: Address, min_cycles: isize) {
        if c == Code::ILL {
            panic!(EmuError::IllegalInstruction)
        }
        
        match c {
            //~12% of instructions are LDA ZPG (12%) ; (19%) ~7% from LDA {ABX,ZPX,ABS,IYI,IMD} 
            Code::LDA => self.load(a, min_cycles, A),
            //~10% of instructions are BNE     (22%) ; (29%)
            Code::BCC => self.branch(a, min_cycles, StatusFlags::C, false),
            //~07% of instructions are JMP ABS (29%) ; (36.5%)
            Code::JMP => {                                                  
                let pc = self.pc;
                let addr = self.read_two_bytes(pc);
                if self.debug {
                    self.last_read_bytes.push_str(&format!("{:02X} {:02X}",
                                                           addr & 0xFF, addr >> 8));
                }
                match a {
                    Address::Specified(AddressType::DoubleByte(DoubleType::Indirect)) => {
                        let low = self.read(addr);
                        let next = if addr & 0xFF == 0xFF {
                            addr & 0xFF00
                        } else {
                            addr.wrapping_add(1)
                        };
                        let high = self.read(next);
                        let real_addr = combine(low, high);
                        if self.debug {
                            self.last_instr.push_str(&format!("(${:04X}) = {:04X}",
                                                              addr, real_addr));
                        }
                        self.counter -= min_cycles;
                        self.pc = real_addr;
                    },
                    Address::Specified(AddressType::DoubleByte(DoubleType::Absolute)) => {
                        if self.debug { 
                            self.last_instr.push_str(&format!("${:04X} ", addr));
                        }
                        self.counter -= min_cycles;
                        self.pc = addr;
                    },
                    _ => panic!(format!("JMP doesn't use {:?}", a))
                }
            },
            //~07% of instructions are INX      (36.5%) ; (43.5%)
            Code::INX => self.inc(a, min_cycles, Some(X)),
            //4.46% of instructions are BPL     (41%) ; (48%)
            Code::BPL => self.branch(a, min_cycles, StatusFlags::N, false),
            //3.82% of instructions are CMP IMD (45%) ; (52%)
            Code::CMP => self.cmp(a, min_cycles, A),
            //3.49% of instructions are BMI     (48%) ; (55.5%)
            Code::BMI => self.branch(a, min_cycles, StatusFlags::N, true),
            //3.32% of instructions are BEQ     (51.5%);(59%)
            Code::BEQ => self.branch(a, min_cycles, StatusFlags::Z, true),
            //3.32% of instructions are BIT ZPG (55%) ; (62%)
            Code::BIT => {
                let (bytes, extra_cycles, val) = self.address_read(a, None);
                if self.debug {
                    self.last_instr.push_str(&format!("= {:02X}", val));
                }
                let zero = self.axy[A] & val == 0;               
                if (val & 0b1000_0000) == 0b1000_0000 {
                    self.set_flag_op(StatusFlags::N)
                } else {
                    self.clear_flag_op(StatusFlags::N)
                }
                if (val & 0b0100_0000) == 0b0100_0000 {
                    self.set_flag_op(StatusFlags::V)
                } else {
                    self.clear_flag_op(StatusFlags::V)
                }
                if zero {
                    self.set_flag_op(StatusFlags::Z)
                } else {
                    self.clear_flag_op(StatusFlags::Z)
                }
                self.modify_pc_counter(bytes, min_cycles + extra_cycles)
            },
            //2.94% of instructions are STA ZPG (58%) ; (65%) + STA ABX + STA ABS = 65 + 1.27 + 1.24
            Code::STA => {
                let val = self.axy[A];
                self.address_write(a, min_cycles, val, None)
            },
            //2.00% of instructions are DEX     (60%) ; (67%)
            Code::DEX => self.dec(a, min_cycles, Some(X)),
            //1.98% of instructions are INY     (62%) ; (69%)
            Code::INY => self.inc(a, min_cycles, Some(Y)),
            //1.77% of instructions are TAY     (63.5%);(71%)
            Code::TAY => {
                let val = self.axy[A];
                self.axy[Y] = val;
                self.set_zn(val);
                self.counter -= min_cycles;
            },
            //1.74% of instructions are INC ZPG (65%); (72.5%)
            Code::INC => self.inc(a, min_cycles, None),
            //1.74% of instructions are BCS     (67%) ; (74%) ; (76.7%)
            Code::BCS => self.branch(a, min_cycles, StatusFlags::C, true),
            //1.41% of instructions are JSR ABS (78.18%)
            Code::JSR => {
                if a == Address::Specified(AddressType::DoubleByte(DoubleType::Absolute)) {
                    let pc = self.pc;
                    let addr = self.read_two_bytes(pc);
                    if self.debug {
                        self.last_read_bytes.push_str(&format!("{:02X} {:02X}",
                                                               addr & 0xFF, addr >> 8));
                        self.last_instr.push_str(&format!("${:04X} ", addr));
                    }
                    self.counter -= min_cycles;
                    self.stack_push_double(pc + 1);
                    self.pc = addr;
                } else {
                    panic!(format!("JSR doesn't use {:?}", a))
                }
            },
            //1.38% LSR A (79.56%)
            Code::LSR => self.address_rmw(a, min_cycles, Some(A), &|cpu, x| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = lsr(x);
                cpu.set_czn(result, carry);
                result
            }),
            //1.37% RTS (80.93%)
            Code::RTS => {
                let addr = self.stack_pop_double();
                self.pc = addr + 1;
                self.counter -= min_cycles;
            },
            //1.32% AND IMD (82.25%)
            Code::AND => self.bitwise(a, min_cycles, &|acc, x| { acc & x }),
            //1.08% CLC (83.33%)
            Code::CLC => self.clear_flag(min_cycles, StatusFlags::C),
            
            Code::EOR => self.bitwise(a, min_cycles, &|acc, x| { acc ^ x }),
            Code::ORA => self.bitwise(a, min_cycles, &|acc, x| { acc | x }),

            Code::CPX => self.cmp(a, min_cycles, X),
            Code::CPY => self.cmp(a, min_cycles, Y),

            Code::BNE => self.branch(a, min_cycles, StatusFlags::Z, false),
            Code::BVC => self.branch(a, min_cycles, StatusFlags::V, false),
            Code::BVS => self.branch(a, min_cycles, StatusFlags::V, true),
            
            Code::LDX => self.load(a, min_cycles, X),
            Code::LDY => self.load(a, min_cycles, Y),

            Code::STX => {
                let val = self.axy[X];
                self.address_write(a, min_cycles, val, None)
            },
            Code::STY => {
                let val = self.axy[Y];
                self.address_write(a, min_cycles, val, None)
            },

            Code::ADC => self.add(a, min_cycles),
            Code::SBC => self.sub(a, min_cycles),
            Code::DEC => self.dec(a, min_cycles, None),
            Code::DEY => self.dec(a, min_cycles, Some(Y)),

            Code::ASL => self.address_rmw(a, min_cycles, Some(A), &|cpu, x| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = asl(x);
                cpu.set_czn(result, carry);
                result
            }),
            Code::ROL => self.address_rmw(a, min_cycles, Some(A), &|cpu, x| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }

                let (result, next_carry) = cpu.rol(x);
                cpu.set_czn(result, next_carry);
                result
            }),
            Code::ROR => self.address_rmw(a, min_cycles, Some(A), &|cpu, x| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, next_carry) = cpu.ror(x);
                cpu.set_czn(result, next_carry);
                result
            }),

            Code::TAX => {
                let val = self.axy[A];
                self.axy[X] = val;
                self.set_zn(val);
                self.counter -= min_cycles;
            },
            Code::TXA => {
                let val = self.axy[X];
                self.axy[A] = val;
                self.set_zn(val);
                self.counter -= min_cycles;
            },
            Code::TYA => {
                let val = self.axy[Y];
                self.axy[A] = val;
                self.set_zn(val);
                self.counter -= min_cycles;
            },
            Code::TSX => {
                self.axy[X] = self.stack_pointer;
                let val = self.axy[X];
                self.set_zn(val);
            },
            Code::TXS => {
                let val = self.axy[X];
                self.stack_pointer = val;
                self.counter -= min_cycles;
            },
            

            Code::PHA => {
                self.counter -= min_cycles;
                let val = self.axy[A];
                self.stack_push(val);
            },
            Code::PLA => {
                self.counter -= min_cycles;
                let acc = self.stack_pop();
                self.set_zn(acc);
                self.axy[A] = acc;
            },
            Code::PHP => {
                self.counter -= min_cycles;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits);
            },

            Code::PLP => {
                self.counter -= min_cycles;
                let flags = self.stack_pop() & 0b1110_1111;
                self.status_register.bits = flags | 0b0010_0000;
            },
            
            Code::RTI => {
                let flags = self.stack_pop() & 0b1110_1111;
                self.status_register.bits = flags | 0b0010_0000;
                let addr = self.stack_pop_double();
                self.pc = addr;
                self.counter -= min_cycles;
            },

            
            Code::SEC => self.set_flag(min_cycles, StatusFlags::C),
            Code::SED => self.set_flag(min_cycles, StatusFlags::D),
            Code::SEI => self.set_flag(min_cycles, StatusFlags::I),

          
            Code::CLD => self.clear_flag(min_cycles, StatusFlags::D),
            Code::CLI => self.clear_flag(min_cycles, StatusFlags::I),
            Code::CLV => self.clear_flag(min_cycles, StatusFlags::V),

            Code::BRK => {
                self.counter -= min_cycles;
                let addr = self.pc + 1;
                self.stack_push_double(addr);
                
                let irq = self.read_two_bytes(IRQ_VECTOR);
                self.pc = irq;
                    
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },

            Code::NOP | Code::_NP  => { 
                if a != Address::Implied {
                    let (bytes, extra_cycle, val) = self.address_read(a, None);

                    if self.debug &&
                        a != Address::Specified(AddressType::SingleByte(SingleType::Immediate))
                    {
                        self.last_instr.push_str(&format!("= {:02X}", val));
                    }
                    
                    self.modify_pc_counter(bytes, min_cycles + extra_cycle);
                } else {
                    self.modify_pc_counter(0, min_cycles);
                }
            },

            Code::LAX => {
                let (bytes, extra_cycles, val) = self.address_read(a, None);
                if self.debug && 
                    a != Address::Specified(AddressType::SingleByte(SingleType::Immediate))
                {
                    self.last_instr.push_str(&format!("= {:02X}", val));
                }
                
                self.set_zn(val);
                self.modify_pc_counter(bytes, min_cycles + extra_cycles);
                self.axy[A] = val;
                self.axy[X] = val;
            },


            Code::OAL => {
                self.bitwise(a, min_cycles, &|acc, x| { (acc | 0xEE) & x });
                let a = self.axy[A];
                self.axy[X] = a;
            },                        


            Code::XAA => {
                let (bytes, extra_cycles, val) = self.address_read(a, None);
                let result = self.axy[A] & val;
                self.axy[A] = result;
                self.set_zn(result);
                self.modify_pc_counter(bytes, min_cycles + extra_cycles);
            },


            Code::ASX => {
                let val = self.axy[A] & self.axy[X];
                self.address_write(a, min_cycles, val, None)
            },
            Code::SAX => {
                let (bytes, _, rhs) = self.address_read(a, None);
                let lhs = self.axy[A] & self.axy[X];
                let carry = lhs >= rhs;
                let result = lhs.wrapping_sub(rhs);
                self.set_czn(result, carry);
                self.axy[X] = result;
                self.modify_pc_counter(bytes, min_cycles);
            },
            
            Code::_SB => self.sub(a, min_cycles),
            

            Code::INS => self.address_rmw(a, min_cycles, None, &|cpu, val| {
                let result = val.wrapping_add(1);
                let (final_result, next_carry, overflow) = cpu.adc(!result);
                cpu.set_cznv(final_result, next_carry, overflow);
                cpu.axy[A] = final_result;
                result //Set M to the result of the INC
            }),
            
            Code::DCM => self.address_rmw(a, min_cycles, None, &|cpu, val| {
                let result = val.wrapping_sub(1);
                let lhs = cpu.axy[A];
                let final_result = lhs.wrapping_sub(result);
                let carry = lhs >= result;
                cpu.set_czn(final_result, carry);
                result
            }),            


            Code::SLO => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = asl(val);
                cpu.set_czn(result, carry);
                
                cpu.bitwise_helper(result, &|x, y| { x | y });
                result
            }),
            Code::RLA => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                if cpu.debug && a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, next_carry) = cpu.rol(val);
                cpu.set_czn(result, next_carry);
                
                cpu.bitwise_helper(result, &|x, y| { x & y });
                result
            }),
            Code::LSE => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                let (result, carry) = lsr(val);
                cpu.set_czn(result, carry);
                
                cpu.bitwise_helper(result, &|x, y| { x ^ y });
                result
            }),
            Code::RRA => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                let (result, next_carry) = cpu.ror(val);
                cpu.set_czn(result, next_carry);

                let (final_result, next_carry, overflow) = cpu.adc(result);
                cpu.set_cznv(final_result, next_carry, overflow);
                cpu.axy[A] = final_result;
                result //Set M to the result of the ROR
            }),
            
            Code::ALR => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                cpu.bitwise_helper(val, &|x, y| { x & y });
                let result = cpu.axy[A];
                let (final_result, carry) = lsr(result);
                cpu.set_czn(result, carry);
                final_result
            }),

            Code::ARR => self.address_rmw(a, min_cycles, Some(A), &|cpu, val| {
                let acc = cpu.axy[A];
                let result = acc & val;

                let (final_result, next_carry) = cpu.ror(result);
                cpu.set_czn(result, next_carry);
                final_result
            }),


            Code::TAS => {
                let pc = self.pc;
                let upper = self.read(pc + 1).wrapping_add(1);
                let val = self.axy[A] & self.axy[X];
                self.stack_pointer = val;
                let result = val & upper;
                self.address_write(a, min_cycles, result, None)
            },
            Code::SAY => {
                let pc = self.pc;
                let upper = self.read(pc + 1).wrapping_add(1);
                let val = self.axy[Y] & upper;
                self.address_write(a, min_cycles, val, None)
            },
            Code::XAS => {
                let pc = self.pc;
                let upper = self.read(pc + 1).wrapping_add(1);
                let val = self.axy[X] & upper;
                self.address_write(a, min_cycles, val, None)
            },
            Code::AXA => {
                let pc = self.pc;
                let upper = self.read(pc + 1).wrapping_add(1);
                let val = self.axy[X] & self.axy[A] & upper;
                self.address_write(a, min_cycles, val, None)
            },
            
            Code::ILL => panic!(format!("{:?}", EmuError::IllegalInstruction))
        }
    }

    fn set_zn(&mut self, val: u8) {
        if val == 0 {
            self.set_flag_op(StatusFlags::Z);
            self.clear_flag_op(StatusFlags::N);
        } else if val < 128 { //positive
            self.clear_flag_op(StatusFlags::N);
            self.clear_flag_op(StatusFlags::Z);
        } else { //negative
            self.set_flag_op(StatusFlags::N);
            self.clear_flag_op(StatusFlags::Z);
        }
    }

    fn set_czn(&mut self, val: u8, carry: bool) {
        self.set_zn(val);
        if carry {
            self.set_flag_op(StatusFlags::C)
        } else {
            self.clear_flag_op(StatusFlags::C)
        }
    }

    fn set_cznv(&mut self, val: u8, carry: bool, overflow: bool) {
        self.set_czn(val, carry);
        if overflow {
            self.set_flag_op(StatusFlags::V);
        } else {
            self.clear_flag_op(StatusFlags::V);
        }
    }

    fn stack_push(&mut self, val: u8) {
        let addr = (self.stack_pointer as u16) + STACK_REGION;
        self.ram[addr as usize] = val;
        let new_sp = self.stack_pointer.wrapping_sub(1);
        self.stack_pointer = new_sp;
    }
    
    fn stack_push_double(&mut self, val: u16) {
        let (lower, upper) = split(val);
        self.stack_push(upper);
        self.stack_push(lower)
    }

    fn stack_pop(&mut self) -> u8 {
        let new_sp = self.stack_pointer.wrapping_add(1);
        let addr = (new_sp as u16)  + STACK_REGION;
        self.stack_pointer = new_sp;
        let val = self.ram[addr as usize];
        val
    }

    fn stack_pop_double(&mut self) -> u16 {
        let lower = self.stack_pop();
        let upper = self.stack_pop();
        let val = combine(lower, upper);
        val
    }

    fn load(&mut self, a: Address, min_cycles: isize, r: usize) {
        let (bytes, extra_cycles, val) = self.address_read(a, None);
        if self.debug && a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        } 
        self.set_zn(val);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        self.axy[r] = val;
    }

    fn inc(&mut self, a: Address, min_cycles: isize, implied: Option<usize>) {
        self.address_rmw(a, min_cycles, implied, &|cpu, x| {
            let result = x.wrapping_add(1);
            cpu.set_zn(result);
            result
        })
    }

    fn dec(&mut self, a: Address, min_cycles: isize, implied: Option<usize>) {
       self.address_rmw(a, min_cycles, implied, &|cpu, x| {
           let result = x.wrapping_sub(1);
           cpu.set_zn(result);
           result
        })
    }

    fn adc(&self, rhs: u8) -> (u8, bool, bool) {
        let carry = self.status_register.get_flag(StatusFlags::C).bits;

        let lhs = self.axy[A];

        let next_carry = (rhs == 255 && carry == 1) || u8::MAX - rhs - carry < lhs;

        let result = lhs.wrapping_add(rhs.wrapping_add(carry));
        
        let overflow = ((!(lhs ^ rhs) & 0b1000_0000) == 0b1000_0000) &&
            ((lhs ^ result) & 0b1000_0000) == 0b1000_0000;
        (result, next_carry, overflow)
    }

    fn add(&mut self, a: Address, min_cycles: isize) {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None);
        
        if self.debug && a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", rhs));
        }
        
        let (result, next_carry, overflow) = self.adc(rhs);

        self.set_cznv(result, next_carry, overflow);

        self.axy[A] = result;

        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
    }

    fn sub(&mut self, a: Address, min_cycles: isize) {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None);
        if self.debug && a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", rhs));
        }
        let (result, next_carry, overflow) = self.adc(!rhs);
        self.set_cznv(result, next_carry, overflow);
        self.axy[A] = result;
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
    }

    fn set_flag_op(&mut self, flag: StatusFlags) {
        self.status_register |= flag;
    }

    fn clear_flag_op(&mut self, flag: StatusFlags) {
        self.status_register &= !flag;
    }

    fn set_flag(&mut self, min_cycles: isize, flag: StatusFlags) {
        self.counter -= min_cycles;
        self.set_flag_op(flag);
    }

    fn clear_flag(&mut self, min_cycles: isize, flag: StatusFlags) {
        self.counter -= min_cycles;
        self.clear_flag_op(flag);
    }

    fn branch(&mut self, a: Address, min_cycles: isize, flag: StatusFlags, on: bool) {
        let cond = if on {
            self.status_register.status(flag)
        } else {
            !self.status_register.status(flag)
        };
        
        let (bytes, _, val) = self.address_read(a, None);
        self.pc += bytes;
        
        let (extra, next_addr) = if val > 127 {
            let disp: u16 = match (val as i8).checked_abs() {
                Some(d) => d as u16,
                None => 128
            };
            let extra = if self.pc & 0xFF <= disp {
                2
            } else {
                0
            };
            (extra, self.pc - disp)
        } else {
            let extra = if (self.pc & 0xFF + (val as u16)) >= 0x100 {
                2
            } else {
                0
            };
            (extra, self.pc + (val as u16))
        };

        if self.debug {
            self.last_instr.push_str(&format!("${:04X} ", next_addr));
        }
        
        if cond {
            self.pc = next_addr;
            self.counter -= min_cycles + 1 + extra;
        } else {
            self.counter -= min_cycles;
        }
    }

    fn cmp(&mut self, a: Address, min_cycles: isize, lhs_idx: usize) {
        let lhs = self.axy[lhs_idx];
        let (bytes, extra_cycles, val) = self.address_read(a, None);
        if self.debug && a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        } 
        let result = lhs.wrapping_sub(val);
        let carry = lhs >= val;
        self.set_czn(result, carry);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
    }

    fn bitwise_helper(&mut self, val: u8, op: &Fn(u8, u8) -> u8) {
        let res = op(self.axy[A], val);
        self.axy[A] = res;
        self.set_zn(res);
    }

    fn bitwise(&mut self, a: Address, min_cycles: isize, op: &Fn(u8, u8) -> u8) {
        let (bytes, extra_cycles, val) = self.address_read(a, None);
        if self.debug && a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        }
        self.bitwise_helper(val, op);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
    }

    fn modify_pc_counter(&mut self, bytes: u16, cycles: isize) {
        self.pc += bytes;
        self.counter -= cycles;
    }

    fn rol(&self, lhs: u8) -> (u8, bool) {
        let old_carry = self.status_register.get_flag_bit(StatusFlags::C);
        let next_carry = (lhs & 0b1000_0000) == 0b1000_0000;
        ((lhs << 1) | old_carry, next_carry)
    }

    fn ror(&self, lhs: u8) -> (u8, bool) {
        let old_carry = self.status_register.get_flag_bit(StatusFlags::C);
        let next_carry = lhs & 0b0000_0001 == 0b0000_0001;
        ((lhs >> 1) | (old_carry << 7), next_carry)
    }
}

#[inline(always)]
fn lsr(lhs: u8) -> (u8, bool) {
    let carry = lhs & 0b0000_00001 == 0b0000_0001;
    (lhs >> 1, carry)
}

#[inline(always)]
fn asl(lhs: u8) -> (u8, bool) {
    let carry = (lhs & 0b1000_0000) == 0b1000_0000;
    (lhs << 1, carry)
}

