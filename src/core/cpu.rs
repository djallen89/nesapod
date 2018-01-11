use std::{i8, u8, u16, fmt, iter};
use core::ppu::{IMAGE_SIZE, PPU};
use core::ines::INES;
use core::addressing::{OPCODE_TABLE, Address, AddressType, SingleType, DoubleType};

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const FRAME_TIMING: u16 = 29781;
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const IRQ_VECTOR: u16 = 0xFFFE;
pub const STACK_REGION: u16 = 0x0100;
pub const A: usize = 0;
pub const X: usize = 1;
pub const Y: usize = 2;

pub type CPUResult<T> = Result<T, String>;

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
    
    pub fn get_flags_status(&self, flag: StatusFlags) -> bool {
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
pub fn counter_inc(x: u16, y: u8) -> u16 {
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
    ADC, SBC, 
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
    NOP, BRK,
    SLO, RLA, LSE, RRA,
    ARR, XAA, OAL, SAX,
    TAS, SAY, XAS, AXA,
    ILL
}

pub struct CPU {
    counter: u16,
    pc: u16,
    stack_pointer: u8,
    axy: Vec<u8>,
    aio_registers: Vec<u8>,
    status_register: StatusFlags,
    ram: [u8; 2048],
    ppu: PPU,
    cartridge: INES,
    last_read_bytes: String,
    last_instr: String,
    last_registers: String,
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let spaces1: String = iter::repeat(' ').take(16 - self.last_read_bytes.len()).collect();
        let spaces2: String = iter::repeat(' ').take(32 - self.last_instr.len()).collect();
        write!(f, "{}{}{}{}", self.last_read_bytes, spaces1, self.last_instr, spaces2)?;
        write!(f, "{}", self.last_registers)
    }
}

impl CPU {
    pub fn print_screen(&self) -> &[u8; IMAGE_SIZE] {
        self.ppu.print_screen()
    }
    
    pub fn power_up(ines: INES) -> Result<CPU, String> {
        Ok(CPU {
            counter: 0,
            pc: RESET_VECTOR,
            stack_pointer: POWERUP_S,
            axy: vec![0, 0, 0],
            aio_registers: vec![0; 32],
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 2048],
            ppu: PPU::init(),
            cartridge: ines,
            last_read_bytes: format!(""),
            last_instr: format!(""),
            last_registers: format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                    0, 0, 0, (StatusFlags::I | StatusFlags::S).bits, POWERUP_S),
        })
    }

    pub fn reset(&mut self) -> CPUResult<String> {
        self.stack_pointer -= 3;
        self.pc = RESET_VECTOR;
        self.status_register = self.status_register | StatusFlags::I;
        //self.step()
        self.execute(Code::JMP, Address::Specified(AddressType::DoubleByte(DoubleType::Absolute)), 5)
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
    }

    pub fn dump_ram(&self) -> String {
        //let lsb = self.ram[1] as usize;
        //let msb = self.ram[2] as usize;
        //format!("{:02X} {:02X}", lsb, msb)
        self.cartridge.dump_ram()
    }

    pub fn step(&mut self) -> CPUResult<String> {
        self.last_instr = format!("");
        let last_registers = format!("A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X}",
                                     self.axy[A], self.axy[X],
                                     self.axy[Y], self.status_register.bits,
                                     self.stack_pointer);
        self.last_registers = last_registers;
        let addr = self.pc;
        let opcode = self.read(addr)?;
        self.last_read_bytes = format!("{:04X}  {:02X} ", addr, opcode);
        let (code, address, cycles) = OPCODE_TABLE[opcode as usize];
        self.last_instr = format!("{:?} ", code);
        if self.counter >= (FRAME_TIMING - 7) / 3 {
            self.ppu.render(&mut self.cartridge);
            self.counter = 0; 
        }
        let val = self.pc;
        self.pc = val.wrapping_add(1);
        self.execute(code, address, cycles)
    }

    #[inline(always)]
    pub fn read(&mut self, address: u16) -> CPUResult<u8> {
        match address {
            0x0000 ... 0x1FFF => Ok(self.ram[(address % 2048) as usize]),
            0x2000 ... 0x3FFF => Ok(self.ppu.read(address)),
            0x4000 ... 0x4017 => Ok(self.aio_registers[(address - 0x4000) as usize]),
            0x4018 ... 0x401F => Err(format!("CPU test mode not yet implemnted!")),
            0x4020 ... 0xFFFF => self.cartridge.read(address),
            _ => Err(format!("I dunno lol"))
        }
    }
    
    #[inline(always)]
    pub fn read_two_bytes(&mut self, addr: u16) -> CPUResult<u16> {
        let lower = self.read(addr)?;
        let upper = self.read(addr + 1)?;
        Ok(combine(lower, upper))
    }

    #[inline(always)]
    pub fn write(&mut self, address: u16, val: u8) -> CPUResult<String> {
        match address as usize {
            0x0000 ... 0x1FFF => {
                let pre = self.ram[(address % 2048) as usize];
                self.last_instr.push_str(&format!("= {:02X}", pre));
                self.ram[(address % 2048) as usize] = val;
                Ok(format!("Wrote {:02X} to address {:04X}", val, address))
            },
            0x2000 ... 0x3FFF => {
                let pre = self.ppu.read(address);
                self.last_instr.push_str(&format!("= {:02X}", pre));
                self.ppu.write(address, val);
                Ok(format!("Wrote {:02X} to PPU register 0x200{}", val, address % 8))
            },
            0x4014 => {
                let page = (val as u16) << 8;
                for b in page .. page + 256 {
                    let val = self.read(b)?;
                    self.ppu.oam_dma_write(val);
                }
                self.counter += 514;
                Ok(format!("Wrote {:04X} to {:04X} to PPU OAM", page, page + 255))
            }
            0x4000 ... 0x4013 | 0x4015 ... 0x4017 => {
                let pre = self.aio_registers[(address - 0x4000) as usize];
                self.last_instr.push_str(&format!("= {:02X}", pre));
                self.aio_registers[(address - 0x4000) as usize] = val;
                Ok(format!("Wrote {:02X} to address {:04X}", val, address))
            },
            0x4018 ... 0x401F => Err(format!("CPU test mode not yet implemnted!")),
            0x4020 ... 0xFFFF => {
                let pre = self.cartridge.read(address)?;
                self.last_instr.push_str(&format!("= {:02X}", pre));
                self.cartridge.write(address, val)
            },
            _ => Err(format!("I dunno lol"))
        }
    }

    #[inline(always)]
    fn decode_single_byte(&mut self, s: SingleType) -> CPUResult<(u16, u16)> {
        let pc = self.pc;
        let addr = self.read(pc)?;
        self.last_read_bytes.push_str(&format!("{:02X} ", addr));
        match s {
            SingleType::ZeroPg => {
                self.last_instr.push_str(&format!("${:02X} ", addr));
                Ok((0, addr as u16))
            },
            SingleType::ZeroPgX => {
                let real_addr = self.axy[X].wrapping_add(addr) as u16;
                self.last_instr.push_str(&format!("${:02X},X @ {:02X} ", addr, real_addr));
                Ok((0, real_addr))
            },
            SingleType::ZeroPgY => {
                let real_addr = self.axy[Y].wrapping_add(addr) as u16;
                self.last_instr.push_str(&format!("${:02X},Y @ {:02X} ", addr, real_addr));
                Ok((0, real_addr))
            },
            SingleType::IndirectX => {
                let lsb = self.axy[X].wrapping_add(addr);
                let real_addr_low = self.ram[(lsb as usize)];
                let real_addr_high = self.ram[(lsb.wrapping_add(1) as usize)];
                let real_addr = combine(real_addr_low, real_addr_high);
                self.last_instr.push_str(&format!("(${:02X},X) @ {:02X} = {:04X} ", addr,
                                                  addr.wrapping_add(self.axy[X]),
                                                  real_addr));
                Ok((0, real_addr))
            },
            SingleType::IndirectY => {
                let low = self.ram[addr as usize];
                let high = self.ram[(addr as u8).wrapping_add(1) as usize];
                let pre_addr = combine(low, high);
                let extra = counter_inc(pre_addr, self.axy[Y]);
                let real_addr = (self.axy[Y] as u16).wrapping_add(pre_addr);
                self.last_instr.push_str(&format!("(${:02X}),Y = {:04X} @ {:04X} ",
                                                  addr, pre_addr, real_addr));
                Ok((extra, real_addr))
            },
            _ => panic!(format!("{:?} is not supported in decode_singlebyte", s))
        }
    }

    #[inline(always)]
    fn decode_double_byte(&mut self, d: DoubleType) -> CPUResult<(u16, u16)> {
        let pc = self.pc;
        let addr = self.read_two_bytes(pc)?;
        self.last_read_bytes.push_str(&format!("{:02X} {:02X} ", addr & 0xFF, addr >> 8));
        match d {
            DoubleType::Absolute => {
                self.last_instr.push_str(&format!("${:04X} ", addr));
                Ok((0, addr))
            },
            DoubleType::AbsoluteX => {
                let real_addr = (self.axy[X] as u16) + addr;
                let extra = counter_inc(addr, self.axy[Y]);
                self.last_instr.push_str(&format!("${:04X},X @ {:04X} ", addr, real_addr));
                Ok((extra, real_addr))
            },
            DoubleType::AbsoluteY => {
                let extra = counter_inc(addr, self.axy[X]);
                let real_addr = (self.axy[Y] as u16).wrapping_add(addr);
                self.last_instr.push_str(&format!("${:04X},Y @ {:04X} ", addr, real_addr));
                Ok((extra, real_addr))
            },
            DoubleType::Indirect => panic!("No indirect in decode double byte!")
        }
    }

    /// Returns the number of bytes of an instruction, if it takes an extra cycle by
    /// crossing a page and the value stored at that location (excepting immediate and
    /// relative instructions).
    #[inline(always)]
    fn address_read(&mut self, a: Address, implied: Option<usize>) -> CPUResult<(u16, u16, u8)> {
        let pc = self.pc;
        let (bytes, extra_cycles, val) = match a {
            Address::Invalid => panic!("Improper use of address_read: Invalid"),
            Address::Implied => if let Some(idx) = implied {
                (0, 0, self.axy[idx])
            } else {
                panic!("Improper use of address read: None Implied")
            },                
            Address::Acc => (0, 0, self.axy[A]),
            Address::Specified(AddressType::SingleByte(SingleType::Relative)) => {
                let val = self.read(pc)?;
                self.last_read_bytes.push_str(&format!("{:02X} ", val));
                (1, 0, val)
            },
            Address::Specified(AddressType::SingleByte(SingleType::Immediate)) => {
                let val = self.read(pc)?;
                self.last_read_bytes.push_str(&format!("{:02X} ", val));
                self.last_instr.push_str(&format!("#${:02X} ", val));
                (1, 0, val)
            },
            Address::Specified(AddressType::SingleByte(s)) => {
                let (extra_cycles, addr) = self.decode_single_byte(s)?;
                let val = self.read(addr)?;
                (1, extra_cycles, val)
            },
            Address::Specified(AddressType::DoubleByte(d)) => {
                let (extra_cycles, addr) = self.decode_double_byte(d)?;
                let val = self.read(addr)?;
                (2, extra_cycles, val)
            }
        };
        Ok((bytes, extra_cycles, val))
    }
    
    #[inline(always)]
    fn address_rmw(&mut self, a: Address, min_cycles: u16, msg: &str, implied: Option<usize>,
                                 op: &Fn(&mut CPU, u8) -> u8) -> CPUResult<String> {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;

        self.counter += min_cycles;

        match a {
            Invalid | 
            Specified(DoubleByte(Indirect)) |
            Specified(SingleByte(Relative)) |
            Specified(SingleByte(Immediate)) => panic!("Don't use read modify write for {:?}", a),
            Implied => if let Some(idx) = implied {
                let val = self.axy[idx];
                let result = op(self, val);
                self.axy[idx] = result;
                Ok(format!("{} ({:02X}) for result {:02X}", msg, val, result))
            } else {
                panic!("Improper use of rmw for None Implied")
            },
            Acc => {
                let val = self.axy[A];
                let result = op(self, val);
                self.axy[A] = result;
                Ok(format!("{} Acc ({:02X}) for result {:02X}", msg, val, result))
            },
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s)?;
                let val = self.read(addr)?;
                let result = op(self, val);
                self.write(addr, result)?;
                self.pc += 1;
                Ok(format!("{} ${:04X} ({:02X}) for result {:02X}", msg, addr, val, result))
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d)?;
                let val = self.read(addr)?;
                let result = op(self, val);
                self.write(addr, result)?;
                self.pc += 2;
                Ok(format!("{} ${:04X} ({:02X}) for result {:02X}", msg, addr, val, result))
            }
        }
    }

    #[inline(always)]
    fn address_write(&mut self, a: Address, min_cycles: u16, val: u8, implied: Option<usize>) -> CPUResult<String> {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;

        let (bytes, msg) = match a {
            Acc |
            Invalid | 
            Specified(SingleByte(Immediate)) |
            Specified(SingleByte(Relative)) |
            Specified(DoubleByte(Indirect)) => panic!("Improper use of address_write!"),
            Implied => if let Some(idx) = implied {
                self.axy[idx] = val;
                (0, format!("axy register {}", idx))
            } else {
                panic!("Improper use of adress write for None Implied")
            },
            Specified(SingleByte(s)) => {
                let (_extra_cyles, addr) = self.decode_single_byte(s)?;
                self.write(addr, val)?;
                (1, format!("{:?}: {:04X}", s, addr))
            },
            Specified(DoubleByte(d)) => {
                let (_extra_cycles, addr) = self.decode_double_byte(d)?;
                self.write(addr, val)?;
                (2, format!("{:?}: {:04X}", d, addr))
            },
        };
        self.counter += min_cycles;
        self.pc += bytes;
        Ok(format!("Stored {:02X} into {}", val, msg))
    }
    
    #[inline(always)]
    fn execute(&mut self, c: Code, a: Address, min_cycles: u16) -> CPUResult<String> {
        match c {
            Code::LDA => self.load(a, min_cycles, A),
            Code::LDX => self.load(a, min_cycles, X),
            Code::LDY => self.load(a, min_cycles, Y),
            Code::LAX => {
                let (bytes, extra_cycles, val) = self.address_read(a, None)?;
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    self.last_instr.push_str(&format!("= {:02X}", val));
                } 
                self.set_zn(val);
                self.modify_pc_counter(bytes, min_cycles + extra_cycles);
                self.axy[A] = val;
                self.axy[X] = val;
                Ok(format!("Loaded ({:08b}) into ACC and X", val))
            },
            Code::XAA => {
                let (bytes, extra_cycles, val) = self.address_read(a, None)?;
                let x = self.axy[X];
                let result = self.axy[A] & val;
                self.axy[A] = result;
                self.set_zn(result);
                self.modify_pc_counter(bytes, min_cycles + extra_cycles);
                Ok(format!("Loaded ({:08b}) into ACC and X", val))
            },

            Code::STA => {
                let val = self.axy[A];
                self.address_write(a, min_cycles, val, None)
            },
            Code::STX => {
                let val = self.axy[X];
                self.address_write(a, min_cycles, val, None)
            },
            Code::STY => {
                let val = self.axy[Y];
                self.address_write(a, min_cycles, val, None)
            },
            Code::ASX => {
                let val = self.axy[A] & self.axy[X];
                self.address_write(a, min_cycles, val, None)
            },
            Code::SAX => {
                let (bytes, _, rhs) = self.address_read(a, None)?;
                let lhs = self.axy[A] & self.axy[X];
                let carry = lhs >= rhs;
                let result = lhs.wrapping_sub(rhs);
                self.set_czn(result, carry);
                self.axy[X] = result;
                self.modify_pc_counter(bytes, min_cycles);
                Ok(format!("SAX: Loaded ({:02X}) into X", result))
            },
            
            Code::ADC => self.add(a, min_cycles),
            Code::SBC => self.sub(a, min_cycles),
            
            Code::INC => self.inc(a, min_cycles, None),
            Code::INX => self.inc(a, min_cycles, Some(X)),
            Code::INY => self.inc(a, min_cycles, Some(Y)),
            Code::INS => self.address_rmw(a, min_cycles, "INS: INC -> SBC", None, &|cpu, val| {
                let result = val.wrapping_add(1);
                let lhs = cpu.axy[A];
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                }

                let (final_result, next_carry, overflow) = cpu.adc(!result);
                cpu.set_cznv(final_result, next_carry, overflow);
                cpu.axy[A] = final_result;
                result //Set M to the result of the INC
            }),
            
            Code::DEC => self.dec(a, min_cycles, None),
            Code::DEX => self.dec(a, min_cycles, Some(X)),
            Code::DEY => self.dec(a, min_cycles, Some(Y)),
            Code::DCM => self.address_rmw(a, min_cycles, "DCM: DEC -> CMP", None, &|cpu, val| {
                let result = val.wrapping_sub(1);
                let lhs = cpu.axy[A];
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                } 
                let final_result = lhs.wrapping_sub(result);
                let carry = lhs >= result;
                cpu.set_czn(final_result, carry);
                result
            }),
            
            Code::ASL => self.address_rmw(a, min_cycles, "Shifted left", Some(A), &|cpu, x| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = asl(x);
                cpu.set_czn(result, carry);
                result
            }),
            Code::LSR => self.address_rmw(a, min_cycles, "Shifted right", Some(A), &|cpu, x| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = lsr(x);
                cpu.set_czn(result, carry);
                result
            }),
            Code::ROL => self.address_rmw(a, min_cycles, "Rotated left", Some(A), &|cpu, x| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, next_carry) = cpu.rol(x);
                cpu.set_czn(result, next_carry);
                result
            }),
            Code::ROR => self.address_rmw(a, min_cycles, "Rotated right", Some(A), &|cpu, x| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, next_carry) = cpu.ror(x);
                cpu.set_czn(result, next_carry);
                result
            }),

            Code::SLO => self.address_rmw(a, min_cycles, "SLO: ASL -> ORA", Some(A), &|cpu, val| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, carry) = asl(val);
                cpu.set_czn(result, carry);
                
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                }
                cpu.bitwise_helper(result, &|x, y| { x | y });
                cpu.axy[A]
            }),
            Code::RLA => self.address_rmw(a, min_cycles, "RLA: ROL -> AND", Some(A), &|cpu, val| {
                if a == Address::Acc {
                    cpu.last_instr.push_str("A ");
                }
                let (result, next_carry) = cpu.ror(val);
                cpu.set_czn(result, next_carry);
                
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                }
                cpu.bitwise_helper(result, &|x, y| { x & y });
                cpu.axy[A]
            }),
            Code::LSE => self.address_rmw(a, min_cycles, "LSE: LSR -> EOR", Some(A), &|cpu, val| {
                let (result, carry) = lsr(val);
                cpu.set_czn(result, carry);
                
                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                }
                cpu.bitwise_helper(result, &|x, y| { x ^ y });
                cpu.axy[A]
            }),
            Code::RRA => self.address_rmw(a, min_cycles, "RRA: ROR -> ADC", Some(A), &|cpu, val| {
                let (result, next_carry) = cpu.ror(val);
                cpu.set_czn(result, next_carry);

                if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
                    cpu.last_instr.push_str(&format!("= {:02X}", val));
                }

                let (final_result, next_carry, overflow) = cpu.adc(result);
                cpu.set_cznv(final_result, next_carry, overflow);
                cpu.axy[A] = final_result;
                result //Set M to the result of the ROR
            }),
            
            Code::ALR => self.address_rmw(a, min_cycles, "ALR: AND -> LSR", Some(A), &|cpu, val| {
                cpu.bitwise_helper(val, &|x, y| { x & y });
                let result = cpu.axy[A];
                let (final_result, carry) = lsr(result);
                cpu.set_czn(result, carry);
                final_result
            }),

            Code::ARR => self.address_rmw(a, min_cycles, "ARR: AND -> ROR", Some(A), &|cpu, val| {
                let acc = cpu.axy[A];
                let result = acc & val;

                let (final_result, next_carry) = cpu.ror(result);
                cpu.set_czn(result, next_carry);
                final_result
            }),

            Code::TAS => {
                let pc = self.pc;
                let upper = self.read(pc + 1)?.wrapping_add(1);
                let val = self.axy[A] & self.axy[X];
                self.stack_pointer = val;
                let result = val & upper;
                self.address_write(a, min_cycles, result, None)
            },
            Code::SAY => {
                let pc = self.pc;
                let upper = self.read(pc + 1)?.wrapping_add(1);
                let val = self.axy[Y] & upper;
                self.address_write(a, min_cycles, val, None)
            },
            Code::XAS => {
                let pc = self.pc;
                let upper = self.read(pc + 1)?.wrapping_add(1);
                let val = self.axy[X] & upper;
                self.address_write(a, min_cycles, val, None)
            },
            Code::AXA => {
                let pc = self.pc;
                let upper = self.read(pc + 1)?.wrapping_add(1);
                let val = self.axy[X] & self.axy[A] & upper;
                self.address_write(a, min_cycles, val, None)
            },

            Code::AND => self.bitwise(a, min_cycles, &|acc, x| { acc & x }, "Logical AND"),
            Code::EOR => self.bitwise(a, min_cycles, &|acc, x| { acc ^ x }, "Logical EOR"),
            Code::ORA => self.bitwise(a, min_cycles, &|acc, x| { acc | x }, "Logical ORA"),
            Code::OAL => {
                let res = self.bitwise(a, min_cycles, &|acc, x| { (acc | 0xEE) & x }, "OAL: ORA $#EE -> AND")?;
                let a = self.axy[A];
                self.axy[X] = a;
                Ok(res)
            },
            
            Code::CMP => self.cmp(a, min_cycles, A),
            Code::CPX => self.cmp(a, min_cycles, X),
            Code::CPY => self.cmp(a, min_cycles, Y),
            
            Code::BIT => {
                let (bytes, extra_cycles, val) = self.address_read(a, None)?;
                self.last_instr.push_str(&format!("= {:02X}", val));
                let zero = self.axy[A] & val == 0;
                let n = (val & 0b1000_0000) == 0b1000_0000;
                let v = (val & 0b0100_0000) == 0b0100_0000;
                if n {
                    self.set_flag_op(StatusFlags::N)
                } else {
                    self.clear_flag_op(StatusFlags::N)
                }
                if v {
                    self.set_flag_op(StatusFlags::V)
                } else {
                    self.clear_flag_op(StatusFlags::V)
                }
                if zero {
                    self.set_flag_op(StatusFlags::Z)
                } else {
                    self.clear_flag_op(StatusFlags::Z)
                }
                self.counter += min_cycles + extra_cycles;
                self.pc += bytes;
                Ok(format!("BIT tested {:08b}", val))
            },
            
            Code::BCC => self.branch(a, min_cycles, "BCC", StatusFlags::C, false),
            Code::BCS => self.branch(a, min_cycles, "BCS", StatusFlags::C, true),
            Code::BEQ => self.branch(a, min_cycles, "BEQ", StatusFlags::Z, true),
            Code::BNE => self.branch(a, min_cycles, "BNE", StatusFlags::Z, false),
            Code::BMI => self.branch(a, min_cycles, "BMI", StatusFlags::N, true),
            Code::BPL => self.branch(a, min_cycles, "BPL", StatusFlags::N, false),
            Code::BVC => self.branch(a, min_cycles, "BVC", StatusFlags::V, false),
            Code::BVS => self.branch(a, min_cycles, "BVS", StatusFlags::V, true),
            
            Code::TAX => {
                let val = self.axy[A];
                self.axy[X] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred A to X"))
            },
            Code::TXA => {
                let val = self.axy[X];
                self.axy[A] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred X to A"))
            },
            Code::TAY => {
                let val = self.axy[A];
                self.axy[Y] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred A to Y"))

            },
            Code::TYA => {
                let val = self.axy[Y];
                self.axy[A] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred Y to A"))

            },
            Code::TSX => {
                self.axy[X] = self.stack_pointer;
                let val = self.axy[X];
                self.set_zn(val);
                Ok(format!("Copied stack pointer to X"))
            },
            Code::TXS => {
                let val = self.axy[X];
                self.stack_pointer = val;
                self.counter += min_cycles;
                Ok(format!("Copied X to stack pointer"))
            },
            
            Code::PHA => {
                self.counter += min_cycles;
                let val = self.axy[A];
                self.stack_push(val)?;
                Ok(format!("Pushed accumulator to stack"))
            },
            Code::PLA => {
                self.counter += min_cycles;
                let acc = self.stack_pop()?;
                self.set_zn(acc);
                self.axy[A] = acc;
                Ok(format!("Pulled accumulator from stack"))
            },
            Code::PHP => {
                self.counter += min_cycles;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)?;
                Ok(format!("Pushed status onto stack"))
            },
            Code::PLP => {
                self.counter += min_cycles;
                let flags = self.stack_pop()? & 0b1110_1111;
                self.status_register.bits = flags | 0b0010_0000;
                Ok(format!("Pulled processor flags from stack"))
            },
            
            Code::JMP => {
                let pc = self.pc;
                let addr = self.read_two_bytes(pc)?;
                self.last_read_bytes.push_str(&format!("{:02X} {:02X}", addr & 0xFF, addr >> 8));
                match a {
                    Address::Specified(AddressType::DoubleByte(DoubleType::Indirect)) => {
                        //let real_addr = self.read_two_bytes(addr)?;
                        let low = self.read(addr)?;
                        let next = if addr & 0xFF == 0xFF {
                            addr & 0xFF00
                        } else {
                            addr.wrapping_add(1)
                        };
                        let high = self.read(next)?;
                        let real_addr = combine(low, high);
                        self.last_instr.push_str(&format!("(${:04X}) = {:04X}", addr, real_addr));
                        self.counter += min_cycles;
                        self.pc = real_addr;
                        Ok(format!("Set pc Indirect ${:04X} to ${:04X}", addr, real_addr))
                    },
                    Address::Specified(AddressType::DoubleByte(DoubleType::Absolute)) => {
                        self.last_instr.push_str(&format!("${:04X} ", addr));
                        self.counter += min_cycles;
                        self.pc = addr;
                        Ok(format!("Set pc to absolute ${:04X}", addr))
                    },
                    _ => Err(format!("JMP doesn't use {:?}", a))
                }
            },
            Code::JSR => {
                match a {
                    Address::Specified(AddressType::DoubleByte(DoubleType::Absolute)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.last_read_bytes.push_str(&format!("{:02X} {:02X}", addr & 0xFF, addr >> 8));
                        self.last_instr.push_str(&format!("${:04X} ", addr));
                        self.counter += min_cycles;
                        self.stack_push_double(pc + 1)?;
                        self.pc = addr;
                        Ok(format!("Pushed ${:04X} onto stack and set pc to {:04X}.", pc + 1, addr))
                    },
                    _ => Err(format!("JSR doesn't use {:?}", a))
                }
            },
            Code::RTS => {
                let addr = self.stack_pop_double()?;
                self.pc = addr + 1;
                self.counter += min_cycles;
                Ok(format!("Returned pc to {:04X}", self.pc)) 
            },
            Code::RTI => {
                let flags = self.stack_pop()? & 0b1110_1111;
                self.status_register.bits = flags | 0b0010_0000;
                let addr = self.stack_pop_double()?;
                self.pc = addr;
                self.counter += min_cycles;
                Ok(format!("Set pc to {:04X} from interrupt", self.pc))
            },
            
            Code::SEC => self.set_flag(min_cycles, StatusFlags::C),
            Code::SED => self.set_flag(min_cycles, StatusFlags::D),
            Code::SEI => self.set_flag(min_cycles, StatusFlags::I),
            
            Code::CLC => self.clear_flag(min_cycles, StatusFlags::C),
            Code::CLD => self.clear_flag(min_cycles, StatusFlags::D),
            Code::CLI => self.clear_flag(min_cycles, StatusFlags::I),
            Code::CLV => self.clear_flag(min_cycles, StatusFlags::V),
            
            Code::NOP => {
                let (bytes, extra_cycle, _) = if a != Address::Implied {
                    self.address_read(a, None)?
                } else {
                    (0, 0, 0)
                };
                self.modify_pc_counter(bytes, min_cycles + extra_cycle);
                Ok(format!("NOP"))
            },
            
            Code::BRK => {
                self.counter += min_cycles;
                let addr = self.pc + 1;
                self.stack_push_double(addr)?;
                
                let irq = self.read_two_bytes(IRQ_VECTOR)?;
                self.pc = irq;
                    
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },

            Code::ILL => Err(format!("Illegal instruction!",))
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

    fn stack_push(&mut self, val: u8) -> CPUResult<String> {
        let addr = (self.stack_pointer as u16) + STACK_REGION;
        self.ram[addr as usize] = val;
        let new_sp = self.stack_pointer.wrapping_sub(1);
        self.stack_pointer = new_sp;
        Ok(format!("Pushed {:02X} to stack at {:04X}", val, addr))
    }
    
    fn stack_push_double(&mut self, val: u16) -> CPUResult<String> {
        let (lower, upper) = split(val);
        self.stack_push(upper)?;
        self.stack_push(lower)
    }

    fn stack_pop(&mut self) -> CPUResult<u8> {
        let new_sp = self.stack_pointer.wrapping_add(1);
        let addr = (new_sp as u16)  + STACK_REGION;
        self.stack_pointer = new_sp;
        let val = self.ram[addr as usize];
        Ok(val)
    }

    fn stack_pop_double(&mut self) -> CPUResult<u16> {
        let lower = self.stack_pop()?;
        let upper = self.stack_pop()?;
        let val = combine(lower, upper);
        Ok(val)
    }

    fn load(&mut self, a: Address, min_cycles: u16, r: usize) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        } 
        self.set_zn(val);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        self.axy[r] = val;
        Ok(format!("Loaded ({:08b}) into axy register {}", val, r))
    }

    fn inc(&mut self, a: Address, min_cycles: u16, implied: Option<usize>) -> CPUResult<String> {
        let msg = "Incremented ";
        self.address_rmw(a, min_cycles, msg, implied, &|cpu, x| {
            let result = x.wrapping_add(1);
            cpu.set_zn(result);
            result
        })
    }

    fn dec(&mut self, a: Address, min_cycles: u16, implied: Option<usize>) -> CPUResult<String> {
       let msg = "Decremented ";
       self.address_rmw(a, min_cycles, msg, implied, &|cpu, x| {
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

    fn add(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None)?;
        if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", rhs));
        } 
        let (result, next_carry, overflow) = self.adc(rhs);
        self.set_cznv(result, next_carry, overflow);
        self.axy[A] = result;
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Added {} to accumulator for {} with carry {}",
                   rhs, result, next_carry))
    }

    fn sub(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None)?;
        if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", rhs));
        }
        let (result, next_carry, overflow) = self.adc(!rhs);
        self.set_cznv(result, next_carry, overflow);
        self.axy[A] = result;
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Subtracted {} from accumulator for result {} with carry: {} and overflow: {} ",
                   rhs, result, !next_carry, overflow))
    }

    fn set_flag_op(&mut self, flag: StatusFlags) {
        self.status_register |= flag;
    }

    fn clear_flag_op(&mut self, flag: StatusFlags) {
        self.status_register &= !flag;
    }

    fn set_flag(&mut self, min_cycles: u16, flag: StatusFlags) -> CPUResult<String> {
        self.counter += min_cycles;
        self.set_flag_op(flag);
        Ok(format!("Set {:?}", flag))
    }

    fn clear_flag(&mut self, min_cycles: u16, flag: StatusFlags) -> CPUResult<String> {
        self.counter += min_cycles;
        self.clear_flag_op(flag);
        Ok(format!("Cleared {:?}", flag))
    }

    fn branch(&mut self, a: Address, min_cycles: u16, msg: &str, flag: StatusFlags, on: bool) -> CPUResult<String> {
        let cond = if on {
            self.status_register.get_flags_status(flag)
        } else {
            !self.status_register.get_flags_status(flag)
        };
        let (bytes, _, val) = self.address_read(a, None)?;
        self.pc += bytes;
        let (msg, extra, next_addr) = if val > 127 {
            let disp: u16 = match (val as i8).checked_abs() {
                Some(d) => d as u16,
                None => 128
            };
            let extra = if self.pc & 0xFF <= disp {
                2
            } else {
                0
            };
            (format!("{}: Branched by -{}", msg, disp), extra, self.pc - disp)
        } else {
            let extra = if (self.pc & 0xFF + (val as u16)) >= 0x100 {
                2
            } else {
                0
            };
            (format!("{}: Branched by {}", msg, val), extra, self.pc + (val as u16))
        };

        self.last_instr.push_str(&format!("${:04X} ", next_addr));
        
        if cond {
            self.pc = next_addr;
            self.counter += min_cycles + 1 + extra;
            Ok(msg)
        } else {
            self.counter += min_cycles;
            Ok(format!("{}: No branch.", msg))
        }
    }

    fn cmp(&mut self, a: Address, min_cycles: u16, lhs_idx: usize) -> CPUResult<String> {
        let lhs = self.axy[lhs_idx];
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        } 
        let result = lhs.wrapping_sub(val);
        let carry = lhs >= val;
        self.set_czn(result, carry);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Compared {} to {} for result {:?}", lhs, val, self.status_register))
    }

    fn bitwise_helper(&mut self, val: u8, op: &Fn(u8, u8) -> u8) {
        let acc = self.axy[A];
        let res = op(self.axy[A], val);
        self.axy[A] = res;
        self.set_zn(res);
    }

    fn bitwise(&mut self, a: Address, min_cycles: u16, op: &Fn(u8, u8) -> u8, msg: &str) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        if a != Address::Specified(AddressType::SingleByte(SingleType::Immediate)) {
            self.last_instr.push_str(&format!("= {:02X}", val));
        }
        self.bitwise_helper(val, op);
        self.modify_pc_counter(bytes, min_cycles);
        Ok(format!("{} by {:02X}", msg, val))
    }

    fn modify_pc_counter(&mut self, bytes: u16, cycles: u16) {
        self.pc += bytes;
        self.counter += cycles;
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

