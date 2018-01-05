use std::{u8, u16, fmt};
use core::ppu::PPU;
use core::ines::INES;
use core::addressing::{OPCODE_TABLE, Address, SingleType, DoubleType};
use core::addressing::Address::*;
use core::addressing::AddressType::*;
use core::addressing::SingleType::*;
use core::addressing::DoubleType::*;

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const FRAME_TIMING: u16 = 29780;
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const IRQ_VECTOR: u16 = 0xFFFE;
pub const STACK_REGION: u16 = 0x0100;
pub const ACCUMULATOR: usize = 0;
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
    LDA, LDX, LDY, 
    STA, STX, STY, 
    ADC, SBC, 
    INC, INX, INY, 
    DEC, DEX, DEY, 
    ASL, LSR,
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
    ILL
}

pub struct CPU {
    counter: u16,
    pc: u16,
    stack_pointer: u8,
    axy_registers: Vec<u8>,
    aio_registers: Vec<u8>,
    status_register: StatusFlags,
    ram: [u8; 2048],
    ppu: PPU,
    cartridge: INES,
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "counter: {:04X} pc: {:04X} ", self.counter, self.pc)?;
        write!(f, "sp: {:02X} acc: {:02X} x: {:02X} ",
               self.stack_pointer, self.axy_registers[ACCUMULATOR], self.axy_registers[X])?;
        write!(f, "y: {:02X} flags: {:?}", self.axy_registers[Y], self.status_register)
    }
}

impl CPU {
    pub fn power_up(ines: INES) -> Result<CPU, String> {
        let mut cpu = CPU {
            counter: 0,
            pc: RESET_VECTOR,
            stack_pointer: POWERUP_S,
            axy_registers: vec![0, 0, 0],
            aio_registers: vec![0; 32],
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 2048],
            ppu: PPU::init(),
            cartridge: ines,
        };
        cpu.reset()?;
        Ok(cpu)
    }

    pub fn reset(&mut self) -> CPUResult<String> {
        self.stack_pointer -= 3;
        self.status_register = self.status_register | StatusFlags::I;
        self.execute(Code::JMP, Specified(DoubleByte(Absolute)), 5, 0x6C)
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
    }

    pub fn step(&mut self) -> CPUResult<String> {
        let addr = self.pc;
        let opcode = self.read(addr)?;
        let (code, address, cycles) = OPCODE_TABLE[opcode as usize];
        if self.counter >= FRAME_TIMING - 7 {
            self.counter = 0; 
        }
        let next = self.pc.wrapping_add(1);
        self.pc = next;
        self.execute(code, address, cycles, opcode)
    }

    pub fn rewind_pc(&mut self) {
        self.pc -= 1;
    }

    pub fn step_pc(&mut self) {
        self.pc += 1;
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
                self.ram[(address % 2048) as usize] = val;
                Ok(format!("Wrote {:02X} to address {:04X}", val, address))
            },
            0x2000 ... 0x3FFF => {
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
                self.aio_registers[(address - 0x4000) as usize] = val;
                Ok(format!("Wrote {:02X} to address {:04X}", val, address))
            },
            0x4018 ... 0x401F => Err(format!("CPU test mode not yet implemnted!")),
            0x4020 ... 0xFFFF => self.cartridge.write(address, val),
            _ => Err(format!("I dunno lol"))
        }
    }

    #[inline(always)]
    fn decode_single_byte(&mut self, s: SingleType) -> CPUResult<(u16, u16)> {
        let pc = self.pc;
        let addr = self.read(pc)?;
        match s {
            SingleType::ZeroPg => Ok((0, addr as u16)),
            SingleType::ZeroPgX => Ok((0, self.axy_registers[X].wrapping_add(addr) as u16)),
            SingleType::ZeroPgY => Ok((0, self.axy_registers[Y].wrapping_add(addr) as u16)),
            SingleType::IndirectX => {
                let real_addr = self.axy_registers[X].wrapping_add(addr) as u16;
                Ok((0, self.read_two_bytes(real_addr)?))
            },
            SingleType::IndirectY => {
                let pre_addr = self.read_two_bytes(addr as u16)?;
                let extra = counter_inc(pre_addr, self.axy_registers[Y]);
                let real_addr = (self.axy_registers[Y] as u16) + pre_addr;
                Ok((extra, real_addr))
            },
            _ => panic!(format!("{:?} is not supported in decode_singlebyte", s))
        }
    }

    #[inline(always)]
    fn decode_double_byte(&mut self, d: DoubleType) -> CPUResult<(u16, u16)> {
        let pc = self.pc;
        let addr = self.read_two_bytes(pc)?;
        match d {
            DoubleType::Absolute => Ok((0, addr)),
            DoubleType::AbsoluteY => {
                let extra = counter_inc(addr, self.axy_registers[X]);
                Ok((extra, (self.axy_registers[Y] as u16) + addr))
            },
            DoubleType::AbsoluteX => {
                let extra = counter_inc(addr, self.axy_registers[Y]);
                Ok((extra, (self.axy_registers[X] as u16) + addr))
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
            Invalid => panic!("Improper use of address_read: Invalid"),
            Implied => if let Some(idx) = implied {
                (0, 0, self.axy_registers[idx])
            } else {
                panic!("Improper use of address read: None Implied")
            },                
            Acc => (0, 0, self.axy_registers[ACCUMULATOR]),
            Specified(SingleByte(Relative)) | 
            Specified(SingleByte(Immediate)) => (1, 0, self.read(pc)?),
            Specified(SingleByte(s)) => {
                let (extra_cycles, addr) = self.decode_single_byte(s)?;
                let val = self.read(addr)?;
                (1, extra_cycles, val)
            },
            Specified(DoubleByte(d)) => {
                let (extra_cycles, addr) = self.decode_double_byte(d)?;
                let val = self.read(addr)?;
                (2, extra_cycles, val)
            }
        };
        Ok((bytes, extra_cycles, val))
    }
    
    #[inline(always)]
    fn address_read_modify_write(&mut self, a: Address, min_cycles: u16, msg: &str, implied: Option<usize>,
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
                let val = self.axy_registers[idx];
                let res = op(self, val);
                self.axy_registers[idx] = res;
                Ok(format!("{} ({:02X}) for res {:02X}", msg, val, res))
            } else {
                panic!("Improper use of rmw for None Implied")
            },
            Acc => {
                let val = self.axy_registers[ACCUMULATOR];
                let res = op(self, val);
                self.axy_registers[ACCUMULATOR] = res;
                Ok(format!("{} Acc ({:02X}) for res {:02X}", msg, val, res))
            },
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s)?;
                let val = self.read(addr)?;
                let res = op(self, val);
                self.write(addr, res)?;
                self.pc += 1;
                Ok(format!("{} ${:04X} ({:02X}) for res {:02X}", msg, addr, val, res))
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d)?;
                let val = self.read(addr)?;
                let res = op(self, val);
                self.write(addr, val)?;
                self.pc += 2;
                Ok(format!("{} ${:04X} ({:02X}) for res {:02X}", msg, addr, val, res))
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
                self.axy_registers[idx] = val;
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
    fn execute(&mut self, c: Code, a: Address, min_cycles: u16, o: u8) -> CPUResult<String> {
        match c {
            Code::LDA => self.load(a, min_cycles, ACCUMULATOR),
            Code::LDX => self.load(a, min_cycles, X),
            Code::LDY => self.load(a, min_cycles, Y),
            
            Code::STA => {
                let val = self.axy_registers[ACCUMULATOR];
                self.address_write(a, min_cycles, val, None)
            },
            Code::STX => {
                let val = self.axy_registers[X];
                self.address_write(a, min_cycles, val, None)
            },
            Code::STY => {
                let val = self.axy_registers[Y];
                self.address_write(a, min_cycles, val, None)
            },
            
            Code::ADC => self.add(a, min_cycles),
            Code::SBC => self.sub(a, min_cycles),
            
            Code::INC => self.inc(a, min_cycles, None),
            Code::INX => self.inc(a, min_cycles, Some(X)),
            Code::INY => self.inc(a, min_cycles, Some(Y)),
            Code::DEC => self.dec(a, min_cycles, None),
            Code::DEX => self.dec(a, min_cycles, Some(X)),
            Code::DEY => self.dec(a, min_cycles, Some(Y)),
            
            Code::ASL => self.address_read_modify_write(a, min_cycles, "Shifted left", Some(ACCUMULATOR), &|cpu, x| {
                let carry = (x & 0b1000_0000) == 0b1000_0000;
                let res = x << 1;
                cpu.set_czn(res, carry);
                res
            }),
            Code::LSR => self.address_read_modify_write(a, min_cycles, "Shifted right", Some(ACCUMULATOR), &|cpu, x| {
                let carry = (x & 0b0000_0001) == 0b0000_0001;
                let res = x >> 1;
                cpu.set_czn(res, carry);
                res
            }),
            Code::ROL => self.address_read_modify_write(a, min_cycles, "Rotated left", Some(ACCUMULATOR), &|cpu, x| {
                let old_carry = cpu.status_register.get_flag_bit(StatusFlags::C);
                let next_carry = (x & 0b1000_0000) == 0b0000_0001;
                let res = (x << 1) + (old_carry >> StatusFlags::flag_position(StatusFlags::C));
                cpu.set_czn(res, next_carry);
                res
            }),
            Code::ROR => self.address_read_modify_write(a, min_cycles, "Rotated right", Some(ACCUMULATOR), &|cpu, x| {
                let old_carry = cpu.status_register.get_flag_bit(StatusFlags::C);
                let next_carry = x & 0b0000_0001 == 0b0000_0001;
                let res = (x >> 1) + old_carry;
                cpu.set_czn(res, next_carry);
                res
            }),
            //    fn bitwise(&mut self, a: Address, min_cycles: u16, op: &Fn(u8, u8) -> u8, msg: &str) -> CPUResult<String> {
            Code::AND => self.bitwise(a, min_cycles, &|acc, x| { acc & x }, "Logical AND"),
            Code::EOR => self.bitwise(a, min_cycles, &|acc, x| { acc ^ x }, "Logical EOR"),
            Code::ORA => self.bitwise(a, min_cycles, &|acc, x| { acc | x }, "Logical ORA"),

            Code::CMP => self.cmp(a, min_cycles, ACCUMULATOR),
            Code::CPX => self.cmp(a, min_cycles, X),
            Code::CPY => self.cmp(a, min_cycles, Y),
            
            Code::BIT => {
                let (bytes, extra_cycles, val) = self.address_read(a, None)?;
                let zero = self.axy_registers[ACCUMULATOR] & val == 0;
                self.status_register.bits |= val & 0b1100_0000;
                if zero {
                    self.set_flag_op(StatusFlags::Z)
                } else {
                    self.clear_flag_op(StatusFlags::Z)
                }
                self.counter += min_cycles + extra_cycles;
                self.pc += bytes;
                Ok(format!("BIT tested {:08b}", val))
            },
            
            Code::BCC => {
                let cond = self.status_register.get_flags_status(StatusFlags::C);
                self.branch(a, min_cycles, "BCC", !cond)
            },
            Code::BCS => {
                let cond = self.status_register.get_flags_status(StatusFlags::C);
                self.branch(a, min_cycles, "BCS", cond)
            },
            Code::BEQ => {
                let cond = self.status_register.get_flags_status(StatusFlags::Z);
                self.branch(a, min_cycles, "BEQ", cond)
            },
            Code::BNE => {
                let cond = self.status_register.get_flags_status(StatusFlags::Z);
                self.branch(a, min_cycles, "BNE", !cond)
            },
            Code::BMI => {
                let cond = self.status_register.get_flags_status(StatusFlags::N);
                self.branch(a, min_cycles, "BMI", cond)
            },
            Code::BPL => {
                let cond = self.status_register.get_flags_status(StatusFlags::N);
                self.branch(a, min_cycles, "BPL", !cond)
            },
            Code::BVC => {
                let cond = self.status_register.get_flags_status(StatusFlags::V);
                self.branch(a, min_cycles, "BVC", !cond)
            },
            Code::BVS => {
                let cond = self.status_register.get_flags_status(StatusFlags::V);
                self.branch(a, min_cycles, "BVS", cond)
            },
            
            Code::TAX => {
                let val = self.axy_registers[ACCUMULATOR];
                self.axy_registers[X] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred A to X"))
            },
            Code::TXA => {
                let val = self.axy_registers[X];
                self.axy_registers[ACCUMULATOR] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred X to A"))
            },
            Code::TAY => {
                let val = self.axy_registers[ACCUMULATOR];
                self.axy_registers[Y] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred A to Y"))

            },
            Code::TYA => {
                let val = self.axy_registers[Y];
                self.axy_registers[ACCUMULATOR] = val;
                self.set_zn(val);
                self.counter += min_cycles;
                Ok(format!("Transferred Y to A"))

            },
            Code::TSX => {
                let val = self.stack_pop()?;
                println!("Popped x ({:02X}) from stack", val);
                self.axy_registers[X] = val;
                self.set_zn(val);
                Ok(format!("Popped from the stack to X"))
            },
            Code::TXS => {
                let val = self.axy_registers[X];
                self.stack_push(val)?;
                println!("Pushed {:02X} to stack", val);
                self.counter += min_cycles;
                Ok(format!("Pushed x ({:02X}) to stack", val))
            },
            
            Code::PHA => {
                self.counter += min_cycles;
                let val = self.axy_registers[ACCUMULATOR];
                self.stack_push(val)?;
                println!("Pushed acc ({:02X}) to stack", val);
                Ok(format!("Pushed accumulator to stack"))
            },
            Code::PLA => {
                self.counter += min_cycles;
                let acc = self.stack_pop()?;
                self.set_zn(acc);
                self.axy_registers[ACCUMULATOR] = acc;
                println!("Pulled acc ({:02X}) from stack", acc);
                Ok(format!("Pulled accumulator from stack"))
            },
            Code::PHP => {
                self.counter += min_cycles;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)?;
                println!("Pushed flags ({:02X}) to stack", flags.bits);
                Ok(format!("Pushed status onto stack"))
            },
            Code::PLP => {
                self.counter += min_cycles;
                let flags = self.stack_pop()?;
                self.status_register.bits |= flags;
                println!("Popped flags ({:02X}) from stack", flags);
                Ok(format!("Pulled processor flags from stack"))
            },
            
            Code::JMP => {
                match a {
                    Specified(DoubleByte(Indirect)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        let real_addr = self.read_two_bytes(addr)?;
                        self.counter += min_cycles;
                        self.pc = real_addr;
                        Ok(format!("Set pc Indirect ${:04X} to ${:04X}", addr, real_addr))
                    },
                    Specified(DoubleByte(Absolute)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.counter += min_cycles;
                        self.pc = addr;
                        Ok(format!("Set pc to absolute ${:04X}", addr))
                    }
                    _ => Err(format!("JMP doesn't use {:?}", a))
                }
            },
            Code::JSR => {
                match a {
                    Specified(DoubleByte(Absolute)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.counter += min_cycles;
                        let ret_addr = pc + 2;
                        println!("Jumping to subroutine at {:04X} with ret addr {:04X}", addr, ret_addr);
                        self.stack_push_double(ret_addr)?;
                        self.pc = addr;
                        Ok(format!("Pushed ${:04X} onto stack and set pc to {:04X}.", ret_addr, addr))
                    },
                    _ => Err(format!("JSR doesn't use {:?}", a))
                }
            },
            Code::RTS => {
                let addr = self.stack_pop_double()?;
                self.pc = addr;
                self.counter += min_cycles;
                println!("returned to {:04X}", addr);
                Ok(format!("Returned pc to {:04X}", self.pc)) 
            },
            Code::RTI => {
                let flags = self.stack_pop()?;
                self.status_register.bits = flags;
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
                self.counter += min_cycles;
                Ok(format!("NOP"))
            },
            Code::BRK => {
                self.counter += min_cycles;
                //let addr = self.pc + 1; // Padding byte.
                let addr = self.pc;
                self.stack_push_double(addr)?;
                
                let irq = self.read_two_bytes(IRQ_VECTOR)?;
                self.pc = irq;
                    
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },
            Code::ILL => Err(format!("Illegal instruction {:02X}!", o))
        }     
    }

    #[inline(always)]
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

    #[inline(always)]
    fn set_czn(&mut self, val: u8, carry: bool) {
        self.set_zn(val);
        if carry {
            self.set_flag_op(StatusFlags::C)
        } else {
            self.clear_flag_op(StatusFlags::C)
        }
    }

    #[inline(always)]
    fn set_cznv(&mut self, val: u8, carry: bool, overflow: bool) {
        self.set_czn(val, carry);
        if overflow {
            self.set_flag_op(StatusFlags::V);
        } else {
            self.clear_flag_op(StatusFlags::V);
        }
    }

    #[inline(always)]
    fn stack_push(&mut self, val: u8) -> CPUResult<String> {
        let addr = (self.stack_pointer as u16) + STACK_REGION;
        let new_sp = self.stack_pointer.wrapping_sub(1);
        self.stack_pointer = new_sp;
        self.write(addr, val)
    }
    
    #[inline(always)]
    fn stack_push_double(&mut self, val: u16) -> CPUResult<String> {
        let (lower, upper) = split(val);
        self.stack_push(upper)?;
        self.stack_push(lower)
    }

    #[inline(always)]
    fn stack_pop(&mut self) -> CPUResult<u8> {
        let new_sp = self.stack_pointer.wrapping_add(1);
        let addr = (new_sp as u16)  + STACK_REGION;
        self.stack_pointer = new_sp;
        self.read(addr)
    }

    #[inline(always)]
    fn stack_pop_double(&mut self) -> CPUResult<u16> {
        let lower = self.stack_pop()?;
        let upper = self.stack_pop()?;
        let val = combine(lower, upper);
        Ok(val)
    }

    #[inline(always)]
    fn load(&mut self, a: Address, min_cycles: u16, r: usize) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        self.set_zn(val);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        self.axy_registers[r] = val;
        Ok(format!("Loaded ({:08b}) into axy register {}", val, r))
    }

    #[inline(always)]
    fn inc(&mut self, a: Address, min_cycles: u16, implied: Option<usize>) -> CPUResult<String> {
        let msg = "Incremented ";
        self.address_read_modify_write(a, min_cycles, msg, implied, &|cpu, x| {
            let res = x.wrapping_add(1);
            cpu.set_zn(res);
            res
        })
    }

    #[inline(always)]
    fn dec(&mut self, a: Address, min_cycles: u16, implied: Option<usize>) -> CPUResult<String> {
       let msg = "Decremented ";
       self.address_read_modify_write(a, min_cycles, msg, implied, &|cpu, x| {
           let res = x.wrapping_sub(1);
           cpu.set_zn(res);
           res
        })
    }

    #[inline(always)]
    fn add(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None)?;
        let carry = self.status_register.get_flag(StatusFlags::C).bits;
        let next_carry = u8::MAX - rhs - carry > self.axy_registers[ACCUMULATOR] || (rhs == 255 && carry == 1);
        let res = self.axy_registers[ACCUMULATOR].wrapping_add(rhs.wrapping_add(carry));
        let overflow = ((self.axy_registers[ACCUMULATOR] ^ res) & 0x80) != 0;
        self.set_cznv(res, next_carry, overflow);
        self.axy_registers[ACCUMULATOR] = res;
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Added {} to accumulator for {} with carry {}",
                   rhs.wrapping_add(carry), res, next_carry))
    }

    #[inline(always)]
    fn sub(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a, None)?;
        let carry = self.status_register.get_flag(StatusFlags::C).bits;
        let next_carry = self.axy_registers[ACCUMULATOR] < (rhs + carry) || (rhs == 255 && carry == 1);
        let res = self.axy_registers[ACCUMULATOR].wrapping_sub(rhs.wrapping_add(carry));
        let overflow = ((self.axy_registers[ACCUMULATOR] ^ (255 - res)) & 0x80) != 0;
        self.set_cznv(res, !next_carry, overflow);
        self.axy_registers[ACCUMULATOR] = res;
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Subtracted {} from accumulator for result {} with carry: {} and overflow: {} ",
                   rhs, res, next_carry, overflow))
    }

    #[inline(always)]
    fn set_flag_op(&mut self, flag: StatusFlags) {
        self.status_register |= flag;
    }

    #[inline(always)]
    fn clear_flag_op(&mut self, flag: StatusFlags) {
        self.status_register &= !flag;
    }

    #[inline(always)]
    fn set_flag(&mut self, min_cycles: u16, flag: StatusFlags) -> CPUResult<String> {
        self.counter += min_cycles;
        self.set_flag_op(flag);
        Ok(format!("Set {:?}", flag))
    }

    #[inline(always)]
    fn clear_flag(&mut self, min_cycles: u16, flag: StatusFlags) -> CPUResult<String> {
        self.counter += min_cycles;
        self.clear_flag_op(flag);
        Ok(format!("Cleared {:?}", flag))
    }

    #[inline(always)]
    fn branch(&mut self, a: Address, min_cycles: u16, msg: &str, cond: bool) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        if cond {
            self.modify_pc_counter(bytes, min_cycles + 1 + 2 * extra_cycles);
            if val > 127 {
                let addr = -((val as i8) as i16) as u16;
                self.pc -= addr;
                Ok(format!("{}: Branched by -{}", msg, addr))
            } else {
                self.pc += val as u16;
                Ok(format!("{}: Branched by {}", msg, val))
            }
        } else {
            self.modify_pc_counter(bytes, min_cycles);
            Ok(format!("{}: No branch.", msg))
        }
    }

    #[inline(always)]
    fn cmp(&mut self, a: Address, min_cycles: u16, lhs_idx: usize) -> CPUResult<String> {
        let lhs = self.axy_registers[lhs_idx];
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        let res = lhs.wrapping_sub(val);
        let carry = lhs < val;
        self.set_czn(res, carry);
        self.modify_pc_counter(bytes, min_cycles + extra_cycles);
        Ok(format!("Compared {} to {} for result {:?}", lhs, val, self.status_register))
    }

    #[inline(always)]
    fn bitwise(&mut self, a: Address, min_cycles: u16, op: &Fn(u8, u8) -> u8, msg: &str) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a, None)?;
        let acc = self.axy_registers[ACCUMULATOR];
        let res = op(acc, val);
        self.axy_registers[ACCUMULATOR] = res;
        self.set_zn(res);
        self.modify_pc_counter(bytes, min_cycles);
        Ok(format!("{} by {:02X}", msg, val))
    }

    #[inline(always)]
    fn modify_pc_counter(&mut self, bytes: u16, cycles: u16) {
        self.pc += bytes;
        self.counter += cycles;
    }
}
