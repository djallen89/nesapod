use std::{u8, u16};
use core::ppu::PPU;
use core::ines::INES;
use core::addressing::{opcode_table, Address, AddressType, SingleType, DoubleType};
use core::addressing::Address::*;
use core::addressing::AddressType::*;
use core::addressing::SingleType::*;
use core::addressing::DoubleType::*;

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const STACK_REGION: u16 = 0x01A0;

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

pub fn split(u: u16) -> (u8, u8) {
    let lower = (u & 0x0F) as u8;
    let upper = (u >> 8) as u8;
    (lower, upper)
}

pub fn combine(lower: u8, upper: u8) -> u16 {
    ((upper as u16) << 8) + (lower as u16)
}

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
    ILLEGAL
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    mnemonic: Code,
    address: Address
}

pub struct CPU {
    opcode_table: Vec<(Code, Address, u16)>,
    counter: u16,
    pc: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status_register: StatusFlags,
    ram: [u8; 2048],
    ppu: PPU,
    cartridge: INES
}

impl CPU {
    pub fn power_up(ines: INES) -> CPU {
        let pc = RESET_VECTOR;
        CPU {
            opcode_table: opcode_table(),
            counter: 0,
            pc: pc,
            stack_pointer: POWERUP_S,
            accumulator: 0,
            x: 0,
            y: 0,
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 2048],
            ppu: PPU::init(),
            cartridge: ines
        }
    }

    pub fn _reset(&mut self) {
        self.stack_pointer -= 3;
        self.status_register = self.status_register | StatusFlags::I;
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
    }

    pub fn init(&mut self) -> CPUResult<String> {
        self.execute(Code::JMP, Address::Specified(AddressType::DoubleByte(DoubleType::Indirect)), 5)
    }

    pub fn get_pc(&self) -> u16 {
        self.pc
    }

    pub fn get_counter(&self) -> u16 {
        self.counter
    }

    pub fn step(&mut self) -> CPUResult<String> {
        let pc = self.pc;
        let (code, address, cycles) = self.opcode_table[self.read(pc)? as usize];
        self.pc += 1;
        self.execute(code, address, cycles)
    }
    
    pub fn read(&mut self, address: u16) -> CPUResult<u8> {
        match address {
            0x0000 ... 0x1FFF => Ok(self.ram[(address % 2048) as usize]),
            0x2000 ... 0x3FFF => Ok(self.ppu.read(address)),
            0x4000 ... 0x4017 => Err(format!("APU and IO not yet implemented!")),
            0x4018 ... 0x401F => Err(format!("CPU test mode not yet implemnted!")),
            0x4020 ... 0xFFFF => self.cartridge.read(address),
            _ => Err(format!("I dunno lol"))
        }
    }

    pub fn read_two_bytes(&mut self, addr: u16) -> CPUResult<u16> {
        let lower = self.read(addr)?;
        let upper = self.read(addr + 1)?;
        Ok(combine(lower, upper))
    }
    
    pub fn write(&mut self, address: u16, val: u8) -> CPUResult<String> {
        match address as usize {
            0x0000 ... 0x1FFF => {
                self.ram[(address % 2048) as usize] = val;
                Ok(format!("Wrote {:x} to address {:x}", val, address))
            },
            0x2000 ... 0x3FFF => {
                self.ppu.write(address, val);
                Ok(format!("Wrote {:x} to ppu {:x}", val, address))
            },
            0x4000 ... 0x4017 => Err(format!("APU and IO not yet implemented!")),
            0x4018 ... 0x401F => Err(format!("CPU test mode not yet implemnted!")),
            0x4020 ... 0xFFFF => self.cartridge.write(address, val),
            _ => Err(format!("I dunno lol"))
        }
    }

    #[inline(always)]
    fn decode_single_byte(&mut self, s: SingleType) -> CPUResult<(u16, u16)> {
        let addr = self.read(self.pc)?;
        match s {
            ZeroPg => Ok((0, addr as u16)),
            ZeroPg_X => Ok((0, self.x.wrapping_add(addr) as u16)),
            ZeroPg_Y => Ok((0, (self.y as u16) + (addr as u16))),
            Indirect_X => Ok((0, self.read_two_bytes(self.x.wrapping_add(addr) as u16)?)),
            Indirect_Y => {
                let pre_addr = self.read_two_bytes(addr as u16)?;
                let extra = counter_inc(pre_addr, self.y);
                let real_addr = (self.y as u16) + pre_addr;
                Ok((extra, real_addr))
            },
            _ => panic!(format!("{:?} is not supported in decode_singlebyte", s))
        }
    }

    fn decode_double_byte(&mut self, d: DoubleType) -> CPUResult<(u16, u16)> {
        let addr = self.read_two_bytes(self.pc)?;
        match d {
            Absolute => Ok((0, addr)),
            Absolute_Y => {
                let extra = counter_inc(addr, self.x);
                Ok((extra, (self.x as u16) + addr))
            },
            Absolute_X => {
                let extra = counter_inc(addr, self.y);
                Ok((extra, (self.y as u16) + addr))
            },
            Indirect => {
                let real_addr = self.read_two_bytes(addr)?;
                Ok((0, real_addr))
            }
        }
    }

    /// Returns the number of bytes of an instruction, if it takes an extra cycle by
    /// crossing a page and the value stored at that location (excepting immediate and
    /// relative instructions).
    fn address_read(&mut self, a: Address) -> CPUResult<(u16, u16, u8)> {
        use core::addressing::SingleType::{Relative, Immediate};
        let (bytes, extra_cycles, val) = match a {
            Invalid | Implied => panic!("Improper use of address_read!"),
            Acc => (0, 0, self.accumulator),
            Specified(SingleByte(Relative)) | 
            Specified(SingleByte(Immediate)) => (1, 0, self.read(self.pc)?),
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

    fn address_read_modify_write(&mut self, a: Address, op: &Fn(u8) -> u8) -> CPUResult<(u16, u8)> {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;
        
        match a {
            Invalid | Implied |
            Specified(DoubleByte(Indirect)) |
            Specified(SingleByte(Relative)) |
            Specified(SingleByte(Immediate)) => panic!("Don't use addres_read_modify_write for {:?}", a),                
            Acc => {
                let val = op(self.accumulator);
                self.accumulator = val;
                Ok((0, val))
            },
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s)?;
                let val = op(self.read(addr)?);
                self.write(addr, val)?;
                Ok((1, val))
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d)?;
                let val = op(self.read(addr)?);
                self.write(addr, val)?;
                Ok((2, val))
            }
        }
    }

    /// Returns the number of bytes of the instruction, then the address to which the result
    ///of an operation will be written
    fn address_write(&mut self, a: Address) -> CPUResult<(u16, u16)> {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;

        match a {
            Acc |
            Invalid | 
            Implied |
            Specified(SingleByte(Immediate)) |
            Specified(SingleByte(Relative)) |
            Specified(DoubleByte(Indirect)) => panic!("Improper use of address_write!"),
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s)?;
                Ok((1, addr))
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d)?;
                Ok((2, addr))
            }
        }
    }

    fn execute(&mut self, c: Code, a: Address, min_cycles: u16) -> CPUResult<String> {
        use self::Code::*;
        match c {
            LDA => self.load(a, min_cycles, &mut self.accumulator),
            LDX => self.load(a, min_cycles, &mut self.x),
            LDY => self.load(a, min_cycles, &mut self.y),
            STA => self.store(a, min_cycles, self.accumulator),
            STX => self.store(a, min_cycles, self.x),
            STY => self.store(a, min_cycles, self.y),
            /*ADC => {
            },
            SBC => {
            },*/
            INC => self.inc(a, min_cycles),
            INX => self.inc_reg(min_cycles, &mut self.x),
            INY => self.inc_reg(min_cycles, &mut self.y),
            DEC => self.dec(a, min_cycles),
            DEX => self.dec_reg(min_cycles, &mut self.x),
            DEY => self.dec_reg(min_cycles, &mut self.y),
            /*ASL => {
            },
            LSR => {
            },
            ROL => {
            },
            ROR => {
            },
            AND => {
            },
            ORA => {
            },
            EOR => {
            },
            CMP => {
            },
            CPX => {
            },
            CPY => {
            },
            BIT => {
            },
            BCC => {
            },
            BCS => {
            },
            BEQ => {
            },
            BMI => {
            },
            BNE => {
            },
            BPL => {
            },
            BVC => {
            },
            BVS => {
            },
            TAX => {
            },
            TXA => {
            },
            TAY => {
            },
            TYA => {
            },
            TSX => {
            },
            TXS => {
            },
            PHA => {
            },
            PLA => {
            },
            PHP => {
            },
            PLP => {
            },
            JMP => {
            },
            JSR => {
            },
            RTS => {
            },
            RTI => {
            },
            SEC => {
            },
            SED => {
            },
            SEI => {
            },
            CLC => {
            },
            CLD => {
            },
            CLI => {
            },
            CLV => {
            },
            NOP => {
            },
            BRK => {
            },*/
            ILLEGAL | _ => panic!("Illegal instruction!")
        }        
    }
        
    fn set_zn(&mut self, val: u8) {
        print!("{:?} -> ", self.status_register);
        if val == 0 {
            self.status_register |= StatusFlags::Z;
            self.status_register &= !StatusFlags::N;
        } else if val > 127 {
            self.status_register |= StatusFlags::N;
            self.status_register &= !StatusFlags::Z;
        } else {
            self.status_register &= !StatusFlags::N;
            self.status_register &= !StatusFlags::Z;
        }
        println!("{:?}", self.status_register);
    }

    fn stack_decrement(&mut self) -> CPUResult<()> {
        if self.stack_pointer == 0 {
            Err(format!("Stack overflow!"))
        } else {
            self.stack_pointer -= 1;
            Ok(())
        }
    }

    fn stack_increment(&mut self) -> CPUResult<()> {
        if self.stack_pointer == 255 {
            Err(format!("Stack underflow!"))
        } else {
            self.stack_pointer += 1;
            Ok(())
        }
    }
    
    fn stack_push(&mut self, val: u8) -> CPUResult<String> {
        let addr = (self.stack_pointer as u16) + STACK_REGION;
        match self.write(addr, val) {
            Ok(s) => {
                self.stack_decrement()?;
                Ok(s)
            },
            Err(f) => Err(f)
        }
    }

    fn stack_push_double(&mut self, val: u16) -> CPUResult<String> {
        let (lower, upper) = split(val);
        self.stack_push(upper)?;
        self.stack_push(lower)
    }

    fn stack_pop(&mut self) -> CPUResult<u8> {
        let addr = (self.stack_pointer as u16) + STACK_REGION;
        match self.read(addr) {
            Ok(u) => {
                self.stack_increment()?;
                Ok(u)
            },
            Err(f) => Err(f)
        }
    }

    fn stack_pop_double(&mut self) -> CPUResult<u16> {
        let lower = self.stack_pop()?;
        let upper = self.stack_pop()?;
        let val = combine(lower, upper);
        Ok(val)
    }

    fn load(&mut self, a: Address, min_cycles: u16, dest: &mut u8) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a)?;
        self.set_zn(val);
        self.pc += bytes;
        self.counter += min_cycles + extra_cycles;
        *dest = val;
        Ok(format!("Loaded {:b} into a register", val))
    }

    fn store(&mut self, a: Address, min_cycles: u16, val: u8) -> CPUResult<String> {
        let (bytes, addr) = self.address_write(a)?;
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += min_cycles;
        Ok(format!("Stored {} into {:?}", val, a))
    }

    fn inc(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, val) = self.address_read_modify_write(a, &|x| {x.wrapping_add(1)})?;
        self.pc += bytes;
        self.counter += min_cycles;
        self.set_zn(val);
        Ok(format!("Incremented {:?}", a))
    }

    fn inc_reg(&mut self, min_cycles: u16, dest: &mut u8) -> CPUResult<String> {
        let val = dest.wrapping_add(1);
        *dest = val;
        self.counter += min_cycles;
        self.set_zn(val);
        Ok(format!("Incremented x or y by one"))
    }

    fn dec(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, val) = self.address_read_modify_write(a, &|x| {x.wrapping_sub(1)})?;
        self.pc += bytes;
        self.counter += min_cycles;
        self.set_zn(val);
        Ok(format!("Decremented {:?}", a))
    }
    
    fn dec_reg(&mut self, min_cycles: u16, dest: &mut u8) -> CPUResult<String> {
        let val = dest.wrapping_sub(1);
        *dest = val;
        self.counter += min_cycles;
        self.set_zn(val);
        Ok(format!("Decremented x or y by one"))
    }

    fn lsr_acc(&mut self) -> CPUResult<String> {
        let carry = self.accumulator & 0b0000_0001;
        let mut val = self.accumulator;
        val >>= 1;
        self.set_zn(val);
        self.status_register.bits |= carry;
        self.accumulator = val;
        self.pc += 1;
        self.counter += 2;
        Ok(format!("Shifted Accumulator right for result {:x}", self.accumulator))
    }

    fn lsr_mem(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, addr) = self.address_write(a)?;
        let mut val = self.read(addr)?;
        let carry = val & 0b0000_0001;
        val >>= 1;
        self.set_zn(val);
        self.status_register.bits |= carry;
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += min_cycles;
        Ok(format!("Shifted {:x} right for result {:x}", addr, val))
    }

    /*
    fn _execute(&mut self, instruction: Instruction) -> CPUResult<String> {
        match (instruction.mnemonic, instruction.address) {
            /* INC */ 
            (Code::INC, a) => self.inc(a),
            /*INX, INY */
            (m @ Code::INX, Address::Implied) |
            (m @ Code::INY, Address::Implied) => self.inc_reg(m),
            /* DEC, DEX, DEY, */
            (Code::DEC, a) => self.dec(a),
            (m @ Code::DEX, Address::Implied) |
            (m @ Code::DEY, Address::Implied) => self.dec_reg(m),
            /* ASL, LSR */
            (Code::LSR, Address::Accumulator) => self.lsr_acc(),
            (Code::LSR, a) => self.lsr_mem(a),
            /* ROL, ROR */
            
            /* AND, ORA, EOR, */
            (c @ Code::AND, a) | (c @ Code::ORA, a) | (c @ Code::EOR, a) => {
                let (bytes, cycles, val) = self.decode_address_read(a)?;
                self.pc += bytes;
                self.counter += cycles;
                let msg = match c {
                    Code::AND => {
                        self.accumulator &= val;
                        format!("Logical and {:x}", val)
                    },
                    Code::EOR => {
                        self.accumulator ^= val;
                        format!("Exclusive or {:x}", val)
                    },
                    _ => {
                        self.accumulator |= val;
                        format!("Logical or {:x}", val)
                    }
                };
                let res = self.accumulator;
                self.set_zn(res);
                Ok(msg)
            },
            /* CMP, CPX, CPY, */
            (c @ Code::CMP, a) | (c @ Code::CPX, a) | (c @ Code::CPY, a) => {
                let (bytes, cycles, val) = self.decode_address_read(a)?;
                let lhs = if c == Code::CMP {
                    self.accumulator
                } else if c == Code::CPX {
                    self.x
                } else {
                    self.y
                };
                if lhs < val {
                    self.status_register |= StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                    self.status_register &= !StatusFlags::C;
                } else if lhs == val {
                    self.status_register |= StatusFlags::Z;
                    self.status_register |= StatusFlags::C;
                    self.status_register &= !StatusFlags::N;
                } else {
                    self.status_register &= !StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                    self.status_register |= StatusFlags::C;
                }
                self.counter += cycles;
                self.pc += bytes;
                Ok(format!("Compared register ({}) to {}", lhs, val))
            },
            /* BIT, */
            
            /* BCC, BCS, BMI, */
            /* BNE, BEQ, BPL */ 
            (c @ Code::BNE, Address::Relative(d)) |
            (c @ Code::BEQ, Address::Relative(d)) |
            (c @ Code::BPL, Address::Relative(d)) => {
                print!("{:?}, Flags: {:?}, ", c, self.status_register);
                print!("{:?}, ", (self.status_register & StatusFlags::Z) == StatusFlags::Z);
                println!("{:?}", self.status_register & StatusFlags::N);
                let cond = if c == Code::BEQ {
                    (self.status_register & StatusFlags::Z) == StatusFlags::Z
                } else if c == Code::BNE {
                    (self.status_register & StatusFlags::Z) != StatusFlags::Z
                } else {
                    (self.status_register & StatusFlags::N) != StatusFlags::N
                };
                if cond {
                    self.counter += 3 + 2 * (counter_inc(self.pc, d as u8) as u16);
                    println!("{}", self.pc);
                    if d < 0 {
                        self.pc -= (-d) as u16
                    } else {
                        self.pc += d as u16;
                    }
                    println!("{}", self.pc);
                    Ok(format!("Branched by {}", d))
                } else {
                    self.counter += 2;
                    self.pc += 2;
                    Ok(format!("No branch."))
                }
            },
            /* BVC, BVS, */
            /* TAX, TXA, TAY, TYA, TSX, TXS, */
            (Code::TXS, Address::Implied) => {
                let val = self.x;
                self.stack_push(val)?;
                self.pc += 1;
                self.counter += 2;
                Ok(format!("Transferred x to stack"))
            }
            /* PHA */ 
            (Code::PHA, Address::Implied) => {
                self.counter += 3;
                self.pc += 1;
                let val = self.accumulator;
                self.stack_push(val)
            },
            /* PHP */
            (Code::PHP, Address::Implied) => {
                self.counter += 2;
                self.pc += 1;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },
            /* PLA */
            (Code::PLA, Address::Implied) => {
                self.counter += 4;
                self.pc += 1;
                let acc = self.stack_pop()?;
                self.set_zn(acc);
                self.accumulator = acc;
                Ok(format!("Pulled accumulator from stack!"))
            }
            /* PLP, */
            /* JMP */
            (Code::JMP, Address::Absolute(idx)) => {
                self.counter += 3;
                self.pc = idx;
                Ok(format!("Set pc to {:x}", idx))
            },
            (Code::JMP, Address::Indirect(idx)) => {
                self.counter += 5;
                let addr = self.read_double(idx)?;
                self.pc = addr;
                Ok(format!("Set pc to {:x}", addr))
            },
            /*JSR */
            (Code::JSR, Address::Absolute(idx)) => {
                self.counter += 6;
                let ret_addr = self.pc + 2;
                self.stack_push_double(ret_addr)?;
                self.pc = idx;
                Ok(format!("Pushed {:x} onto stack and set pc to {:x}.", ret_addr, idx))
            },
            /*RTS, RTI, */
            (Code::RTS, Address::Implied) => {
                let addr = self.stack_pop_double()?;
                self.pc = addr + 1;
                self.counter += 6;
                Ok(format!("Returned pc to {:x}", self.pc)) 
            },
            (Code::RTI, Address::Implied) => {
                let flags = self.stack_pop()?;
                println!("{}", flags);
                self.status_register.bits = flags;
                let addr = self.stack_pop_double()?;
                self.pc = addr;
                self.counter += 6;
                Ok(format!("Set pc to {:x} from interrupt", self.pc))
            }
            /* SEC, SED, SEI,*/
            (Code::SEI, Address::Implied) => {
                self.counter += 2;
                self.pc += 1;
                self.status_register |=  StatusFlags::I;
                Ok(format!("Set Interrupt Disable to 1"))
            }
            /* CLC, CLD, CLI, CLV, */
            (Code::CLD, Address::Implied) => {
                self.counter += 2;
                self.pc += 1;
                self.status_register &= !StatusFlags::D;
                Ok(format!("Cleared decimal flag"))
            }
            /* NOP, BRK */
            (Code::BRK, Address::Implied) => {
                if self.stack_pointer < 2 {
                    Err(format!("Stack overflowed!"))
                } else {
                    self.counter += 7;
                    let addr = self.pc;
                    self.stack_push_double(addr)?;
                    
                    let irq = self.read_double(0xFFFE)?;
                    self.pc = irq;
                    
                    let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                    self.stack_push(flags.bits)
                }
            },
            _ => Err(format!("{:?} not implemented", instruction))
        }
    }*/
}
