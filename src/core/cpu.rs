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

impl StatusFlags {
    pub fn get_flag(&self, flag: StatusFlags) -> StatusFlags {
        *self & flag
    }
    
    pub fn get_flags_status(&self, flag: StatusFlags) -> bool {
        self.get_flag(flag) == flag
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
        let addr = self.pc;
        let opcode = self.read(addr)? as usize;
        print!("{:X}, opcode: {:X}, ", self.pc, opcode);
        let (code, address, cycles) = self.opcode_table[opcode];
        println!("{:?}", code);
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
                Ok(format!("Wrote {:X} to address {:X}", val, address))
            },
            0x2000 ... 0x3FFF => {
                self.ppu.write(address, val);
                Ok(format!("Wrote {:X} to ppu {:X}", val, address))
            },
            0x4000 ... 0x4017 => Err(format!("APU and IO not yet implemented!")),
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
            ZeroPg => Ok((0, addr as u16)),
            ZeroPg_X => Ok((0, self.x.wrapping_add(addr) as u16)),
            ZeroPg_Y => Ok((0, (self.y as u16) + (addr as u16))),
            Indirect_X => {
                let real_addr = self.x.wrapping_add(addr) as u16;
                Ok((0, self.read_two_bytes(real_addr)?))
            },
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
        let pc = self.pc;
        let addr = self.read_two_bytes(pc)?;
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
            Indirect => panic!("No indirect in decode double byte!")
        }
    }

    /// Returns the number of bytes of an instruction, if it takes an extra cycle by
    /// crossing a page and the value stored at that location (excepting immediate and
    /// relative instructions).
    fn address_read(&mut self, a: Address) -> CPUResult<(u16, u16, u8)> {
        let pc = self.pc;
        let (bytes, extra_cycles, val) = match a {
            Invalid | Implied => panic!("Improper use of address_read!"),
            Acc => (0, 0, self.accumulator),
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

    fn address_read_modify_write(&mut self, a: Address,
                                 op: &Fn(&mut CPU, u8) -> u8) -> CPUResult<(u16, u8)> {
        use core::addressing::Address::*;
        use core::addressing::AddressType::*;
        use core::addressing::SingleType::*;
        use core::addressing::DoubleType::*;
        
        match a {
            Invalid | Implied |
            Specified(DoubleByte(Indirect)) |
            Specified(SingleByte(Relative)) |
            Specified(SingleByte(Immediate)) => panic!("Don't use read modify write for {:?}", a),
            Acc => {
                let val = self.accumulator;
                let res = op(self, val);
                self.accumulator = res;
                Ok((0, val))
            },
            Specified(SingleByte(s)) => {
                let (_, addr) = self.decode_single_byte(s)?;
                let val = self.read(addr)?;
                let res = op(self, val);
                self.write(addr, res)?;
                Ok((1, val))
            },
            Specified(DoubleByte(d)) => {
                let (_, addr) = self.decode_double_byte(d)?;
                let val = self.read(addr)?;
                let res = op(self, val);
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
            LDA | LDX | LDY => self.load(a, min_cycles, c),
            
            STA => {
                let val = self.accumulator;
                self.store(a, min_cycles, val)
            },
            STX => {
                let val = self.x;
                self.store(a, min_cycles, val)
            },
            STY => {
                let val = self.y;
                self.store(a, min_cycles, val)
            },
            
            ADC => self.add(a, min_cycles),
            SBC => self.sub(a, min_cycles),
            
            INC => self.inc(a, min_cycles),
            INX | INY => self.inc_reg(min_cycles, c),
            DEC => self.dec(a, min_cycles),
            DEX | DEY => self.dec_reg(min_cycles, c),
            
            ASL => self.shift_rotate(a, min_cycles, &|cpu, x| {
                let carry = x & 0x01;
                cpu.status_register.bits |= carry;
                x >> 1
            }),
            LSR => self.shift_rotate(a, min_cycles, &|cpu, x| {
                let carry = (x & 0b1000_0000) >> 7;
                cpu.status_register.bits |= carry;
                x << 1
            }),
            ROL => self.shift_rotate(a, min_cycles, &|cpu, x| {
                let old_carry = (cpu.status_register & StatusFlags::C).bits;
                let next_carry = x & 0b1000_0000;
                cpu.status_register.bits |= next_carry >> 7;
                (x << 1) + old_carry
            }),
            ROR => self.shift_rotate(a, min_cycles, &|cpu, x| {
                let old_carry = (cpu.status_register & StatusFlags::C).bits;
                let next_carry = x & 0x01;
                cpu.status_register.bits |= next_carry;
                (x >> 1) + old_carry 
            }),
            
            c @ AND | c @ ORA | c @ EOR => {
                let (bytes, extra_cycles, val) = self.address_read(a)?;
                self.pc += bytes;
                self.counter += min_cycles + extra_cycles;
                let msg = match c {
                    AND => {
                        self.accumulator &= val;
                        format!("Logical AND result: {:X}", val)
                    },
                    EOR => {
                        self.accumulator ^= val;
                        format!("Logical EOR result: {:X}", val)
                    },
                    ORA | _ => {
                        self.accumulator |= val;
                        format!("Logical ORA result: {:X}", val)
                    }
                };
                let res = self.accumulator;
                self.set_zn(res);
                Ok(msg)
            },

            c @ CMP | c @ CPX | c @ CPY => {
                let (bytes, extra_cycles, val) = self.address_read(a)?;
                let lhs = if c == Code::CMP {
                    self.accumulator
                } else if c == Code::CPX {
                    self.x
                } else {
                    self.y
                };
                if lhs < val {
                    self.status_register &= !StatusFlags::C;
                    self.status_register |= StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                } else if lhs == val {
                    self.status_register |= StatusFlags::C;
                    self.status_register &= !StatusFlags::N;
                    self.status_register |= StatusFlags::Z;
                } else {
                    self.status_register |= StatusFlags::C;
                    self.status_register &= !StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                }
                self.counter += min_cycles + extra_cycles;
                self.pc += bytes;
                Ok(format!("Compared {} to {}", lhs, val))
            },
            
            /*BIT => {},*/
            BCC => {
                let cond = self.status_register.get_flags_status(StatusFlags::C);
                self.branch(a, min_cycles, !cond)
            },
            BCS => {
                let cond = self.status_register.get_flags_status(StatusFlags::C);
                self.branch(a, min_cycles, cond)
            },
            BEQ => {
                let cond = self.status_register.get_flags_status(StatusFlags::Z);
                self.branch(a, min_cycles, cond)
            },
            BNE => {
                let cond = self.status_register.get_flags_status(StatusFlags::Z);
                self.branch(a, min_cycles, !cond)
            },
            BMI => {
                let cond = self.status_register.get_flags_status(StatusFlags::N);
                self.branch(a, min_cycles, cond)
            },
            BPL => {
                let cond = self.status_register.get_flags_status(StatusFlags::N);
                self.branch(a, min_cycles, !cond)
            },
            BVC => {
                let cond = self.status_register.get_flags_status(StatusFlags::V);
                self.branch(a, min_cycles, !cond)
            },
            BVS => {
                let cond = self.status_register.get_flags_status(StatusFlags::V);
                self.branch(a, min_cycles, cond)
            },
            /*TAX => {
            },
            TXA => {
            },
            TAY => {
            },
            TYA => {
            },
            TSX => {
            },*/
            TXS => {
                let val = self.x;
                self.stack_push(val)?;
                self.counter += min_cycles;
                Ok(format!("Transferred x to stack"))
            },
            PHA => {
                self.counter += min_cycles;
                let val = self.accumulator;
                self.stack_push(val)
            },
            PLA => {
                self.counter += min_cycles;
                let acc = self.stack_pop()?;
                self.set_zn(acc);
                self.accumulator = acc;
                Ok(format!("Pulled accumulator from stack!"))
            },
            PHP => {
                self.counter += min_cycles;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },
            /*PLP => {
            },*/
            JMP => {
                match a {
                    Specified(DoubleByte(Indirect)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.counter += min_cycles;
                        self.pc = addr;
                        Ok(format!("Set pc to {:X}", addr))
                    },
                    Specified(DoubleByte(Absolute)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.counter += min_cycles;
                        self.pc = addr;
                        Ok(format!("Set pc to {:X}", addr))
                    }
                    _ => Err(format!("JMP doesn't use {:?}", a))
                }
            },
            JSR => {
                match a {
                    Specified(DoubleByte(Absolute)) => {
                        let pc = self.pc;
                        let addr = self.read_two_bytes(pc)?;
                        self.counter += min_cycles;
                        let ret_addr = self.pc + 2 - 1;
                        self.stack_push_double(ret_addr)?;
                        self.pc = addr;
                        Ok(format!("Pushed {:X} onto stack and set pc to {:X}.", ret_addr, addr))
                    },
                    _ => Err(format!("JSR doesn't use {:?}", a))
                }
            },
            RTS => {
                let addr = self.stack_pop_double()?;
                self.pc = addr;
                self.counter += min_cycles;
                Ok(format!("Returned pc to {:X}", self.pc)) 
            },
            RTI => {
                let flags = self.stack_pop()?;
                self.status_register.bits = flags;
                let addr = self.stack_pop_double()?;
                self.pc = addr + 1;
                self.counter += min_cycles;
                Ok(format!("Set pc to {:X} from interrupt", self.pc))
            },
            SEC => self.set_flag(min_cycles, StatusFlags::C),
            SED => self.set_flag(min_cycles, StatusFlags::D),
            SEI => self.set_flag(min_cycles, StatusFlags::I),
            CLC => self.clear_flag(min_cycles, StatusFlags::C),
            CLD => self.clear_flag(min_cycles, StatusFlags::D),
            CLI => self.clear_flag(min_cycles, StatusFlags::I),
            CLV => self.clear_flag(min_cycles, StatusFlags::V),
            /*NOP => {
            },*/
            BRK => {
                if self.stack_pointer < 2 {
                    Err(format!("Stack overflowed!"))
                } else {
                    self.counter += 7;
                    let addr = self.pc;
                    self.stack_push_double(addr)?;
                    
                    let irq = self.read_two_bytes(0xFFFE)?;
                    self.pc = irq;
                    
                    let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                    self.stack_push(flags.bits)
                }

            },
            ILLEGAL | _ => Err(format!("Illegal instruction {:?}!", c))
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

    fn set_cznv(&mut self, val: u8, carry: bool, overflow: bool) {
        self.set_zn(val);
        if carry {
            self.set_flag_op(StatusFlags::C);
        } else {
            self.clear_flag_op(StatusFlags::C);
        }

        if overflow {
            self.set_flag_op(StatusFlags::V);
        } else {
            self.clear_flag_op(StatusFlags::V);
        }
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

    fn load(&mut self, a: Address, min_cycles: u16, c: Code) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a)?;
        self.set_zn(val);
        self.pc += bytes;
        self.counter += min_cycles + extra_cycles;
        match c {
            LDA => self.accumulator = val,
            LDX => self.x = val,
            LDY => self.y = val,
            _ => panic!(format!("Expected LDx, found {:?}", c))
        }
        Ok(format!("Loaded {:b} into a register", val))
    }

    fn store(&mut self, a: Address, min_cycles: u16, val: u8) -> CPUResult<String> {
        let (bytes, addr) = self.address_write(a)?;
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += min_cycles;
        Ok(format!("Stored {} into {:?}", val, addr))
    }

    fn inc(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, val) = self.address_read_modify_write(a, &|cpu, x| {x.wrapping_add(1)})?;
        self.pc += bytes;
        self.counter += min_cycles;
        self.set_zn(val);
        Ok(format!("Incremented {:?}", a))
    }

    fn inc_reg(&mut self, min_cycles: u16, c: Code) -> CPUResult<String> {
        match c {
            INX => {
                let val = self.x.wrapping_add(1);
                self.counter += min_cycles;
                self.set_zn(val);
                self.x = val;
                Ok(format!("Incremented x by one for res {}", val))
            },
            INY => {
                let val = self.y.wrapping_add(1);
                self.counter += min_cycles;
                self.set_zn(val);
                self.y = val;
                Ok(format!("Incremented x by one for res {}", val))
            },
            _ => panic!(format!("Expected INX/INY, found {:?}", c))
        }
    }

    fn dec(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, val) = self.address_read_modify_write(a, &|cpu, x| {
            x.wrapping_sub(1)
        })?;
        self.set_zn(val);
        self.pc += bytes;
        self.counter += min_cycles;
        Ok(format!("Decremented {:?}", a))
    }
    
    fn dec_reg(&mut self, min_cycles: u16, c: Code) -> CPUResult<String> {
        match c {
            DEX => {
                let val = self.x.wrapping_sub(1);
                self.counter += min_cycles;
                self.set_zn(val);
                self.x = val;
                Ok(format!("Incremented x by one"))
            },
            DEY => {
                let val = self.y.wrapping_sub(1);
                self.counter += min_cycles;
                self.set_zn(val);
                self.y = val;
                Ok(format!("Incremented x by one"))
            },
            _ => panic!(format!("Expected DEX/DEY, found {:?}", c))
        }
    }

    fn add(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a)?;
        let carry = self.status_register.get_flag(StatusFlags::C).bits;
        let next_carry = u8::MAX - rhs - carry > self.accumulator || (rhs == 255 && carry == 1);
        let res = self.accumulator.wrapping_add(rhs.wrapping_add(carry));
        let overflow = ((self.accumulator ^ res) & 0x80) != 0;
        self.set_cznv(res, next_carry, overflow);
        self.accumulator = res;
        self.pc += bytes;
        self.counter += min_cycles + extra_cycles;
        Ok(format!("Added {} to accumulator for {} with carry {}",
                   rhs.wrapping_add(carry), res, next_carry))
    }

    fn sub(&mut self, a: Address, min_cycles: u16) -> CPUResult<String> {
        let (bytes, extra_cycles, rhs) = self.address_read(a)?;
        let carry = self.status_register.get_flag(StatusFlags::C).bits;
        let next_carry = self.accumulator < (rhs + carry) || (rhs == 255 && carry == 1);
        let res = self.accumulator.wrapping_sub(rhs.wrapping_add(carry));
        let overflow = ((self.accumulator ^ (255 - res)) & 0x80) != 0;
        self.set_cznv(res, !next_carry, overflow);
        self.accumulator = res;
        self.pc += bytes;
        self.counter += min_cycles + extra_cycles;
        Ok(format!("Subtracted {} from accumulator for result {} with carry: {} and overflow: {} ",
                   rhs, res, next_carry, overflow))
    }

    fn shift_rotate(&mut self, a: Address, min_cycles: u16,
                    op: &Fn(&mut CPU, u8) -> u8) -> CPUResult<String> {
        let (bytes, val) = self.address_read_modify_write(a, op)?;
        self.set_zn(val);
        self.pc += bytes;
        self.counter += min_cycles;
        Ok(format!("Shifted {:?} right for result {:X}", a, val))
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

    fn branch(&mut self, a: Address, min_cycles: u16, cond: bool) -> CPUResult<String> {
        let (bytes, extra_cycles, val) = self.address_read(a)?;
        if cond {
            self.counter += min_cycles + 1 + 2 * extra_cycles;
            println!("Relative operand bytes: {}", bytes);
            self.pc += bytes;
            if val > 127 {
                self.pc -= (!val) as u16;
                Ok(format!("Branched by -{}", !val as u16))
            } else {
                self.pc += val as u16;
                Ok(format!("Branched by {}", val))
            }
        } else {
            self.counter += min_cycles;
            self.pc += bytes;
            Ok(format!("No branch."))
        }
    }
}
