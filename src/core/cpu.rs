use core::ines::INES;
use std::{u8, u16};
use std::num::Wrapping;

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const STACK_REGION: u16 = 0x01A0;

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

pub fn counter_inc(x: u16, y: u8) -> u8 {
    //x + y > c  | x + y < c
    //c < x + y  | c > x + y
    //c - x < y  | c - x > y
    if u8::MAX - ((x & 0x0F) as u8) > y {
        0
    } else {
        1
    }        
}

#[derive(Debug, Clone, Copy)]
pub enum AddressMode {
    Accumulator,
    Implied,
    Immediate(u8),
    Absolute(u16),
    ZeroPage(u8),
    Relative(i8),
    AbsIndexedX(u16),
    AbsIndexedY(u16),
    ZPIndexedX(u8),
    ZPIndexedY(u8),
    ZPIndexedIndirect(u8),
    ZPIndirectIndexed(u8),
    Indirect(u16)
}

#[derive(Debug, Clone, Copy)]
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
    NOP, BRK //56
}

impl Code {
    #[inline(always)]
    pub fn branch(upper_nybble: u8) -> Code {
        match upper_nybble {
            0x01 => Code::BPL,
            0x03 => Code::BMI,
            0x05 => Code::BVC,
            0x07 => Code::BVS,
            0x09 => Code::BCC,
            0x0B => Code::BCS,
            0x0D => Code::BNE,
            0x0F => Code::BEQ,
            _ => panic!(format!("Bad opcode; Found {:x}", upper_nybble))
        }
    }

    /// Implicit assumption that the low nybble is 1, 4, 9 or D
    #[inline(always)]
    pub fn arith_logic(upper_nybble: u8) -> Code {
        match upper_nybble {
            0x00 | 0x01 => Code::ORA,
            0x02 | 0x03 => Code::AND,
            0x04 | 0x05 => Code::EOR,
            0x06 | 0x07 => Code::ADC,
            0x08 | 0x09 => Code::STA,
            0x0A | 0x0B => Code::LDA,
            0x0C | 0x0D => Code::CMP,
            0x0E | 0x0F => Code::SBC,
            _ => panic!(format!("Bad opcode; Found {:x}", upper_nybble))
        }
    }
    
    #[inline(always)]
    pub fn stack_set_clear(upper_nybble: u8) -> Code {
        use self::Code::{PHP, CLC, PLP, SEC, PHA, CLI, PLA, SEI,
                         DEY, TYA, TAY, CLV, INY, CLD, INX, SED};
                   
        match upper_nybble {
            0x00 => PHP,
            0x01 => CLC,
            0x02 => PLP,
            0x03 => SEC,
            0x04 => PHA,
            0x05 => CLI,
            0x06 => PLA,
            0x07 => SEI,
            0x08 => DEY,
            0x09 => TYA,
            0x0A => TAY,
            0x0B => CLV,
            0x0C => INY,
            0x0D => CLD,
            0x0E => INX,
            0x0F => SED,
            _ => panic!(format!("Impossible! Got {:x}", upper_nybble))
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Instruction {
    mnemonic: Code,
    address: AddressMode
}

pub type CPUResult<T> = Result<T, String>;

pub struct CPU {
    counter: u16,
    pc: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status_register: StatusFlags,
    ram: [u8; 2048],
    cartridge: INES
}

impl CPU {
    pub fn power_up(ines: INES) -> CPU {
        let pc = RESET_VECTOR;
        CPU {
            counter: 0,
            pc: pc,
            stack_pointer: POWERUP_S,
            accumulator: 0,
            x: 0,
            y: 0,
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 2048],
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

    pub fn read(&self, address: u16) -> CPUResult<u8> {
        match address as usize {
            0x0000 ... 0x1FFF => {
                Ok(self.ram[(address % 2048) as usize])
            },
            a @ 0x2000 ... 0x401F => {
                Err(format!("Access to {:x} not yet implemented!", a))
            },
            0x4020 ... 0xFFFF => {
                self.cartridge.read(address as usize)
            }
            _ => Err(format!("I dunno lol"))
        }
    }

    pub fn write(&mut self, address: u16, val: u8) -> CPUResult<String> {
        match address as usize {
            0x0000 ... 0x1FFF => {
                self.ram[(address % 2048) as usize] = val;
                Ok(format!("Wrote {:x} to address {:x}", val, address))
            },
            0x2000 ... 0x401F => {
                Err(format!("Not implemented!"))
            },
            0x4020 ... 0xFFFF => {
                self.cartridge.write(address as usize, val)?;
                Ok(format!("Wrote {:x} to address {:x}", val, address))
            },
            _ => Err(format!("I dunno lol"))
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

    fn absolute_helper(&self) -> CPUResult<u16> {
        let upper_byte = (self.read(self.pc + 1)? as u16) << 8;
        let lower_byte = self.read(self.pc + 2)? as u16;
        Ok(upper_byte + lower_byte)
    }

    fn absolute(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Absolute(self.absolute_helper()?))
    }

    fn absolute_indexed_by_x(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedX(self.absolute_helper()?))
    }

    fn absolute_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedY(self.absolute_helper()?))
    }

    fn indirect(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Indirect(self.absolute_helper()?))
    }

    fn immediate(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Immediate(self.read(self.pc + 1)?))
    }
    
    fn relative(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Relative(self.read(self.pc + 1)? as i8))
    }

    fn indexed_indirect(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedIndirect(self.read(self.pc + 1)?))
    }
    
    fn indirect_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndirectIndexed(self.read(self.pc + 1)?))
    }

    fn zero_page(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZeroPage(self.read(self.pc + 1)?))
    }

    fn zero_page_indexed_by_x(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedX(self.read(self.pc + 1)?))
    }

    fn zero_page_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedY(self.read(self.pc + 1)?))
    }

    fn wrap_address(u: u8, v: u8) -> u16 {
        (Wrapping(u) + Wrapping(v)).0 as u16
    }

    fn make_instruction(&self, c: u8) -> CPUResult<Instruction> {
        let implied = AddressMode::Implied;
        let lower = c & 0x0F;
        let upper = (c & 0xF0) >> 4;
        let (code, admode) = match (lower, upper) {
            (0x00, 0x00) => (Code::BRK, implied),
            (0x00, x) => match x {
                0x04 => (Code::RTI, implied),
                0x06 => (Code::RTS, implied),
                0x0A => (Code::LDY, self.immediate()?),
                0x0C => (Code::CPY, self.immediate()?),
                0x0E => (Code::CPX, self.immediate()?),
                0x02 => (Code::JSR, self.absolute()?),
                x => (Code::branch(x), self.relative()?)
            },
            (0x01, x) => {
                let address = if x % 2 == 0 {
                    self.indexed_indirect()?
                } else {
                    self.indirect_indexed_by_y()?
                };
                (Code::arith_logic(x), address)
            },
            (0x02, 0x0A) => {
                (Code::LDX, self.immediate()?)
            },
            (0x04, x @ 0x02) | (0x04, x @ 0x08 ... 0x0C) | (0x04, x @ 0x0E) => {
                let address = if x % 2 == 0 {
                    self.zero_page()?
                } else {
                    self.zero_page_indexed_by_x()?
                };
                match x {
                    0x02 => (Code::BIT, address),
                    0x08 | 0x09 => (Code::STY, address),
                    0x0A | 0x0B => (Code::LDY, address),
                    0x0C => (Code::CPY, address),
                    0x0E => (Code::CPX, address),
                    _ => return Err(format!("Undefined opcode {:x}", c))
                }
            },
            (0x05, x) => {
                let address = if x % 2 == 0 {
                    self.zero_page()?
                } else {
                    self.zero_page_indexed_by_x()?
                };
                (Code::arith_logic(x), address)
            },
            (0x06, x @ 0x00 ... 0x07) | (0x06, x @ 0x0C ... 0x0F) => {
                let address = if x % 2 == 0 {
                    self.zero_page()?
                } else {
                    self.zero_page_indexed_by_x()?
                };
                match x / 2 {
                    0 => (Code::ASL, address),
                    1 => (Code::ROL, address),
                    2 => (Code::LSR, address),
                    3 => (Code::ROR, address),
                    6 => (Code::DEC, address),
                    7 => (Code::INC, address),
                    _ => panic!(format!("Undefined opcode {:x}", c))
                }
            },
            (0x06, x @ 0x08 ... 0x0F) => {
                let address = if x % 2 == 0 {
                    self.zero_page()?
                } else {
                    self.zero_page_indexed_by_y()?
                };
                if x / 2 == 4 {
                    (Code::STX, address)
                } else {
                    (Code::LDX, address)
                }
            },
            (0x08, x) => (Code::stack_set_clear(x), implied),
            (0x09, x) => {
                let address = if x % 2 == 0 {
                    self.immediate()?
                } else {
                    self.absolute_indexed_by_y()?
                };
                (Code::arith_logic(x), address)
            },
            (0x0A, x @ 0x00) | (0x0A, x @ 0x02) | (0x0A, x @ 0x04) | (0x0A, x @ 0x06) => {
                let address = AddressMode::Accumulator;
                match x {
                    0x00 => (Code::ASL, address),
                    0x02 => (Code::ROL, address),
                    0x04 => (Code::LSR, address),
                    0x06 => (Code::ROR, address),
                    _ => panic!(format!("Bad opcode; Found {:x}", x))
                }
            },
            (0x0A, x @ 0x08 ... 0x0C) | (0x0A, x @ 0x0E) => {
                let address = AddressMode::Implied;
                match x {
                    0x08 => (Code::TXA, address),
                    0x09 => (Code::TXS, address),
                    0x0A => (Code::TAX, address),
                    0x0B => (Code::TSX, address),
                    0x0C => (Code::DEX, address),
                    0x0E => (Code::NOP, address),
                    _ => panic!(format!("Bad opcode; Found {:x}", x))
                }
            },
            (0x0C, 0x02) => (Code::BIT, self.absolute()?),
            (0x0C, 0x04) => (Code::JMP, self.absolute()?),
            (0x0C, 0x06) => (Code::JMP, self.indirect()?),
            (0x0C, 0x08) => (Code::STY, self.absolute()?),
            (0x0C, 0x0A) => (Code::LDY, self.absolute()?),
            (0x0C, 0x0B) => (Code::LDY, self.absolute_indexed_by_x()?),
            (0x0C, 0x0C) => (Code::CPY, self.absolute()?),
            (0x0C, 0x0E) => (Code::CPX, self.absolute()?),
            (0x0D, x) => {
                let address = if x % 2 == 0 {
                    self.absolute()?
                } else {
                    self.absolute_indexed_by_x()?
                };
                (Code::arith_logic(x), address)
            },
            (0x0E, x @ 0x00 ... 0x07) => {
                let address = if x % 2 == 0 {
                    self.absolute()?
                } else {
                    self.absolute_indexed_by_x()?
                };
                match x / 2 {
                    0 => (Code::ASL, address),
                    1 => (Code::ROL, address),
                    2 => (Code::LSR, address),
                    3 => (Code::ROR, address),
                    _ => panic!(format!("Bad opcode; Found {:x}", x))
                }
            },
            (0x0E, 0x08) => (Code::STX, self.absolute()?),
            (0x0E, 0x0A) => (Code::LDX, self.absolute()?),
            (0x0E, 0x0B) => (Code::LDX, self.absolute_indexed_by_y()?),
            (0x0E, x @ 0x0C ... 0x0F) => {
                let address = if x % 2 == 0 {
                    self.absolute()?
                } else {
                    self.absolute_indexed_by_x()?
                };
                match x / 2 {
                    6 => (Code::DEC, address),
                    7 => (Code::INC, address),
                    _ => panic!(format!("Bad opcode; Found {:x}", x))
                }
            },
            _ => return Err(format!("Undefined opcode {:x}", c))
        };
        Ok(Instruction {
            mnemonic: code,
            address: admode
        })
    }

    fn load(&mut self, m: Code, a: AddressMode) -> CPUResult<String> {        
        let (bytes, cycles, val) = match a {
            AddressMode::Immediate(n) => (2, 2, n),
            AddressMode::ZeroPage(u) => (2, 3, self.read(u as u16)?),
            AddressMode::ZPIndexedX(u) => (2, 4, self.read((u + self.x) as u16)?),
            AddressMode::ZPIndexedY(u) => (2, 4, self.read((u + self.y) as u16)?),
            AddressMode::Absolute(addr) => (3, 4, self.read(addr)?),
            AddressMode::AbsIndexedX(addr) => {
                let counter = 4 + counter_inc(addr, self.x);
                let addr = addr + (self.x as u16);
                (3, counter, self.read(addr)?)
            },
            AddressMode::AbsIndexedY(addr) => {
                let counter = counter_inc(addr, self.y);
                let addr = addr + (self.x as u16);
                (3, counter, self.read(addr)?)
            },
            AddressMode::ZPIndexedIndirect(u) => {
                let addr = CPU::wrap_address(u, self.x);
                (2, 6, self.read(addr)?)
            },
            AddressMode::ZPIndirectIndexed(u) => {
                let counter = 5 + counter_inc(u as u16, self.y);
                (2, counter, self.read((u + self.y) as u16)?)
            },
            _=> {
                return Err(format!("Unexpected addressing mode {:?}", a))
            }
        };
        self.pc += bytes;
        self.counter += cycles as u16;
        match m {
            Code::LDA => self.accumulator = val,
            Code::LDX => self.x = val,
            Code::LDY => self.y = val,
            x => panic!(format!("Expected Load; Found {:?}", x))
        }

        Ok(format!("Loaded {} into {:?}", val, m))
    }

    pub fn store(&mut self, m: Code, a: AddressMode) -> CPUResult<String> {
        let val = match m {
            Code::STA => self.accumulator,
            Code::STX => self.x,
            Code::STY => self.y,
            x => panic!(format!("Expected Load; Found {:?}", x))
        };
        
        let (bytes, cycles, addr) = match a {
            AddressMode::ZeroPage(u) => (2, 3, u as u16),
            AddressMode::ZPIndexedX(u) => (2, 4, (u + self.x) as u16),
            AddressMode::ZPIndexedY(u) => (2, 4, (u + self.y) as u16),
            AddressMode::Absolute(addr) => (3, 4, addr),
            AddressMode::AbsIndexedX(addr) => (3, 5, addr + (self.x as u16)),
            AddressMode::AbsIndexedY(addr) => (3, 5, addr + (self.y as u16)),
            AddressMode::ZPIndexedIndirect(u) => (2, 6, CPU::wrap_address(u, self.x)),
            AddressMode::ZPIndirectIndexed(u) => (2, 6, (u + self.y) as u16),
            _=> return Err(format!("Unexpected addressing mode {:?}", a))
        };
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += cycles as u16;

        Ok(format!("Stored {:?} into {}", m, val))
    }

    #[inline(always)]
    pub fn wrapped_inc(&mut self, u: u8) -> CPUResult<u8> {
        let val = (Wrapping(u) + Wrapping(1)).0;
        if val == 0 {
            self.status_register | StatusFlags::Z;
        } else if (val & 0b1000_0000) == 1 {
            self.status_register | StatusFlags::N;
        }
        Ok(val)
    }

    pub fn inc(&mut self, a: AddressMode) -> CPUResult<String> {
        let (bytes, cycles, address) = match a {
            AddressMode::ZeroPage(u) => (2, 5, u as u16),
            AddressMode::ZPIndexedX(u) => (2, 6, (u + self.x) as u16),
            AddressMode::Absolute(addr) => (3, 6, addr),
            AddressMode::AbsIndexedX(addr) => (3, 7, addr + (self.x as u16)),
            x => return Err(format!("Impossible addressing mode {:?}", x))
        };
        self.pc += bytes;
        self.counter += cycles;
        let u = self.read(address)?;
        let val = self.wrapped_inc(u)?;
        self.write(address, val)
    }

    pub fn inc_reg(&mut self, m: Code) -> CPUResult<String> {
        self.pc += 1;
        self.counter += 2;
        match m {
            Code::INX => {
                let u = self.x;
                let val = self.wrapped_inc(u)?;
                self.x = val;
                Ok(format!("Incremented X by 1 for a result of {}", self.x))
            },
            _ => {
                let u = self.y;
                let val = self.wrapped_inc(u)?;
                self.y = val;
                Ok(format!("Incremented Y by 1 for a result of {}", self.y))
            }
        }
    }

    fn lsr_acc(&mut self) -> CPUResult<String> {
        self.status_register.bits |= self.accumulator & 0b0000_0001;
        self.accumulator >>= 1;
        if self.accumulator == 0 {
            self.status_register |= StatusFlags::Z;
        }
        self.pc += 1;
        self.counter += 2;
        Ok(format!("Shifted Accumulator right for result {:x}", self.accumulator))
    }

    fn lsr_mem(&mut self, a: AddressMode) -> CPUResult<String> {
        let (bytes, cycles, addr) = match a {
            AddressMode::ZeroPage(u) => (2, 5, u as u16),
            AddressMode::ZPIndexedX(u) => (2, 6, (u + self.x) as u16),
            AddressMode::Absolute(addr) => (3, 6, addr),
            AddressMode::AbsIndexedX(addr) => (3, 7, addr + (self.x as u16)),
            _ => return Err(format!("Unexpected addressing mode {:?}", a))
        };
        let mut val = self.read(addr)?;
        self.status_register.bits |= val & 0b0000_0001;
        val >>= 1;
        if val == 0 {
            self.status_register |= StatusFlags::Z;
        }
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += cycles;
        Ok(format!("Shifted {:x} right for result {:x}", addr, val))
    }

    pub fn execute(&mut self, instruction: Instruction) -> CPUResult<String> {
        match (instruction.mnemonic, instruction.address) {
            /* LDA, LDX, LDY, */
            (m @ Code::LDA, a) | (m @ Code::LDX, a) | (m @ Code::LDY, a) => self.load(m, a),
            /* STA, STX, STY, */
            (m @ Code::STA, a) | (m @ Code::STX, a) | (m @ Code::STY, a) => self.store(m, a),
            /* ADC, SBC, */
            
            /* INC */ 
            (Code::INC, a) => self.inc(a),
            /*INX, INY */
            (m @ Code::INX, AddressMode::Implied) |
            (m @ Code::INY, AddressMode::Implied) => self.inc_reg(m),
            /* DEC, DEX, DEY, */
            
            /* ASL, LSR */
            (Code::LSR, AddressMode::Accumulator) => self.lsr_acc(),
            (Code::LSR, a) => self.lsr_mem(a),
            /* ROL, ROR */
            
            /* AND, ORA, EOR, */
            
            /* CMP, CPX, CPY, */
            
            /* BIT, */
            
            /* BCC, BCS, BEQ, BMI, BNE, BPL, BVC, BVS, */
            
            /* TAX, TXA, TAY, TYA, TSX, TXS, */
            
            /* PHA, PLA, PHP, PLP, */
            
            /* JMP */
            (Code::JMP, AddressMode::Absolute(idx)) => {
                self.counter += 3;
                self.pc = idx;
                Ok(format!("Set pc to {:x}", idx))
            },
            (Code::JMP, AddressMode::Indirect(idx)) => {
                self.counter += 5;
                let lower = self.read(idx)? as u16;
                let upper = (self.read(idx + 1)?  as u16) << 8;
                let addr = upper + lower;
                self.pc = addr;
                Ok(format!("Set pc to {:x}", addr))
            },
            /*JSR */

            /*RTS, RTI, */
            (Code::RTI, AddressMode::Implied) => {
                let flags = self.stack_pop()?;
                self.status_register.bits = flags;
                /* let lower = self.stack_pop()? as u16;
                let upper = (self.stack_pop()? as u16) << 8; */
                let upper = (self.stack_pop()? as u16) << 8;
                let lower = self.stack_pop()? as u16;
                self.pc = upper + lower;
                self.counter += 6;
                Ok(format!("Set pc to {:x} from interrupt", self.pc))
            }
            /* SEC, SED, SEI,*/
            (Code::SEI, AddressMode::Implied) => {
                self.counter += 2;
                self.pc += 1;
                self.status_register = self.status_register | StatusFlags::I;
                Ok(format!("Set Interrupt Disable to 1"))
            }
            /* CLC, CLD, CLI, CLV, */
            /* NOP, BRK */
            (Code::BRK, AddressMode::Implied) => {
                if self.stack_pointer < 2 {
                    Err(format!("Stack overflowed!"))
                } else {
                    self.counter += 7;
                    let upper = ((self.pc & 0xFF00) >> 8) as u8;
                    self.stack_push(upper)?;
                    let lower = (self.pc & 0x00FF) as u8;
                    self.stack_push(lower)?;
                    
                    let irq_lower = self.read(0xFFFE)? as u16;
                    let irq_upper = (self.read(0xFFFF)? as u16) << 8;
                    self.pc = irq_upper + irq_lower;
                    
                    let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                    self.stack_push(flags.bits)
                }
            },
            _ => Err(format!("{:?} not implemented", instruction))
        }
    }

    pub fn get_pc(&self) -> u16 {
        self.pc
    }

    pub fn get_counter(&self) -> u16 {
        self.counter
    }

    pub fn step(&mut self) -> CPUResult<String> {
        let code = self.read(self.pc)?;
        let instruction = self.make_instruction(code)?;
        self.execute(instruction)
    }

    pub fn init(&mut self) -> CPUResult<String> {
        let instruction = Instruction {
            mnemonic: Code::JMP, address: AddressMode::Indirect(self.pc)
        };
        self.execute(instruction)
    }
}
