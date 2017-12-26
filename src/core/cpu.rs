use core::ppu::PPU;
use core::ines::INES;
use std::{i8, u8, u16};
use std::num::Wrapping;

pub const POWERUP_S: u8 = 0xFD;
pub const _MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const _CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const RESET_VECTOR: u16 = 0xFFFC;
pub const STACK_REGION: u16 = 0x01A0;
pub const COUNTER_CONST: u16 = 2;

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
    ppu: PPU,
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
            ppu: PPU::init(),
            cartridge: ines
        }
    }

    pub fn _reset(&mut self) {
        self.stack_pointer -= 3;
        self.status_register = self.status_register | StatusFlags::I;
    }

    pub fn read_double(&mut self, addr: u16) -> CPUResult<u16> {
        let lower = self.read(addr)?;
        let upper = self.read(addr + 1)?;
        Ok(combine(lower, upper))
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
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

    fn absolute_helper(&mut self) -> CPUResult<u16> {
        let addr = self.pc + 1;
        self.read_double(addr)
    }

    fn absolute(&mut self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Absolute(self.absolute_helper()?))
    }

    fn absolute_indexed_by_x(&mut self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedX(self.absolute_helper()?))
    }

    fn absolute_indexed_by_y(&mut self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedY(self.absolute_helper()?))
    }

    fn indirect(&mut self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Indirect(self.absolute_helper()?))
    }

    fn immediate(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::Immediate(self.read(addr)?))
    }
    
    fn relative(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::Relative(self.read(addr)? as i8))
    }

    fn indexed_indirect(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::ZPIndexedIndirect(self.read(addr)?))
    }
    
    fn indirect_indexed_by_y(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::ZPIndirectIndexed(self.read(addr)?))
    }

    fn zero_page(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::ZeroPage(self.read(addr)?))
    }

    fn zero_page_indexed_by_x(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::ZPIndexedX(self.read(addr)?))
    }

    fn zero_page_indexed_by_y(&mut self) -> CPUResult<AddressMode> {
        let addr = self.pc + 1;
        Ok(AddressMode::ZPIndexedY(self.read(addr)?))
    }

    fn wrap_address(u: u8, v: u8) -> u16 {
        (Wrapping(u) + Wrapping(v)).0 as u16
    }

    pub fn decode_address_read(&mut self, a: AddressMode) -> CPUResult<(u16, u16, u8)> {
        match a {
            AddressMode::Immediate(n) => Ok((2, 2, n)),
            AddressMode::ZeroPage(u) => Ok((2, 3, self.read(u as u16)?)),
            AddressMode::ZPIndexedX(u) => {
                let addr = (u as u16) + (self.x as u16);
                Ok((2, 4, self.read(addr)?))
            },
            AddressMode::ZPIndexedY(u) => {
                let res = (u as u16) + (self.y as u16);
                Ok((2, 4, self.read(res)?))
            },
            AddressMode::Absolute(addr) => Ok((3, 4, self.read(addr)?)),
            AddressMode::AbsIndexedX(addr) => {
                let counter = 4 + counter_inc(addr, self.x);
                let addr = addr + (self.x as u16);
                Ok((3, counter, self.read(addr)?))
            },
            AddressMode::AbsIndexedY(addr) => {
                let counter = 4 + counter_inc(addr, self.y);
                let addr = addr + (self.x as u16);
                Ok((3, counter, self.read(addr)?))
            },
            AddressMode::ZPIndexedIndirect(u) => {
                let addr = CPU::wrap_address(u, self.x);
                Ok((2, 6, self.read(addr)?))
            },
            AddressMode::ZPIndirectIndexed(u) => {
                let counter = 5 + counter_inc(u as u16, self.y);
                let pre_addr = (u + self.y) as u16;
                Ok((2, counter, self.read(pre_addr)?))
            },
            _=> Err(format!("Unexpected addressing mode {:?}", a))
        }
    }

    #[inline(always)]
    pub fn decode_address_write(&self, a: AddressMode) -> CPUResult<(u16, u16, u16)> {
        match a {
            AddressMode::ZeroPage(u) => Ok((2, 3, u as u16)),
            AddressMode::ZPIndexedX(u) => Ok((2, 4, (u as u16) + (self.x as u16))),
            AddressMode::ZPIndexedY(u) => Ok((2, 4, (u as u16) + (self.y as u16))),
            AddressMode::Absolute(addr) => Ok((3, 4, addr)),
            AddressMode::AbsIndexedX(addr) => Ok((3, 5, addr + (self.x as u16))),
            AddressMode::AbsIndexedY(addr) => Ok((3, 5, addr + (self.y as u16))),
            AddressMode::ZPIndexedIndirect(u) => Ok((2, 6, CPU::wrap_address(u, self.x))),
            AddressMode::ZPIndirectIndexed(u) => Ok((2, 6, (u as u16) + (self.y as u16))),
            _=> Err(format!("Unexpected addressing mode {:?}", a))
        }
    }

    fn make_instruction(&mut self, c: u8) -> CPUResult<Instruction> {
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
        let (bytes, cycles, val) = self.decode_address_read(a)?;
        self.pc += bytes;
        self.counter += cycles;
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
        
        let (bytes, cycles, addr) = self.decode_address_write(a)?;
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += cycles;

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
        println!("doot {}", val);
        Ok(val)
    }

    #[inline(always)]
    pub fn wrapped_dec(&mut self, u: u8) -> CPUResult<u8> {
        let val = (Wrapping(u) - Wrapping(1)).0;
        if val == 0 {
            self.status_register | StatusFlags::Z;
        } else if (val & 0b1000_0000) == 1 {
            self.status_register | StatusFlags::N;
        }
        Ok(val)
    }

    pub fn inc(&mut self, a: AddressMode) -> CPUResult<String> {
        let (bytes, cycles, address) = self.decode_address_write(a)?;
        self.pc += bytes;
        self.counter += cycles + COUNTER_CONST;
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
                println!("{}", u);
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

    pub fn dec(&mut self, a: AddressMode) -> CPUResult<String> {
        let (bytes, cycles, address) = self.decode_address_write(a)?;
        self.pc += bytes;
        self.counter += cycles + COUNTER_CONST;
        let u = self.read(address)?;
        let val = self.wrapped_inc(u)?;
        self.write(address, val)
    }

    pub fn dec_reg(&mut self, m: Code) -> CPUResult<String> {
        self.pc += 1;
        self.counter += 2;
        match m {
            Code::DEX => {
                let u = self.x;
                let val = self.wrapped_dec(u)?;
                self.x = val;
                Ok(format!("Decremented X by 1 for a result of {}", self.x))
            },
            _ => {
                let u = self.y;
                let val = self.wrapped_dec(u)?;
                self.y = val;
                Ok(format!("Decremented Y by 1 for a result of {}", self.y))
            }
        }
    }

    fn lsr_acc(&mut self) -> CPUResult<String> {
        self.status_register.bits |= self.accumulator & 0b0000_0001;
        self.accumulator >>= 1;
        if self.accumulator == 0 {
            self.status_register |= StatusFlags::Z;
        } else {
            self.status_register &= !StatusFlags::Z;
        }
        self.pc += 1;
        self.counter += 2;
        Ok(format!("Shifted Accumulator right for result {:x}", self.accumulator))
    }

    fn lsr_mem(&mut self, a: AddressMode) -> CPUResult<String> {
        let (bytes, cycles, addr) = self.decode_address_write(a)?;
        let mut val = self.read(addr)?;
        self.status_register.bits |= val & 0b0000_0001;
        val >>= 1;
        if val == 0 {
            self.status_register |= StatusFlags::Z;
        } else {
            self.status_register &= !StatusFlags::Z;
        }
        self.write(addr, val)?;
        self.pc += bytes;
        self.counter += cycles + COUNTER_CONST;
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
            (Code::DEC, a) => self.dec(a),
            (m @ Code::DEX, AddressMode::Implied) |
            (m @ Code::DEY, AddressMode::Implied) => self.dec_reg(m),
            /* ASL, LSR */
            (Code::LSR, AddressMode::Accumulator) => self.lsr_acc(),
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
                if self.accumulator == 0 {
                    self.status_register |= StatusFlags::Z;
                    self.status_register &= !StatusFlags::N;
                } else if self.accumulator & 0b1000_0000 == 0b1000_0000 {
                    self.status_register |= StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                }
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
                } else {
                    if lhs == val {
                        self.status_register |= StatusFlags::Z;
                        self.status_register |= StatusFlags::C;
                        self.status_register &= !StatusFlags::N;
                    }
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
            (c @ Code::BNE, AddressMode::Relative(d)) |
            (c @ Code::BEQ, AddressMode::Relative(d)) |
            (c @ Code::BPL, AddressMode::Relative(d)) => {
                let cond = if c == Code::BEQ {
                    println!("BEQ {}", (self.status_register & StatusFlags::Z) == StatusFlags::Z);
                    (self.status_register & StatusFlags::Z) == StatusFlags::Z
                } else if c == Code::BNE {
                    println!("BNE {}",(self.status_register & StatusFlags::Z) != StatusFlags::Z);
                    (self.status_register & StatusFlags::Z) == StatusFlags::Z
                } else {
                    println!("BPL {}", (self.status_register & StatusFlags::N) != StatusFlags::N);
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
            (Code::TXS, AddressMode::Implied) => {
                let val = self.x;
                self.stack_push(val)?;
                self.pc += 1;
                self.counter += 2;
                Ok(format!("Transferred x to stack"))
            }
            /* PHA */ 
            (Code::PHA, AddressMode::Implied) => {
                self.counter += 3;
                self.pc += 1;
                let val = self.accumulator;
                self.stack_push(val)
            },
            /* PHP */
            (Code::PHP, AddressMode::Implied) => {
                self.counter += 2;
                self.pc += 1;
                let flags = self.status_register | StatusFlags::S | StatusFlags::B;
                self.stack_push(flags.bits)
            },
            /* PLA */
            (Code::PLA, AddressMode::Implied) => {
                self.counter += 4;
                self.pc += 1;
                self.accumulator = self.stack_pop()?;
                if self.accumulator == 0 {
                    self.status_register |= StatusFlags::Z;
                    self.status_register &= !StatusFlags::N;
                } else if (self.accumulator & 0b1000_0000) == 1 {
                    self.status_register |= StatusFlags::N;
                    self.status_register &= !StatusFlags::Z;
                }
                Ok(format!("Pulled accumulator from stack!"))
            }
            /* PLP, */
            /* JMP */
            (Code::JMP, AddressMode::Absolute(idx)) => {
                self.counter += 3;
                self.pc = idx;
                Ok(format!("Set pc to {:x}", idx))
            },
            (Code::JMP, AddressMode::Indirect(idx)) => {
                self.counter += 5;
                let addr = self.read_double(idx)?;
                self.pc = addr;
                Ok(format!("Set pc to {:x}", addr))
            },
            /*JSR */
            (Code::JSR, AddressMode::Absolute(idx)) => {
                self.counter += 6;
                let ret_addr = self.pc + 2;
                self.stack_push_double(ret_addr)?;
                self.pc = idx;
                Ok(format!("Pushed {:x} onto stack and set pc to {:x}.", ret_addr, idx))
            },
            /*RTS, RTI, */
            (Code::RTS, AddressMode::Implied) => {
                let addr = self.stack_pop_double()?;
                self.pc = addr + 1;
                self.counter += 6;
                Ok(format!("Returned pc to {:x}", self.pc)) 
            },
            (Code::RTI, AddressMode::Implied) => {
                let flags = self.stack_pop()?;
                self.status_register.bits = flags;
                let addr = self.stack_pop_double()?;
                self.pc = addr;
                self.counter += 6;
                Ok(format!("Set pc to {:x} from interrupt", self.pc))
            }
            /* SEC, SED, SEI,*/
            (Code::SEI, AddressMode::Implied) => {
                self.counter += 2;
                self.pc += 1;
                self.status_register |=  StatusFlags::I;
                Ok(format!("Set Interrupt Disable to 1"))
            }
            /* CLC, CLD, CLI, CLV, */
            (Code::CLD, AddressMode::Implied) => {
                self.counter += 2;
                self.pc += 1;
                self.status_register &= !StatusFlags::D;
                Ok(format!("Cleared decimal flag"))
            }
            /* NOP, BRK */
            (Code::BRK, AddressMode::Implied) => {
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
    }

    pub fn get_pc(&self) -> u16 {
        self.pc
    }

    pub fn get_counter(&self) -> u16 {
        self.counter
    }

    pub fn step(&mut self) -> CPUResult<String> {
        let pc = self.pc;
        let code = self.read(pc)?;
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
