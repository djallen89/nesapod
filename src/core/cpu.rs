use core::ines::INES;
use std::{u16, mem};

pub const POWERUP_S: u8 = 0xFD;
pub const MASTER_FREQ_NTSC: f64 = 21.477272; //MHz
pub const CPU_FREQ_NTSC: f64 = 1.789773; //MHz
pub const RESET_VECTOR: u16 = 0xFFFC;

bitflags! {
    struct StatusFlags: u8 {
        const C = 0b0000_0001;
        const Z = 0b0000_0010;
        const I = 0b0000_0100;
        const D = 0b0000_1000;
        const S = 0b0011_0000;
        const V = 0b0100_0000;
        const N = 0b1000_0000;
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

impl AddressMode {
    pub fn size(&self) -> usize {
        mem::size_of::<AddressMode>()
    }
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

impl Instruction {
    pub fn size(&self) -> u8 {
        self.address.size() as u8
    }
}

pub type CPUResult<T> = Result<T, String>;

pub struct CPU {
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

    pub fn reset(&mut self) {
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
            0x2000 ... 0x401F => {
                Err(format!("Not implemented!"))
            },
            0x4020 ... 0xFFFF => {
                self.cartridge.read(address as usize)
            }
            _ => Err(format!("I dunno lol"))
        }
    }

    pub fn write(&mut self, address: u16) -> CPUResult<String> {
        Ok(format!("FOO"))
    }

    #[inline(always)]
    pub fn absolute_helper(&self) -> CPUResult<u16> {
        let upper_byte = (self.read(self.pc + 1)? as u16) << 8;
        let lower_byte = self.read(self.pc + 2)? as u16;
        Ok(upper_byte + lower_byte)
    }

    #[inline(always)]
    pub fn absolute(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Absolute(self.absolute_helper()?))
    }

    #[inline(always)]
    pub fn absolute_indexed_by_x(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedX(self.absolute_helper()?))
    }

    #[inline(always)]
    pub fn absolute_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::AbsIndexedY(self.absolute_helper()?))
    }

    #[inline(always)]
    pub fn indirect(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Indirect(self.absolute_helper()?))
    }

    #[inline(always)]
    pub fn immediate(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Immediate(self.read(self.pc + 1)?))
    }
    
    #[inline(always)]
    pub fn relative(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::Relative(self.read(self.pc + 1)? as i8))
    }

    #[inline(always)]
    pub fn indexed_indirect(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedIndirect(self.read(self.pc + 1)?))
    }
    
    #[inline(always)]
    pub fn indirect_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndirectIndexed(self.read(self.pc + 1)?))
    }

    #[inline(always)]
    pub fn zero_page(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZeroPage(self.read(self.pc + 1)?))
    }

    #[inline(always)]
    pub fn zero_page_indexed_by_x(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedX(self.read(self.pc + 1)?))
    }

    #[inline(always)]
    pub fn zero_page_indexed_by_y(&self) -> CPUResult<AddressMode> {
        Ok(AddressMode::ZPIndexedY(self.read(self.pc + 1)?))
    }

    pub fn make_instruction(&self, c: u8) -> CPUResult<Instruction> {
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
            (0x0D, x) => {
                let address = if x % 2 == 0 {
                    self.absolute()?
                } else {
                    self.absolute_indexed_by_x()?
                };
                (Code::arith_logic(x), address)
            },
            _ => return Err(format!("Undefined opcode {:x}", c))
        };
        Ok(Instruction {
            mnemonic: code,
            address: admode
        })
    }

    pub fn execute(&mut self, instruction: Instruction) -> CPUResult<String> {
        match (instruction.mnemonic, instruction.address) {
            (Code::JMP, AddressMode::Absolute(idx)) => {
                self.pc = idx;
                Ok(format!("Set pc to {:x}", idx))
            },
            _ => Err(format!("{:?} not implemented", instruction))
        }
    }

    pub fn step(&mut self) -> CPUResult<String> {
        let code = self.read(self.pc)?;
        println!("{:x}: {:x}", self.pc, code);
        match self.make_instruction(code) {
            Ok(x) => {
                let result = self.execute(x);
                if self.pc == u16::MAX {
                    self.pc = 0x6000;
                    return Err(format!("max size counter, last byte {:?}", result))
                } else {
                    self.pc += 1;
                    result
                }
            },
            Err(f) => {
                if self.pc == u16::MAX {
                    self.pc = 0x8000;
                } else {
                    self.pc += 1;
                }
                Err(f)
            }
        }
    }

    pub fn init(&mut self) -> CPUResult<String> {
        let upper = (self.read(self.pc)? as u16) << 8;
        let lower = self.read(self.pc)? as u16;
        let instruction = Instruction {
            mnemonic: Code::JMP, address: AddressMode::Indirect(upper + lower)
        };
        self.execute(instruction)
    }
}
