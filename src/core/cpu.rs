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
    pub fn len(&self) -> usize {
        mem::size_of::<AddressMode>() - 1
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
    pub fn branch(lower_nybble: u8) -> Code {
        match lower_nybble {
            0x01 => Code::BPL,
            0x03 => Code::BMI,
            0x05 => Code::BVC,
            0x07 => Code::BVS,
            0x09 => Code::BCC,
            0x0B => Code::BCS,
            0x0D => Code::BNE,
            0x0F => Code::BEQ,
            _ => panic!("Impossible")
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

    pub fn read(&self, address: usize) -> CPUResult<u8> {
        match address {
            0x0000 ... 0x1FFF => {
                Ok(self.ram[(address % 2048) as usize])
            },
            0x2000 ... 0x401F => {
                Err(format!("Not implemented!"))
            },
            0x4020 ... 0xFFFF => {
                self.cartridge.read(address)
            }
            _ => Err(format!("I dunno lol"))
        }
    }

    pub fn write(&mut self, address: u16) -> CPUResult<String> {
        Ok(format!("FOO"))
    }

    pub fn absolute(&self, idx: usize) -> CPUResult<AddressMode> {
        let upper_byte = (self.read(idx + 1)? as u16) << 8;
        let lower_byte = self.read(idx + 2)? as u16;
        Ok(AddressMode::Absolute(upper_byte + lower_byte))
    }

    pub fn immediate(&self, idx: usize) -> CPUResult<AddressMode> {
        Ok(AddressMode::Immediate(self.read(idx + 1)?))
    }

    pub fn relative(&self, idx: usize) -> CPUResult<AddressMode> {
        Ok(AddressMode::Relative(self.read(idx + 1)? as i8))
    }

    pub fn make_instruction(&self, c: u8) -> CPUResult<Instruction> {
        let implied = AddressMode::Implied;
        let (code, admode) = match (c & 0x0F, (c & 0xF0) >> 4) {
            (0x00, x) => match x {
                0x00 => (Code::BRK, implied),
                0x02 => {
                    let address = self.absolute(self.pc as usize)?;
                    (Code::JSR, address)
                },
                0x04 => (Code::RTI, implied),
                0x06 => (Code::RTS, implied),
                0x0A => (Code::LDY, self.immediate(self.pc as usize)?),
                0x0C => (Code::CPY, self.immediate(self.pc as usize)?),
                0x0E => (Code::CPX, self.immediate(self.pc as usize)?),
                x => {
                    let address = self.relative(x as usize)?;
                    (Code::branch(x), address)
                }
            },
            (0x0C, 0x04) => {
                let address = self.absolute(self.pc as usize)?;
                (Code::JMP, address)
            },
            _ => return Err(format!("Undefined"))
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
        let code = self.read(self.pc as usize)?;
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
        let instruction = Instruction {
            mnemonic: Code::JMP, address: self.absolute(self.pc as usize - 1)?
        };
        self.execute(instruction)
    }
}
