use core::ines::INES;

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

pub struct AsmInstruction {
    code: Code,
    admode: AddressMode
}

impl AsmInstruction {
    pub fn new(code: Code, admode: AddressMode) -> AsmInstruction {
        AsmInstruction {
            code: code,
            admode: admode
        }
    }
}

pub struct CPU {
    program_counter: u16,
    stack_pointer: u8,
    accumulator: u8,
    x: u8,
    y: u8,
    status_register: StatusFlags,
    ram: [u8; 8192],
    cartridge: INES
}

impl CPU {
    pub fn power_up(ines: INES) -> CPU {
        CPU {
            program_counter: RESET_VECTOR,
            stack_pointer: POWERUP_S,
            accumulator: 0,
            x: 0,
            y: 0,
            status_register: StatusFlags::I | StatusFlags::S,
            ram: [0; 8192],
            cartridge: ines
        }
    }

    pub fn shut_down(self) -> INES {
        self.cartridge
    }
}
