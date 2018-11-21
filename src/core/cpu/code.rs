use std::fmt;
use super::CPU;

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
            Code::LDA | Code::LDX | Code::LDY |
            Code::STA | Code::STX | Code::STY |
            Code::ADC | Code::SBC |
            Code::INC | Code::INX | Code::INY |
            Code::DEC | Code::DEX | Code::DEY |
            Code::ASL | Code::LSR |
            Code::ROL | Code::ROR |
            Code::AND | Code::EOR | Code::ORA |
            Code::CMP | Code::CPX | Code::CPY | Code::BIT |
            Code::BCC | Code::BCS |
            Code::BEQ | Code::BNE |
            Code::BPL | Code::BVC |
            Code::BVS | Code::BMI |
            Code::TAX | Code::TXA | Code::TAY |
            Code::TYA | Code::TSX | Code::TXS |
            Code::PHA | Code::PLA |
            Code::PHP | Code::PLP |
            Code::JMP | Code::JSR | Code::RTS | Code::RTI |
            Code::SEC | Code::SED | Code::SEI |
            Code::CLC | Code::CLD | Code::CLI | Code::CLV |
            Code::NOP | Code::BRK | Code::ILL => write!(f, " {}", code),
            Code::LAX | Code::SAX | Code::ALR | Code::SLO | 
            Code::RLA | Code::RRA | Code::ARR | Code::XAA |
            Code::OAL | Code::TAS | Code::SAY |
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

use self::Code::*;

pub const OPCODE_TABLE: [Code; 256] =
//   0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    [BRK, ORA, ILL, SLO, _NP, ORA, ASL, SLO, PHP, ORA, ASL, ILL, _NP, ORA, ASL, SLO, //0
     BPL, ORA, ILL, SLO, _NP, ORA, ASL, SLO, CLC, ORA, _NP, SLO, _NP, ORA, ASL, SLO, //1
     JSR, AND, ILL, RLA, BIT, AND, ROL, RLA, PLP, AND, ROL, ILL, BIT, AND, ROL, RLA, //2
     BMI, AND, ILL, RLA, _NP, AND, ROL, RLA, SEC, AND, _NP, RLA, _NP, AND, ROL, RLA, //3
     RTI, EOR, ILL, LSE, _NP, EOR, LSR, LSE, PHA, EOR, LSR, ALR, JMP, EOR, LSR, LSE, //4
     BVC, EOR, ILL, LSE, _NP, EOR, LSR, LSE, CLI, EOR, _NP, LSE, _NP, EOR, LSR, LSE, //5
     RTS, ADC, ILL, RRA, _NP, ADC, ROR, RRA, PLA, ADC, ROR, ARR, JMP, ADC, ROR, RRA, //6
     BVS, ADC, ILL, RRA, _NP, ADC, ROR, RRA, SEI, ADC, _NP, RRA, _NP, ADC, ROR, RRA, //7
     _NP, STA, _NP, ASX, STY, STA, STX, ASX, DEY, _NP, TXA, XAA, STY, STA, STX, ASX, //8
     BCC, STA, ILL, AXA, STY, STA, STX, ASX, TYA, STA, TXS, TAS, SAY, STA, XAS, AXA, //9
     LDY, LDA, LDX, LAX, LDY, LDA, LDX, LAX, TAY, LDA, TAX, OAL, LDY, LDA, LDX, LAX, //A
     BCS, LDA, ILL, LAX, LDY, LDA, LDX, LAX, CLV, LDA, TSX, ILL, LDY, LDA, LDX, LAX, //B
     CPY, CMP, ILL, DCM, CPY, CMP, DEC, DCM, INY, CMP, DEX, SAX, CPY, CMP, DEC, DCM, //C
     BNE, CMP, ILL, DCM, _NP, CMP, DEC, DCM, CLD, CMP, _NP, DCM, _NP, CMP, DEC, DCM, //D
     CPX, SBC, _NP, INS, CPX, SBC, INC, INS, INX, SBC, NOP, _SB, CPX, SBC, INC, INS, //E
     BEQ, SBC, ILL, INS, _NP, SBC, INC, INS, SED, SBC, _NP, INS, _NP, SBC, INC, INS];//F

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Address {
    Accumulator,
    Implied,
    Immediate,
    Indirect,
    XIndirect,
    IndirectY,
    Halt,
    Relative,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY
}

const ACC: Address = Address::Accumulator;
const IMP: Address = Address::Implied;
const IND: Address = Address::Indirect;
const IMD: Address = Address::Immediate;
const IXI: Address = Address::XIndirect;
const IYI: Address = Address::IndirectY;
const HLT: Address = Address::Halt;
const REL: Address = Address::Relative;
const ZPG: Address = Address::ZeroPage;
const ZPX: Address = Address::ZeroPageX;
const ZPY: Address = Address::ZeroPageY;
const ABS: Address = Address::Absolute;
const ABX: Address = Address::AbsoluteX;
const ABY: Address = Address::AbsoluteY;

pub const ADDR_TABLE: [Address; 256] = 
//   0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F  
    [IMP, IXI, HLT, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, ACC, HLT, ABS, ABS, ABS, ABS, //0
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX, //1
     ABS, IXI, HLT, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, ACC, HLT, ABS, ABS, ABS, ABS, //2
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX, //3
     IMP, IXI, HLT, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, ACC, IMD, ABS, ABS, ABS, ABS, //4
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX, //5
     IMP, IXI, HLT, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, ACC, IMD, IND, ABS, ABS, ABS, //6
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX, //7
     IMD, IXI, IMD, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, IMP, IMD, ABS, ABS, ABS, ABS, //8
     REL, IYI, HLT, IXI, ZPX, ZPX, ZPY, ZPY, IMP, ABY, IMP, ABY, ABX, ABX, ABY, ABY, //9
     IMD, IXI, IMD, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, IMP, IMD, ABS, ABS, ABS, ABS, //A
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPY, ZPY, IMP, ABY, IMP, HLT, ABX, ABX, ABY, ABY, //B
     IMD, IXI, HLT, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, IMP, IMD, ABS, ABS, ABS, ABS, //C
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX, //D
     IMD, IXI, IMD, IXI, ZPG, ZPG, ZPG, ZPG, IMP, IMD, IMP, IMD, ABS, ABS, ABS, ABS, //E
     REL, IYI, HLT, IYI, ZPX, ZPX, ZPX, ZPX, IMP, ABY, IMP, ABY, ABX, ABX, ABX, ABX];//F


pub const BYTES_TABLE: [u8; 256] =
//   0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F  
    [0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 2, 2, 2, 2, //0
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, //1
     2, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 2, 2, 2, 2, //2
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, //3
     0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //4
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, //5
     0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //6
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, //7
     1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //8
     1, 1, 0, 1, 1, 1, 1, 2, 0, 2, 0, 2, 2, 2, 2, 2, //9
     1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //A
     1, 1, 0, 1, 1, 1, 1, 2, 0, 2, 0, 0, 2, 2, 2, 2, //B
     1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //C
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2, //D
     1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2, //E
     1, 1, 0, 1, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2];//F

pub fn print_instr(cpu:&CPU) -> String {
    let idx = cpu.last_op as usize;
    let addr = ADDR_TABLE[idx];
    let bytes = match addr {
        ACC | IMP | HLT => format!(""),
        IMD | IXI | IYI | REL |
        ZPG | ZPY | ZPX => format!(" {:02X}", cpu.byte_1),
        ABS | ABX | ABY | IND  => format!(" {:02X} {:02X}",
                                          cpu.byte_1, cpu.byte_2)
    };

    let bytes_len = BYTES_TABLE[idx];
    let spaces: String = match bytes_len {
        0 => format!("       "),
        1 => format!("    "),
        2 |
        _ => format!(" "),
    };

    let instr = OPCODE_TABLE[idx];

    let mut msg = match addr {
        ACC => format!(" A"),
        IMP | HLT => format!(""),
        IMD => format!(" #${:02X}", cpu.byte_1),
        IND => format!(" (${:02X}{:02X}) = {:02X}{:02X}",
                       cpu.byte_2,
                       cpu.byte_1,
                       cpu.pch,
                       cpu.pcl
        ),
        IYI => format!(" (${:02X}),Y = {:04X} @ {:04X}",
                       cpu.byte_1,
                       cpu.last_eff_addr.wrapping_sub(cpu.yir as u16),
                       cpu.last_eff_addr
        ),
        IXI => format!(" (${:02X},X) @ {:02X} = {:04X}",
                       cpu.byte_1,
                       cpu.byte_1.wrapping_add(cpu.xir),
                       cpu.last_eff_addr
        ),
        ABS => format!(" ${:02X}{:02X}", cpu.byte_2, cpu.byte_1),
        ABX | ABY => format!(" ${:02X}{:02X},{} @ {:04X}",
                             cpu.byte_2,
                             cpu.byte_1,
                             if addr == ABX { "X" } else { "Y" },
                             cpu.last_eff_addr
        ),
        ZPG => format!(" ${:02X}", cpu.byte_1),
        ZPX | ZPY => format!(" ${:02X},{} @ {:02X}",
                             cpu.byte_1,
                             if addr == ZPX { "X" } else { "Y" },
                             cpu.last_eff_addr
        ),
        REL => format!(" ${:04X}", cpu.last_eff_addr),
        _ => format!("")
    };

    match instr {
        STA | STX | STY |
        BIT => msg.push_str(&format!(" = {:02X}", cpu.last_val)),
        _NP | 
        INC | DEC |
        ASL | LSR |
        ROL | ROR => if addr != ACC && addr != IMP {
            msg.push_str(&format!(" = {:02X}", cpu.last_val))
        },
        SBC => if addr != IMD {
            msg.push_str(&format!(" = {:02X}", !cpu.last_val))
        }
        ADC |
        LDA | LDX | LDY |
        ORA | EOR | AND |
        CMP | CPX | CPY   => if addr != IMD {
            msg.push_str(&format!(" = {:02X}", cpu.last_val))
        },
        _ => {}
    }

    format!("{:04X}  {:02X}{}{}{}{}", cpu.last_pc, idx, bytes, spaces, instr, msg)
}
