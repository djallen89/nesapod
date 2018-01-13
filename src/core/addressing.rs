use core::cpu::Code;
use core::cpu::Code::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Address {
    Specified(AddressType),
    Implied,
    Acc,
    Invalid,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AddressType {
    SingleByte(SingleType),
    DoubleByte(DoubleType)
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SingleType {
    Immediate,
    Relative,
    ZeroPg,
    ZeroPgX,
    ZeroPgY,
    IndirectX,
    IndirectY,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DoubleType {
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
}

const ACC: Address = Address::Acc;
const IMP: Address = Address::Implied;
const HLT: (Code, Address, u16) = (ILL, Address::Invalid, 0);
const IXI: Address = Address::Specified(AddressType::SingleByte(SingleType::IndirectX));
const IYI: Address = Address::Specified(AddressType::SingleByte(SingleType::IndirectY));
const REL: Address = Address::Specified(AddressType::SingleByte(SingleType::Relative));
const IMD: Address = Address::Specified(AddressType::SingleByte(SingleType::Immediate));
const ZPG: Address = Address::Specified(AddressType::SingleByte(SingleType::ZeroPg));
const ZPX: Address = Address::Specified(AddressType::SingleByte(SingleType::ZeroPgX));
const ZPY: Address = Address::Specified(AddressType::SingleByte(SingleType::ZeroPgY));
const IND: Address = Address::Specified(AddressType::DoubleByte(DoubleType::Indirect));
const ABS: Address = Address::Specified(AddressType::DoubleByte(DoubleType::Absolute));
const ABX: Address = Address::Specified(AddressType::DoubleByte(DoubleType::AbsoluteX));
const ABY: Address = Address::Specified(AddressType::DoubleByte(DoubleType::AbsoluteY));

pub const OPCODE_TABLE: [(Code, Address, u16); 256] =
//              0              1               2             3              4             5              6               7             8              9              A               B              C              D              E              F
    [(BRK, IMP, 7), (ORA, IXI, 6),           HLT, (SLO, IXI, 8), (_NP, ZPG, 3), (ORA, ZPG, 3), (ASL, ZPG, 5), (SLO, ZPG, 5), (PHP, IMP, 3), (ORA, IMD, 2), (ASL, ACC, 2),           HLT, (_NP, ABS, 4), (ORA, ABS, 4), (ASL, ABS, 6), (SLO, ABS, 6), //0
     (BPL, REL, 2), (ORA, IYI, 5),           HLT, (SLO, IYI, 8), (_NP, ZPX, 4), (ORA, ZPX, 4), (ASL, ZPX, 6), (SLO, ZPX, 6), (CLC, IMP, 2), (ORA, ABY, 4), (_NP, IMP, 2), (SLO, ABY, 7), (_NP, ABX, 4), (ORA, ABX, 4), (ASL, ABX, 7), (SLO, ABX, 7), //1
     (JSR, ABS, 6), (AND, IXI, 6),           HLT, (RLA, IXI, 8), (BIT, ZPG, 3), (AND, ZPG, 3), (ROL, ZPG, 5), (RLA, ZPG, 5), (PLP, IMP, 4), (AND, IMD, 2), (ROL, ACC, 2),           HLT, (BIT, ABS, 4), (AND, ABS, 4), (ROL, ABS, 6), (RLA, ABS, 6), //2
     (BMI, REL, 2), (AND, IYI, 5),           HLT, (RLA, IYI, 8), (_NP, ZPX, 4), (AND, ZPX, 4), (ROL, ZPX, 6), (RLA, ZPX, 6), (SEC, IMP, 2), (AND, ABY, 4), (_NP, IMP, 2), (RLA, ABY, 7), (_NP, ABX, 4), (AND, ABX, 4), (ROL, ABX, 7), (RLA, ABX, 7), //3
     (RTI, IMP, 6), (EOR, IXI, 6),           HLT, (LSE, IXI, 8), (_NP, ZPG, 3), (EOR, ZPG, 3), (LSR, ZPG, 5), (LSE, ZPG, 5), (PHA, IMP, 3), (EOR, IMD, 2), (LSR, ACC, 2), (ALR, IMD, 2), (JMP, ABS, 3), (EOR, ABS, 4), (LSR, ABS, 6), (LSE, ABS, 6), //4
     (BVC, REL, 2), (EOR, IYI, 5),           HLT, (LSE, IYI, 8), (_NP, ZPX, 4), (EOR, ZPX, 4), (LSR, ZPX, 6), (LSE, ZPX, 6), (CLI, IMP, 2), (EOR, ABY, 4), (_NP, IMP, 2), (LSE, ABY, 7), (_NP, ABX, 4), (EOR, ABX, 4), (LSR, ABX, 7), (LSE, ABX, 7), //5
     (RTS, IMP, 6), (ADC, IXI, 6),           HLT, (RRA, IXI, 8), (_NP, ZPG, 3), (ADC, ZPG, 3), (ROR, ZPG, 5), (RRA, ZPG, 5), (PLA, IMP, 4), (ADC, IMD, 2), (ROR, ACC, 2), (ARR, IMD, 2), (JMP, IND, 5), (ADC, ABS, 4), (ROR, ABS, 6), (RRA, ABS, 6), //6
     (BVS, REL, 2), (ADC, IYI, 5),           HLT, (RRA, IYI, 8), (_NP, ZPX, 4), (ADC, ZPX, 4), (ROR, ZPX, 6), (RRA, ZPX, 6), (SEI, IMP, 2), (ADC, ABY, 5), (_NP, IMP, 2), (RRA, ABY, 7), (_NP, ABX, 4), (ADC, ABX, 4), (ROR, ABX, 7), (RRA, ABX, 7), //7
     (_NP, IMD, 2), (STA, IXI, 6), (_NP, IMD, 2), (ASX, IXI, 6), (STY, ZPG, 3), (STA, ZPG, 3), (STX, ZPG, 3), (ASX, ZPG, 3), (DEY, IMP, 2), (_NP, IMD, 2), (TXA, IMP, 2), (XAA, IMD, 2), (STY, ABS, 4), (STA, ABS, 4), (STX, ABS, 4), (ASX, ABS, 4), //8
     (BCC, REL, 2), (STA, IYI, 6),           HLT, (AXA, IXI, 6), (STY, ZPX, 4), (STA, ZPX, 4), (STX, ZPY, 4), (ASX, ZPY, 4), (TYA, IMP, 2), (STA, ABY, 5), (TXS, IMP, 2), (TAS, ABY, 5), (SAY, ABX, 4), (STA, ABX, 5), (XAS, ABY, 5), (AXA, ABY, 5), //9
     (LDY, IMD, 2), (LDA, IXI, 6), (LDX, IMD, 2), (LAX, IXI, 6), (LDY, ZPG, 3), (LDA, ZPG, 3), (LDX, ZPG, 3), (LAX, ZPG, 3), (TAY, IMP, 2), (LDA, IMD, 2), (TAX, IMP, 2), (OAL, IMD, 2), (LDY, ABS, 4), (LDA, ABS, 4), (LDX, ABS, 4), (LAX, ABS, 4), //A
     (BCS, REL, 2), (LDA, IYI, 5),           HLT, (LAX, IYI, 5), (LDY, ZPX, 4), (LDA, ZPX, 4), (LDX, ZPY, 4), (LAX, ZPY, 4), (CLV, IMP, 2), (LDA, ABY, 4), (TSX, IMP, 2),           HLT, (LDY, ABX, 4), (LDA, ABX, 4), (LDX, ABY, 4), (LAX, ABY, 4), //B
     (CPY, IMD, 2), (CMP, IXI, 6),           HLT, (DCM, IXI, 8), (CPY, ZPG, 3), (CMP, ZPG, 3), (DEC, ZPG, 5), (DCM, ZPG, 5), (INY, IMP, 2), (CMP, IMD, 2), (DEX, IMP, 2), (SAX, IMD, 2), (CPY, ABS, 4), (CMP, ABS, 4), (DEC, ABS, 6), (DCM, ABS, 6), //C
     (BNE, REL, 2), (CMP, IYI, 5),           HLT, (DCM, IYI, 8), (_NP, ZPX, 2), (CMP, ZPX, 4), (DEC, ZPX, 6), (DCM, ZPX, 6), (CLD, IMP, 2), (CMP, ABY, 4), (_NP, IMP, 2), (DCM, ABY, 7), (_NP, ABX, 4), (CMP, ABX, 4), (DEC, ABX, 7), (DCM, ABX, 7), //D
     (CPX, IMD, 2), (SBC, IXI, 6), (_NP, IMD, 2), (INS, IXI, 8), (CPX, ZPG, 3), (SBC, ZPG, 3), (INC, ZPG, 5), (INS, ZPG, 5), (INX, IMP, 2), (SBC, IMD, 2), (NOP, IMP, 2), (_SB, IMD, 2), (CPX, ABS, 4), (SBC, ABS, 4), (INC, ABS, 6), (INS, ABS, 7), //E
     (BEQ, REL, 2), (SBC, IYI, 5),           HLT, (INS, IYI, 8), (_NP, ZPX, 4), (SBC, ZPX, 4), (INC, ZPX, 6), (INS, ZPX, 6), (SED, IMP, 2), (SBC, ABY, 4), (_NP, IMP, 2), (INS, ABY, 7), (_NP, ABX, 4), (SBC, ABX, 4), (INC, ABX, 7), (INS, ABX, 7)];//F
