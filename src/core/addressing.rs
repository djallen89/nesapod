use core::cpu::Code;

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
const ILL: (Code, Address, u16) = (Code::ILL, Address::Invalid, 0);
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
//                    0                    1                     2    3                    4                   5                    6     7                   8                    9                    A     B                    C                   D                    E     F
    [(Code::BRK, IMP, 7), (Code::ORA, IXI, 6),                 ILL, ILL,                 ILL, (Code::ORA, ZPG, 3), (Code::ASL, ZPG, 5), ILL, (Code::PHP, IMP, 3), (Code::ORA, IMD, 2), (Code::ASL, ACC, 2), ILL,                 ILL, (Code::ORA, ABS, 4), (Code::ASL, ABS, 6), ILL, //0
     (Code::BPL, REL, 2), (Code::ORA, IYI, 5),                 ILL, ILL,                 ILL, (Code::ORA, ZPX, 4), (Code::ASL, ZPX, 6), ILL, (Code::CLC, IMP, 2), (Code::ORA, ABY, 4),                 ILL, ILL,                 ILL, (Code::ORA, ABX, 4), (Code::ASL, ABX, 7), ILL, //1
     (Code::JSR, ABS, 6), (Code::AND, IXI, 6),                 ILL, ILL, (Code::BIT, ZPG, 3), (Code::AND, ZPG, 3), (Code::ROL, ZPG, 5), ILL, (Code::PLP, IMP, 4), (Code::AND, IMD, 2), (Code::ROL, ACC, 2), ILL, (Code::BIT, ABS, 4), (Code::AND, ABS, 4), (Code::ROL, ABS, 6), ILL, //2
     (Code::BMI, REL, 2), (Code::AND, IYI, 5),                 ILL, ILL,                 ILL, (Code::AND, ZPX, 4), (Code::ROL, ZPX, 6), ILL, (Code::SEC, IMP, 2), (Code::AND, ABY, 4),                 ILL, ILL,                 ILL, (Code::AND, ABX, 4), (Code::ROL, ABX, 7), ILL, //3
     (Code::RTI, IMP, 6), (Code::EOR, IXI, 6),                 ILL, ILL,                 ILL, (Code::EOR, ZPG, 3), (Code::LSR, ZPG, 5), ILL, (Code::PHA, IMP, 3), (Code::EOR, IMD, 2), (Code::LSR, ACC, 2), ILL, (Code::JMP, ABS, 3), (Code::EOR, ABS, 4), (Code::LSR, ABS, 6), ILL, //4
     (Code::BVC, REL, 2), (Code::EOR, IYI, 5),                 ILL, ILL,                 ILL, (Code::EOR, ZPX, 4), (Code::LSR, ZPX, 6), ILL, (Code::CLI, IMP, 2), (Code::EOR, ABY, 4),                 ILL, ILL,                 ILL, (Code::EOR, ABX, 4), (Code::LSR, ABX, 7), ILL, //5
     (Code::RTS, IMP, 6), (Code::ADC, IXI, 6),                 ILL, ILL,                 ILL, (Code::ADC, ZPG, 3), (Code::ROR, ZPG, 5), ILL, (Code::PLA, IMP, 4), (Code::ADC, IMD, 2), (Code::ROR, ACC, 2), ILL, (Code::JMP, IND, 5), (Code::ADC, ABS, 4), (Code::ROR, ABS, 6), ILL, //6
     (Code::BVS, REL, 2), (Code::ADC, IYI, 5),                 ILL, ILL,                 ILL, (Code::ADC, ZPX, 4), (Code::ROR, ZPX, 6), ILL, (Code::SEI, IMP, 2), (Code::ADC, ABY, 5),                 ILL, ILL,                 ILL, (Code::ADC, ABX, 4), (Code::ROR, ABX, 7), ILL, //7
     {              ILL}, (Code::STA, IXI, 6),                 ILL, ILL, (Code::STY, ZPG, 3), (Code::STA, ZPG, 3), (Code::STX, ZPG, 3), ILL, (Code::DEY, IMP, 2),                 ILL, (Code::TXA, IMP, 2), ILL, (Code::STY, ABS, 4), (Code::STA, ABS, 4), (Code::STX, ABS, 4), ILL, //8
     (Code::BCC, REL, 2), (Code::STA, IYI, 6),                 ILL, ILL, (Code::STY, ZPX, 4), (Code::STA, ZPX, 4), (Code::STX, ZPY, 4), ILL, (Code::TYA, IMP, 2), (Code::STA, ABY, 5), (Code::TXS, IMP, 2), ILL,                 ILL, (Code::STA, ABX, 5),                 ILL, ILL, //9
     (Code::LDY, IMD, 2), (Code::LDA, IXI, 6), (Code::LDX, IMD, 2), ILL, (Code::LDY, ZPG, 3), (Code::LDA, ZPG, 3), (Code::LDX, ZPG, 3), ILL, (Code::TAY, IMP, 2), (Code::LDA, IMD, 2), (Code::TAX, IMP, 2), ILL, (Code::LDY, ABS, 4), (Code::LDA, ABS, 4), (Code::LDX, ABS, 4), ILL, //A
     (Code::BCS, REL, 2), (Code::LDA, IYI, 5),                 ILL, ILL, (Code::LDY, ZPX, 4), (Code::LDA, ZPX, 4), (Code::LDX, ZPY, 4), ILL, (Code::CLV, IMP, 2), (Code::LDA, ABY, 4), (Code::TSX, IMP, 2), ILL, (Code::LDY, ABX, 4), (Code::LDA, ABX, 4), (Code::LDX, ABY, 4), ILL, //B
     (Code::CPY, IMD, 2), (Code::CMP, IXI, 6),                 ILL, ILL, (Code::CPY, ZPG, 3), (Code::CMP, ZPG, 3), (Code::DEC, ZPG, 5), ILL, (Code::INY, IMP, 2), (Code::CMP, IMD, 2), (Code::DEX, IMP, 2), ILL, (Code::CPY, ABS, 4), (Code::CMP, ABS, 4), (Code::DEC, ABS, 6), ILL, //C
     (Code::BNE, REL, 2), (Code::CMP, IYI, 5),                 ILL, ILL,                 ILL, (Code::CMP, ZPX, 4), (Code::DEC, ZPX, 6), ILL, (Code::CLD, IMP, 2), (Code::CMP, ABY, 4),                 ILL, ILL,                 ILL, (Code::CMP, ABX, 4), (Code::DEC, ABX, 7), ILL, //D
     (Code::CPX, IMD, 2), (Code::SBC, IXI, 6),                 ILL, ILL, (Code::CPX, ZPG, 3), (Code::SBC, ZPG, 3), (Code::INC, ZPG, 5), ILL, (Code::INX, IMP, 2), (Code::SBC, IMD, 2), (Code::NOP, IMP, 2), ILL, (Code::CPX, ABS, 4), (Code::SBC, ABS, 4), (Code::INC, ABS, 6), ILL, //E
     (Code::BEQ, REL, 2), (Code::SBC, IYI, 5),                 ILL, ILL,                 ILL, (Code::SBC, ZPX, 4), (Code::INC, ZPX, 6), ILL, (Code::SED, IMP, 2), (Code::SBC, ABY, 4),                 ILL, ILL,                 ILL, (Code::SBC, ABX, 4), (Code::INC, ABX, 7), ILL];//F


