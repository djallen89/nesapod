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
    ZeroPg_X,
    ZeroPg_Y,
    Indirect_X,
    Indirect_Y,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DoubleType {
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect
}

pub fn opcode_table() -> Vec<(Code, Address, u16)> {
    use core::cpu::Code::*;
    let acc = Address::Acc;
    let imp = Address::Implied;
    let ill = (ILLEGAL, Address::Invalid, 0);
    let ixi = Address::Specified(AddressType::SingleByte(SingleType::Indirect_X));
    let iyi = Address::Specified(AddressType::SingleByte(SingleType::Indirect_Y));
    let rel = Address::Specified(AddressType::SingleByte(SingleType::Relative));
    let imd = Address::Specified(AddressType::SingleByte(SingleType::Immediate));
    let zpg = Address::Specified(AddressType::SingleByte(SingleType::ZeroPg));
    let zpx = Address::Specified(AddressType::SingleByte(SingleType::ZeroPg_X));
    let zpy = Address::Specified(AddressType::SingleByte(SingleType::ZeroPg_Y));
    let ind = Address::Specified(AddressType::DoubleByte(DoubleType::Indirect));
    let abs = Address::Specified(AddressType::DoubleByte(DoubleType::Absolute));
    let abx = Address::Specified(AddressType::DoubleByte(DoubleType::Absolute_X));
    let aby = Address::Specified(AddressType::DoubleByte(DoubleType::Absolute_Y));
    //              0              1               2    3              4             5              6     7             8              9              A     B              C             D              E     F
    vec![(BRK, imp, 7), (ORA, ixi, 6),           ill, ill,           ill, (ORA, zpg, 3), (ASL, zpg, 5), ill, (PHP, imp, 3), (ORA, imd, 2), (ASL, acc, 2), ill,           ill, (ORA, abs, 4), (ASL, abs, 6), ill, // 0
         (BPL, rel, 2), (ORA, iyi, 5),           ill, ill,           ill, (ORA, zpx, 4), (ASL, zpx, 6), ill, (CLC, imp, 2), (ORA, aby, 4),           ill, ill,           ill, (ORA, abx, 4), (ASL, abx, 7), ill, // 1
         (JSR, abs, 6), (AND, ixi, 6),           ill, ill, (BIT, zpg, 3), (AND, zpg, 3), (ROL, zpg, 5), ill, (PLP, imp, 4), (AND, imd, 2), (ROL, acc, 2), ill, (BIT, abs, 4), (AND, abs, 4), (ROL, abs, 6), ill, // 2
         (BMI, rel, 2), (AND, iyi, 5),           ill, ill,           ill, (AND, zpx, 4), (ROL, zpx, 6), ill, (SEC, imp, 2), (AND, aby, 4),           ill, ill,           ill, (AND, abx, 4), (ROL, abx, 7), ill, // 3
         (RTI, imp, 6), (EOR, ixi, 6),           ill, ill,           ill, (EOR, zpg, 3), (LSR, zpg, 5), ill, (PHA, imp, 3), (EOR, imd, 2), (LSR, acc, 2), ill, (JMP, abs, 3), (EOR, abs, 4), (LSR, abs, 6), ill, // 4
         (BVC, rel, 2), (EOR, iyi, 5),           ill, ill,           ill, (EOR, zpx, 4), (LSR, zpx, 6), ill, (CLI, imp, 2), (EOR, aby, 4),           ill, ill,           ill, (EOR, abx, 4), (LSR, abx, 7), ill, // 5
         (RTS, imp, 6), (ADC, ixi, 6),           ill, ill,           ill, (ADC, zpg, 3), (ROR, zpg, 5), ill, (PLA, imp, 4), (ADC, imd, 2), (ROR, acc, 2), ill, (JMP, ind, 5), (ADC, abs, 4), (ROR, abs, 6), ill, // 6
         (BVS, rel, 2), (ADC, iyi, 5),           ill, ill,           ill, (ADC, zpx, 4), (ROR, zpx, 6), ill, (SEI, imp, 2), (ADC, aby, 5),           ill, ill,           ill, (ADC, abx, 4), (ROR, abx, 7), ill, // 7
         {        ill}, (STA, ixi, 6),           ill, ill, (STY, zpg, 3), (STA, zpg, 3), (STX, zpg, 3), ill, (DEY, imp, 2),           ill, (TXA, imp, 2), ill, (STY, abs, 4), (STA, abs, 4), (STX, abs, 4), ill, // 8
         (BCC, rel, 2), (STA, iyi, 6),           ill, ill, (STY, zpx, 4), (STA, zpx, 4), (STX, zpy, 4), ill, (TYA, imp, 2), (STA, aby, 5), (TXS, imp, 2), ill,           ill, (STA, abx, 5),           ill, ill, // 9
         (LDY, imd, 2), (LDA, ixi, 6), (LDX, imd, 2), ill, (LDY, zpg, 3), (LDA, zpg, 3), (LDX, zpg, 3), ill, (TAY, imp, 2), (LDA, imd, 2), (TAX, imp, 2), ill, (LDY, abs, 4), (LDA, abs, 4), (LDX, abs, 4), ill, // A
         (BCS, rel, 2), (LDA, iyi, 5),           ill, ill, (LDY, zpx, 4), (LDA, zpx, 4), (LDX, zpy, 4), ill, (CLV, imp, 2), (LDA, aby, 4), (TSX, imp, 2), ill, (LDY, abx, 4), (LDA, abx, 4), (LDX, aby, 4), ill, // B
         (CPY, imd, 2), (CMP, ixi, 6),           ill, ill, (CPY, zpg, 3), (CMP, zpg, 3), (DEC, zpg, 5), ill, (INY, imp, 2), (CMP, imd, 2), (DEX, imp, 2), ill, (CPY, abs, 4), (CMP, abs, 4), (DEC, abs, 6), ill, // C
         (BNE, rel, 2), (CMP, iyi, 5),           ill, ill,           ill, (CMP, zpx, 4), (DEC, zpx, 6), ill, (CLD, imp, 2), (CMP, aby, 4),           ill, ill,           ill, (CMP, abx, 4), (DEC, abx, 7), ill, // D
         (CPX, imd, 2), (SBC, ixi, 6),           ill, ill, (CPX, zpg, 3), (SBC, zpg, 3), (INC, zpg, 5), ill, (INX, imp, 2), (SBC, imd, 2), (NOP, imp, 2), ill, (CPX, abs, 4), (SBC, abs, 4), (INC, abs, 6), ill, // E
         (BEQ, rel, 2), (SBC, iyi, 5),           ill, ill,           ill, (SBC, zpx, 4), (INC, zpx, 6), ill, (SED, imp, 2), (SBC, aby, 4),           ill, ill,           ill, (SBC, abx, 4), (INC, abx, 7), ill] // F
}


