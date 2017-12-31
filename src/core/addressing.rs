use core::cpu::Code;

#[derive(Debug, Clone, Copy)]
pub enum Address {
    Spec(AddressType),
    Implied,
    Acc,
    Invalid,
}

impl Address {
    pub fn bytes(&self) -> u16 {
        match *self {
            Address::Spec(ref x) => x.bytes(),
            _ => 0
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AddressType {
    Single(SingleType),
    Double(DoubleType)
}

impl AddressType {
    pub fn bytes(&self) -> u16 {
        match *self {
            AddressType::Single(_) => 1,
            _ => 2
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum SingleType {
    Imdiat,
    Reltiv,
    ZeroPg,
    ZPIdxX,
    ZPIdxY,
    IdxInd,
    IndIdx,
}

#[derive(Debug, Clone, Copy)]
pub enum DoubleType {
    Absolt,
    IdxedX,
    IdxedY,
    Indrct
}

pub fn opcode_table() -> Vec<(Code, Address, u16)> {
    use core::cpu::Code::*;
    use core::addressing::Address::*;
    use core::addressing::AddressType::*;
    use core::addressing::SingleType::*;
    use core::addressing::DoubleType::*;
    let imp = Implied;
    let ill = (ILLEGAL, Invalid, 0);
    let ind = Spec(Double(Indrct));
    let ixi = Spec(Single(IdxInd));
    let iyi = Spec(Single(IndIdx));
    let rel = Spec(Single(Reltiv));
    let abs = Spec(Double(Absolt));
    let imd = Spec(Single(Imdiat));
    let zpg = Spec(Single(ZeroPg));
    let zpx = Spec(Single(ZPIdxX));
    let zpy = Spec(Single(ZPIdxY));
    let abx = Spec(Double(IdxedX));
    let aby = Spec(Double(IdxedY));
    vec![(BRK, imp, 7), (ORA, ixi, 6),           ill,           ill, (ORA, zpg, 3), (ASL, zpg, 5), (PHP, imp, 3), (ORA, imd, 2), (ASL, Acc, 2),           ill, (ORA, abs, 4), (ASL, abs, 6), ill,
         (BPL, rel, 2), (ORA, iyi, 5),           ill,           ill, (ORA, zpx, 4), (ASL, zpx, 6), (CLC, imp, 2), (ORA, aby, 4),           ill,           ill, (ORA, abx, 4), (ASL, abx, 7), ill,
         (JSR, abs, 6), (AND, ixi, 6),           ill, (BIT, zpg, 3), (AND, zpg, 3), (ROL, zpg, 5), (PLP, imp, 4), (AND, imd, 2), (ROL, Acc, 2), (BIT, abs, 4), (AND, abs, 4), (ROL, abs, 6), ill,
         (BMI, rel, 2), (AND, iyi, 5),           ill,           ill, (AND, zpx, 4), (ROL, zpx, 6), (SEC, imp, 2), (AND, aby, 4),           ill,           ill, (AND, abx, 4), (ROL, abx, 7), ill,
         (RTI, imp, 6), (EOR, ixi, 6),           ill,           ill, (EOR, zpg, 3), (LSR, zpg, 5), (PHA, imp, 3), (EOR, imd, 2), (LSR, Acc, 2), (JMP, abs, 3), (EOR, abs, 4), (LSR, abs, 6), ill,
         (BVC, rel, 2), (EOR, iyi, 5),           ill,           ill, (EOR, zpx, 4), (LSR, zpx, 6), (CLI, imp, 2), (EOR, aby, 4),           ill,           ill, (EOR, abx, 4), (LSR, abx, 7), ill,
         (RTS, imp, 6), (ADC, ixi, 6),           ill,           ill, (ADC, zpg, 3), (ROR, zpg, 5), (PLA, imp, 4), (ADC, imd, 2), (ROR, Acc, 2), (JMP, ind, 5), (ADC, abs, 4), (ROR, abs, 6), ill,
         (BVS, rel, 2), (ADC, iyi, 5),           ill,           ill, (ADC, zpx, 4), (ROR, zpx, 6), (SEI, imp, 2), (ADC, aby, 5),           ill,           ill, (ADC, abx, 4), (ROR, abx, 7), ill,
         {        ill}, (STA, ixi, 6),           ill, (STY, zpg, 3), (STA, zpg, 3), (STX, zpg, 3), (DEY, imp, 2),           ill, (TXA, imp, 2), (STY, abs, 4), (STA, abs, 4), (STX, abs, 4), ill,
         (BCC, rel, 2), (STA, iyi, 6),           ill, (STY, zpx, 4), (STA, zpx, 4), (STX, zpy, 4), (TYA, imp, 2), (STA, aby, 5), (TXS, imp, 2),           ill, (STA, abx, 5),           ill, ill,
         (LDY, imd, 2), (LDA, ixi, 6), (LDX, imd, 2), (LDY, zpg, 3), (LDA, zpg, 3), (LDX, zpg, 3), (TAY, imp, 2), (LDA, imd, 2), (TAX, imp, 2), (LDY, abs, 4), (LDA, abs, 4), (LDX, abs, 4), ill,
         (BCS, rel, 2), (LDA, iyi, 5),           ill, (LDY, zpx, 4), (LDA, zpx, 4), (LDX, zpy, 4), (CLV, imp, 2), (LDA, aby, 4), (TSX, imp, 2), (LDY, abx, 4), (LDA, abx, 4), (LDX, aby, 4), ill,
         (CPY, imd, 2), (CMP, ixi, 6),           ill, (CPY, zpg, 3), (CMP, zpg, 3), (DEC, zpg, 5), (INY, imp, 2), (CMP, imd, 2), (DEX, imp, 2), (CPY, abs, 4), (CMP, abs, 4), (DEC, abs, 6), ill,
         (BNE, rel, 2), (CMP, iyi, 5),           ill,           ill, (CMP, zpx, 4), (DEC, zpx, 6), (CLD, imp, 2), (CMP, aby, 4),           ill,           ill, (CMP, abx, 4), (DEC, abx, 7), ill,
         (CPX, imd, 2), (SBC, ixi, 6),           ill, (CPX, zpg, 3), (SBC, zpg, 3), (INC, zpg, 5), (INX, imp, 2), (SBC, imd, 2), (NOP, imp, 2), (CPX, abs, 4), (SBC, abs, 4), (INC, abs, 6), ill,
         (BEQ, rel, 2), (SBC, iyi, 5),           ill,           ill, (SBC, zpx, 4), (INC, zpx, 6), (SED, imp, 2), (SBC, aby, 4),           ill,           ill, (SBC, abx, 4), (INC, abx, 7), ill]
}


