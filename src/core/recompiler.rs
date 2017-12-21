bitflags! {
    struct Flags: u8 {
        const C = 0b0000_0001;
        const Z = 0b0000_0010;
        const I = 0b0000_0100;
        const D = 0b0000_1000;
        const SL = 0b0001_0000;
        const SH = 0b0010_0000;
        const V = 0b0100_0000;
        const N = 0b1000_0000;
    }
}

pub enum Registers {
    ProgramCounter(u16),
    StackPointer(u8),
    Accumulator(u8),
    IDX(u8),
    IDY(u8),
    Status(Flags)
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

bitflags! {
    pub struct Opcode: u8 {
        const LOW = 0b0000_1111;
        const HIGH = 0b1111_0000;
    }
}

pub enum Operand {
    Single(u8),
    Double(u8,u8)
}

pub struct CpuInstruction {
    opcode: Opcode,
    operand: Operand
}

pub struct AsmInstruction {
    code: Code,
    admode: AddressMode
}



