use core::cpu::*;
use core::ines::INES;

pub fn parser(address: u16, ines: &INES) -> AsmInstruction {
    let opcode = ines.read(address);
    AsmInstruction::new(opcode)
}
