use core::cpu::*;
use core::ines::INES;

pub fn parser(address: usize, ines: &INES) -> AsmInstruction {
    let opcode = ines.read(address);
    AsmInstruction::new(opcode)
}
