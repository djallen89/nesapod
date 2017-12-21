use super::core::parser;

pub fn test_parser() {
    let ines = parser::INES::new("assets/instr_test-v5/rom_singles/01-basics.nes");
    match ines {
        Ok(_) => {},
        Err(f) => panic!("{}", f)
    }
}
