bitflags! {
    pub struct StatusFlags: u8 {
        const C = 0b0000_0001;
        const Z = 0b0000_0010;
        const I = 0b0000_0100;
        const D = 0b0000_1000;
        const B = 0b0001_0000;
        const S = 0b0010_0000;
        const V = 0b0100_0000;
        const N = 0b1000_0000;
    }
}

impl StatusFlags {
    pub fn get_flag(&self, flag: StatusFlags) -> StatusFlags {
        *self & flag
    }

    pub fn flag_position(flag: StatusFlags) -> u8 {
        match flag {
            StatusFlags::C => 0,
            StatusFlags::Z => 1,
            StatusFlags::I => 2,
            StatusFlags::D => 3,
            StatusFlags::B => 4,
            StatusFlags::S => 5,
            StatusFlags::V => 6,
            StatusFlags::N => 7,
            _ => panic!(format!("Flag position of {:?} doesn't make sense!", flag))
        }
    }

    pub fn get_flag_bit(&self, flag: StatusFlags) -> u8 {
        (*self & flag).bits
    }

    pub fn set_flag_bit(&mut self, flag: u8) {
        self.bits = flag;
    }
    
    pub fn status(&self, flag: StatusFlags) -> bool {
        self.get_flag(flag) == flag
    }
}
