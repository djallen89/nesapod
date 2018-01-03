use std::fmt;

pub mod sxrom {
    use core::cpu::CPUResult;
    use std::fmt;

    #[derive(Clone, Copy, Debug)]
    pub enum Mirroring {
        OneScreenLower,
        OneScreenUpper,
        Vertical,
        Horizontal
    }

    impl Mirroring {
        pub fn set(x: u8) -> Mirroring {
            match x {
                0 => Mirroring::OneScreenLower,
                1 => Mirroring::OneScreenUpper,
                2 => Mirroring::Vertical,
                3 => Mirroring::Horizontal,
                _ => panic!(format!("Invalid mirroring {}!", x))
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub enum PrgBankMode {
        Ignore,
        First,
        Last
    }

    impl PrgBankMode {
        pub fn set(id: u8) -> PrgBankMode {
            match id {
                0 | 1 => PrgBankMode::Ignore,
                2 => PrgBankMode::First,
                3 => PrgBankMode::Last,
                _ => panic!(format!("Invalid id {}!", id))
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct SxRom {
        id: u8,
        mirroring: Mirroring,
        prg_bank_mode: PrgBankMode,
        prg_ram_enable: bool,
        chr_switch_4: bool,
        prg_bank: u8,
        chr_bank_0: u8,
        chr_bank_1: u8,
        shift_register: u8,
    }
        
    impl SxRom {
        pub fn new(id: u8, mirroring: u8, enable: bool) -> SxRom {
            SxRom {
                id: id,
                mirroring: Mirroring::set(mirroring),
                prg_bank_mode: PrgBankMode::set(0),
                prg_ram_enable: enable,
                chr_switch_4: false,
                prg_bank: 0,
                chr_bank_0: 0,
                chr_bank_1: 0,
                shift_register: 0,
            }
        }

        pub fn write(&mut self, addr: u16, val: u8) -> CPUResult<String> {
            let len = self.sr_len();
            if len < 4 {
                self.shift_register(val)
            } else if len == 5 {
                self.shift_register(val)?;
                self.set_register(addr)
            } else {
                panic!(format!("Impossible shift register length {}", len))
            }
        }
        
        fn sr_len(&self) -> usize {
            let mut bit = 7;
            while self.shift_register >> bit == 0 && bit > 0 {
                bit -= 1;
            }
            bit + 1
        }

        fn shift_register(&mut self, val: u8) -> CPUResult<String> {
            if val > 127 {
                self.shift_register = 0;
                Ok(format!("Reset SxROM shift register"))
            } else {
                self.shift_register <<= 1;
                self.shift_register += val & 0x01;
                Ok(format!("Shifted SxROM shift register; {:b}", self.shift_register))
            }
        }

        fn set_register(&mut self, addr: u16) -> CPUResult<String> {
            match addr {
                0x8000 ... 0x9FFF => {
                    let mirroring = self.shift_register & 0b0000_0011;
                    self.mirroring = Mirroring::set(mirroring);
                    let prg_mode = (self.shift_register & 0b0000_1100) >> 2;
                    self.prg_bank_mode = PrgBankMode::set(prg_mode);
                    let chr_mode = self.shift_register >> 4 == 1;
                    self.chr_switch_4 = chr_mode;
                    self.shift_register = 0;
                    Ok(format!("Set control register to {:?} {:?} 4 kB switching: {}",
                               self.mirroring, self.prg_bank_mode, self.chr_switch_4))
                },
                0xA000 ... 0xBFFF => {
                    self.chr_bank_0 = self.shift_register;
                    self.shift_register = 0;
                    Ok(format!("Set chr bank 0 to {:X}", self.chr_bank_0))
                },
                0xC000 ... 0xDFFF => {
                    self.chr_bank_1 = self.shift_register;
                    self.shift_register = 0;
                    Ok(format!("Set chr bank 1 to {:X}", self.chr_bank_0))
                },
                0xE000 ... 0xFFFF | _ => {
                    if self.id == 155 {
                        self.prg_bank = self.shift_register & 0b0000_1111;
                        self.shift_register = 0;
                        Ok(format!("Dind't change prg ram chip enable; 155"))
                    } else {
                        let enable = self.shift_register >> 5 == 1;
                        self.prg_ram_enable = enable;
                        self.prg_bank = self.shift_register & 0b0000_1111;
                        self.shift_register = 0;
                        Ok(format!("Set prg bank to: ram enabled {}, rom bank: {}",
                                   self.prg_ram_enable, self.prg_bank))
                    }
                }
            }
        }
    }

    impl fmt::Display for SxRom {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            writeln!(f, "id: {}, mirroring: {:?}",
                   self.id, self.mirroring)?;
            writeln!(f, "prg bank mode: {:?}, prg ram enabled: {}, prg_bank: {}",
                   self.prg_bank_mode, self.prg_ram_enable, self.prg_bank)?;
            write!(f, "chr switch 4: {}, chr bank 0: {}, chr bank 1: {}",
                   self.chr_switch_4, self.chr_bank_0, self.chr_bank_1)
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UxRom {
    NO002 = 2,
    NO094 = 94,
    NO180 = 180
}

impl UxRom {
    pub fn new(id: u8) -> UxRom {
        if id == 2 {
            UxRom::NO002
        } else if id == 94 {
            UxRom::NO094
        } else {
            UxRom::NO180
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CnRom {
    NoIssue = 3,
    CopyIssue = 185,
}

impl CnRom {
    pub fn new(id: u8) -> CnRom {
        if id == 3 {
            CnRom::NoIssue
        } else {
            CnRom::CopyIssue
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum TxRom {
    MMC3 = 4,
    NO118 = 118,
    NO119 = 119
}

impl TxRom {
    pub fn new(id: u8) -> TxRom {
        if id == 4 {
            TxRom::MMC3
        } else if id == 118 {
            TxRom::NO118
        } else {
            TxRom::NO119
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Ffe {
    NO006 = 6,
    NO008 = 8,
    NO012 = 12,
    NO017 = 17
}

impl Ffe {
    pub fn new(id: u8) -> Ffe {
        match id {
            6 => Ffe::NO006,
            8 => Ffe::NO008,
            12 => Ffe::NO012,
            17 => Ffe::NO017,
            x => panic!(format!("Expected 6, 8, 12 or 17, got {}", x))
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Fcg {
    NO016 = 16,
    NO153 = 153,
    NO159 = 159
}

impl Fcg {
    pub fn new(id: u8) -> Fcg {
        if id == 16 {
            Fcg::NO016
        } else if id == 153 {
            Fcg::NO153
        } else {
            Fcg::NO159
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Vrc {
    VRC4AC = 21,
    VRC2A = 22,
    VRC2B4EF = 23,
    VRC4BD = 25,
    VRC6A = 24,
    VRC6B = 26,
    VRC3 = 73,
    VRC1 = 75,
    VRC7 = 85
}

impl Vrc {
    pub fn new(id: u8) -> Vrc {
        match id {
            21 => Vrc::VRC4AC,
            22 => Vrc::VRC2A,
            23 => Vrc::VRC2B4EF,
            24 => Vrc::VRC6A,
            25 => Vrc::VRC4BD,
            26 => Vrc::VRC6B,
            73 => Vrc::VRC3,
            75 => Vrc::VRC1,
            85 => Vrc::VRC7,
            x => panic!(format!("Expected 21, 22, 23 or 25, got {}", x))
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Mapper {
    NROM,
    SXROM(sxrom::SxRom),
    UXROM(UxRom),
    CNROM(CnRom),
    TXROM(TxRom),
    MMC5,
    FFE(Ffe),
    AxROM,
    MMC2,
    MMC4,
    COLORDREAMS,
    CPROM,
    FCG(Fcg),
    JALECO,
    NAMCO163,
    KONAMIVRC(Vrc),
    BNROM,
    RAMBO064,
    RAMBO158,
    GXROM,
    SUNSOFT4,
    SUNSOFT7,
    CAMERICA,
    DXROM,
    NO210,
    NOTSUPPORTED(u8)
}

impl Mapper {
    pub fn new(id: u8, mirroring: u8) -> Mapper {
        match id {
            0 => Mapper::NROM,
            001 | 105 | 155 => Mapper::SXROM(sxrom::SxRom::new(id, mirroring, true)),
            002 | 094 | 180 => Mapper::UXROM(UxRom::new(id)),
            003 | 185 => Mapper::CNROM(CnRom::new(id)),
            004 | 118 | 119 => Mapper::TXROM(TxRom::new(id)),
            005 => Mapper::MMC5,
            006 | 008 | 012 | 017 => Mapper::FFE(Ffe::new(id)),
            007 => Mapper::AxROM,
            009 => Mapper::MMC2,
            010 => Mapper::MMC4,
            011 => Mapper::COLORDREAMS,
            013 => Mapper::CPROM,
            016 | 153 | 159 => Mapper::FCG(Fcg::new(id)),
            018 => Mapper::JALECO,
            019 => Mapper::NAMCO163,
            21 ... 26 | 73 | 75 | 85 => Mapper::KONAMIVRC(Vrc::new(id)),
            034 => Mapper::BNROM,
            064 => Mapper::RAMBO064,
            158 => Mapper::RAMBO158,
            066 => Mapper::GXROM,
            068 => Mapper::SUNSOFT4,
            069 => Mapper::SUNSOFT7,
            071 => Mapper::CAMERICA,
            206 => Mapper::DXROM,
            210 => Mapper::NO210,
            x => Mapper::NOTSUPPORTED(x)
        }
    }
}


impl fmt::Display for Mapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Mapper::SXROM(ref sxrom) => {
                write!(f, "{}", sxrom)
            },
            x => write!(f, "{:?}", x)
        }
    }
}
