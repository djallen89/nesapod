use std::fmt;

pub mod sxrom {
    use core::ines::Mirroring;
    use std::fmt;

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
        pub chr_switch_4: bool,
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

        pub fn prg_ram_enabled(&self) -> bool {
            self.prg_ram_enable
        }

        pub fn prg_read(&self, idx: u16) -> usize {
            match self.prg_bank_mode {
                PrgBankMode::Ignore => {
                    /* 32 KB bank mode */
                    let bank = ((self.prg_bank as usize) & 0b01110) << 14;
                    bank + (idx as usize)
                },
                PrgBankMode::First => {
                    let bank = (self.prg_bank as usize) << 14;
                    bank + (idx as usize) + 0xC000
                },
                PrgBankMode::Last => {
                    let bank = (self.prg_bank as usize) << 14;
                    bank + (idx as usize)
                }
            }
        }

        pub fn nametable_mirror(&self, addr: u16) -> u16 {
            match self.mirroring {
                Mirroring::OneScreenLower => addr - 0x2000,
                Mirroring::OneScreenUpper => addr - 0x2000,
                Mirroring::Vertical       => addr % 0x800,
                Mirroring::Horizontal     => ((addr / 2) & 0x400) + (addr % 0x400),
            }
        }

        pub fn set_mirroring(&mut self, mirror: Mirroring) {
            self.mirroring = mirror
        }

        fn chr_bank_0(&self) -> u16 {
            if self.chr_switch_4 {
                (self.chr_bank_0 << 3) as u16 
            } else {
                ((self.chr_bank_0 << 3) & 0b1111_0000) as u16
            }
        }

        fn chr_bank_1(&self) -> u16 {
            (self.chr_bank_1 << 3) as u16
        }

        pub fn chr_read(&self, idx: u16) -> u16 {
            let bank = if self.chr_switch_4 {
                match idx {
                    0x0000 ... 0x0FFF => self.chr_bank_0(),
                    0x1000 ... 0x1FFF => self.chr_bank_1(),
                    _ => panic!("Misuse of SXROM chr read!")
                }
            } else {
                self.chr_bank_0()
            };
            bank + idx
        }

        pub fn write(&mut self, addr: u16, val: u8) -> Mirroring {
            let len = self.sr_len();
            if len < 4 {
                self.shift_register(val)
            } else if len == 4 {
                self.shift_register(val);
                self.set_register(addr)
            } else {
                panic!(format!("Impossible shift register length {}", len))
            }

            self.mirroring
        }
        
        fn sr_len(&self) -> usize {
            let mut bit = 7;
            while self.shift_register >> bit == 0 && bit > 0 {
                bit -= 1;
            }
            bit + 1
        }

        fn shift_register(&mut self, val: u8) {
            if val > 127 {
                self.shift_register = 0;
            } else {
                self.shift_register <<= 1;
                self.shift_register += val & 0x01;
            }
        }

        fn sr_drain(&mut self, mut index: u8) -> u8 {
            let mut res = 0;
            while index > 0 {
                res <<= 1;
                res += self.shift_register & 1;
                self.shift_register >>= 1;
                index -= 1;
            }
            res
        }

        fn set_register(&mut self, addr: u16) {
            match addr {
                0x8000 ... 0x9FFF => {
                    let chr_mode = self.sr_drain(1) == 1;
                    self.chr_switch_4 = chr_mode;

                    let mirroring = self.sr_drain(2);
                    self.mirroring = Mirroring::set(mirroring);

                    let prg_mode = self.sr_drain(2);
                    self.prg_bank_mode = PrgBankMode::set(prg_mode);

                },
                0xA000 ... 0xBFFF => {
                    let val = self.sr_drain(5);
                    self.chr_bank_0 = val;
                },
                0xC000 ... 0xDFFF => {
                    let val = self.sr_drain(5);
                    self.chr_bank_1 = val;
                },
                0xE000 ... 0xFFFF | _ => {
                    let prg_ram_enable = self.sr_drain(1) == 1;
                    let val = self.sr_drain(4);
                    self.prg_bank = val;
                    if self.id == 155 {
                    } else {
                        self.prg_ram_enable = prg_ram_enable;
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
            001 | 105 | 155 | 002 => Mapper::SXROM(sxrom::SxRom::new(id, mirroring, true)),
             094 | 180 => Mapper::UXROM(UxRom::new(id)),
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
