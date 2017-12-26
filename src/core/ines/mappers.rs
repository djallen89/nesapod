#[derive(Clone, Copy, Debug)]
pub enum SxRom {
    MMC1 = 1,
    NO105 = 105,
    NO155 = 155
}

impl SxRom {
    pub fn new(id: u8) -> SxRom {
        if id == 1 {
            SxRom::MMC1
        } else if id == 105 {
            SxRom::NO105
        } else {
            SxRom::NO155
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
    SXROM(SxRom),
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
    pub fn new(id: u8) -> Mapper {
        match id {
            0 => Mapper::NROM,
            001 | 105 | 155 => Mapper::SXROM(SxRom::new(id)),
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
