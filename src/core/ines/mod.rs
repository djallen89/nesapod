use std::fs::File;
use std::io::Bytes;
use std::io::Error as IOError;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::str;
use core::ines::mappers::Mapper;
use core::ines::mappers::Mapper::*;
use core::ines::mappers::*;
use core::cpu::CPUResult;

mod mappers; 

const MIN_INES_SIZE: u64 = 16 + 16384 + 8192;
const INES_MAGIC_CODE: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

bitflags! {
    pub struct Flags6: u8 {
        const MIRROR = 0b0000_0001;
        const PRG_RAM = 0b0000_0010;
        const FOURSCREEN = 0b0000_1000;
        const LOWERMAPPER = 0b1111_0000;
    }
}

bitflags! {
    pub struct Flags7: u8 {
        const VS_UNISYSTEM = 0b0000_0001;
        const _NESTWOFORMAT = 0b0000_1100;
        const UPPERMAPPER = 0b1111_0000;
    }
}

bitflags! {
    pub struct Flags9: u8 {
        const _TV_SYSTEM = 0b0000_0001;
        const _RESERVED = 0b1111_1110;
    }
}

bitflags! {
    pub struct Flags10: u8 {
        const _TV_SYSTEM = 0b0000_0011;
        const _PRG_RAM = 0b0001_0000;
        const BUS_CONFLICTS = 0b010_0000;
    }
}

pub enum Mirroring {
    Horizontal,
    Vertical,
    SingleScreen,
    FourScreenVRAM
}

pub struct Header {
    prg_rom: u8,
    prg_ram: u8,
    pub chr_rom: u8,
    pub chr_ram: u8,
    flags_6: u8,
    flags_7: u8,
    flags_9: u8,
    flags_10: u8
}


/// prg_rom is number of 16384 byte tables, chr_rom/chr_ram is number of
impl Header {
    pub fn new(reader: &mut Bytes<File>) -> Result<Header, IOError> {
        let mut header = Header {
            prg_rom: 0,
            prg_ram: 0,
            chr_rom: 0,
            chr_ram: 0,
            flags_6: 0,
            flags_7: 0,
            flags_9: 0,
            flags_10: 0,
        };
        
        let mut magic = [0u8; 4];
        for n in 0..4 {
            magic[n] = reader.next().unwrap()?;
        }
        
        if magic != INES_MAGIC_CODE {
            return Err(IOError::new(
                ErrorKind::Other,
                format!("Expected `NES' + MS-DOS EOF, got {:?}", magic)));
        }

        header.prg_rom = reader.next().unwrap()?;
        match reader.next().unwrap()? {
            0 => header.chr_ram = 1,
            x => header.chr_rom = x
        };
        
        header.flags_6 = reader.next().unwrap()?;
        header.flags_7 = reader.next().unwrap()?;

        header.prg_ram = match reader.next().unwrap()? {
            0 => 1, //compatibility, see PRG RAM circuit
            x => x 
        };
        
        header.flags_9 = reader.next().unwrap()?;
        header.flags_10 = reader.next().unwrap()?;
        
        let mut padding = [0u8; 5];
        for n in 0..5 {
            padding[n] = reader.next().unwrap()?;
        }

        //discard 10flags
        if padding != [0, 0, 0, 0, 0] { 
            Err(IOError::new(
                ErrorKind::Other,
                format!("Expected 5 0 bytes, found {:?}", padding)))
        } else {
            Ok(header)
        }
    }

    pub fn prg_rom_size(&self) -> usize {
        self.prg_rom as usize * 16384
    }

    pub fn prg_ram_size(&self) -> usize {
        self.prg_ram as usize * 8192
    }
    
    /// Create representation of ROM with filled rest of file. On completion file should
    /// be exhausted.
    pub fn fill_mem(&self, size: usize, fbytes: &mut Bytes<File>) -> CPUResult<Vec<u8>> {
        let mut data = Vec::with_capacity(size);
        for i in 0..size {
            match fbytes.next() {
                Some(x) => match x {
                    Ok(x) => data.push(x),
                    Err(f) => return Err(format!("Unexpected file end at byte {}", i))
                },
                None => return Err(format!("Unexpected file end at byte {}", i))
            }
        }

        if data.len() == size {
            Ok(data)
        } else {
            Err(format!("Expected {} bytes, found {}", size, data.len()))
        }
    }

    pub fn vs_unisystem(&self) -> bool {
        self.flags_7 & Flags7::VS_UNISYSTEM.bits == 1
    }

    pub fn bus_conflicts(&self) -> bool {
        self.flags_10 & Flags10::BUS_CONFLICTS.bits == 1
    }

    pub fn mirroring(&self) -> Mirroring {
        match (self.flags_6 & Flags6::MIRROR.bits,
               self.flags_6 & Flags6::FOURSCREEN.bits) {
            (0, 0) => Mirroring::Horizontal,
            (1, 0) => Mirroring::Vertical,
            (_, 1) | _ => Mirroring::FourScreenVRAM,
        }
    }

    pub fn mapper(&self) -> Mapper {
        let lower = self.flags_6 & Flags6::LOWERMAPPER.bits >> 4;
        let upper = self.flags_7 & Flags7::UPPERMAPPER.bits;
        let id = upper + lower;
        Mapper::new(id)
    }
}

pub struct INES {
    _vs_unisystem: bool,
    _bus_conflicts: bool,
    prg_rom_size: usize,
    chr_mem_size: usize,
    _mirroring: Mirroring,
    mapper: Mapper,
    prg_rom: Vec<u8>,
    prg_ram: Vec<u8>,
    chr_mem: Vec<u8>,
}

impl INES {
    pub fn init_ram(cap: usize) -> Vec<u8> {
        let mut ram = Vec::with_capacity(cap);
        for _ in 0 .. cap {
            ram.push(0)
        }
        ram
    }
    
    pub fn new(path: &str) -> CPUResult<INES> {
        let file = match File::open(path) {
            Ok(x) => x,
            Err(f) => return Err(format!("{:?}", f))
        };
        let metadata = match file.metadata() {
            Ok(x) => x,
            Err(f) => return Err(format!("{:?}", f))
        };
        if metadata.len() < MIN_INES_SIZE {
            return Err(format!("File is smaller than minimum possible size!"))
        }
        let mut filebytes = file.bytes();
        let header = match Header::new(&mut filebytes) {
            Ok(x) => x,
            Err(f) => return Err(format!("{:?}", f))
        };
        let prg_rom = header.fill_mem(header.prg_rom_size(), &mut filebytes)?;
        let chr_mem = match (header.chr_rom, header.chr_ram) {
            (x, 0) => header.fill_mem((x as usize) * 8192, &mut filebytes)?,
            (0, x) => INES::init_ram((x as usize) * 8192),
            (x, y) => panic!("Both chr_rom and chr_rm {} {} should not happen!")
        };
        let prg_ram = INES::init_ram(header.prg_ram_size());

        Ok(INES {
            _vs_unisystem: header.vs_unisystem(),
            _bus_conflicts: header.bus_conflicts(),
            prg_rom_size: header.prg_rom_size(),
            chr_mem_size: chr_mem.len(),
            _mirroring: header.mirroring(),
            mapper: header.mapper(),
            prg_rom: prg_rom,
            prg_ram: prg_ram,
            chr_mem: chr_mem
        })
    }

    pub fn size(&self) -> usize {
        self.prg_rom_size + self.chr_mem_size
    }

    pub fn read(&self, idx: u16) -> CPUResult<u8> {
        match self.mapper {
            NROM => {
                match idx {
                    0x0000 ... 0x5FFF => Err(format!("{:x} not on cartridge", idx)),
                    0x6000 ... 0x7FFF => Ok(self.prg_ram[(idx - 0x6000) as usize]),
                    0x8000 ... 0xFFFF => {
                        Ok(self.prg_rom[((idx - 0x8000) as usize) % self.prg_rom_size])
                    },
                    _ => panic!("This should not happen with u16")
                }
            },
            SXROM(SxRom::MMC1) => {
                match idx {
                    0x0000 ... 0x5FFF => Err(format!("{:x} not on cartridge", idx)),
                    0x6000 ... 0x7FFF => Ok(self.prg_ram[(idx - 0x6000) as usize]),
                    0x8000 ... 0xFFFF => {
                        Ok(self.prg_rom[((idx - 0x8000) as usize) % self.prg_rom_size])
                    },
                    _ => panic!("This should not happen with u16")
                }
            },
            x => Err(format!("{:?} not covered yet", x))
        }
    }
    
    pub fn write(&mut self, idx: u16, val: u8) -> CPUResult<String> {
        match self.mapper {
            NROM => {
                match idx {
                    0x0000 ... 0x5FFF => Err(format!("{:x} not on cartridge", idx)),
                    0x6000 ... 0x7FFF => {
                        self.prg_ram[(idx - 0x6000) as usize] = val;
                        Ok(format!("Wrote {:x} to address {:x}", val, idx))
                    },
                    0x7FFF ... 0xFFFF => Err(format!("Can't write to {:x}!", idx)),
                    _ => panic!("This should not happen with u16")
                }
            },
            SXROM(SxRom::MMC1) => {
                Err(format!("adfasdf"))
            },
            x => Err(format!("{:?} not covered yet", x))
        }
    }

    pub fn ppu_read(&self) {
    }
    
    pub fn ppu_write(&self) {
        
    }
    
    pub fn mapper(&self) -> Mapper {
        self.mapper.clone()
    }
}
