use std::fs::File;
use std::io::Bytes;
use std::io::Error as IOError;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::str;
use core::ines::mappers::Mapper;
use core::cpu::CPUResult;

mod mappers; 

const MIN_INES_SIZE: u64 = 16 + 16384 + 8192;
const INES_MAGIC_CODE: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];

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
    FourScreenVRAM
}

pub struct Header {
    prg_rom: u8,
    prg_ram: u8,
    chr_rom: u8,
    chr_ram: u8,
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
        let chr_mem = reader.next().unwrap()?;
        match chr_mem {
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

    pub fn chr_mem_size(&self) -> usize {
        (self.chr_rom + self.chr_ram) as usize * 8192
    }

    /// Create representation of ROM with filled rest of file. On completion file should
    /// be exhausted.
    pub fn fill_mem(&self, filebytes: &mut Bytes<File>) -> Result<Vec<u8>, IOError> {
        let size = self.prg_rom_size() + (self.chr_rom as usize) * 8192;
        let mut data = Vec::with_capacity(size);
        for i in 0..size {
            let hexpair = match filebytes.next() {
                Some(x) => x?,
                None => {
                    return Err(
                        IOError::new(ErrorKind::Other,
                                     format!("Unexpected file end at byte {}", i)))
                }
            };
            data.push(hexpair);
        }

        if data.len() == size {
            Ok(data)
        } else {
            Err(IOError::new(
                ErrorKind::Other,
                format!("Expected {} bytes, found {}", size, data.len())))
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
            _ => Mirroring::FourScreenVRAM,
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
    vs_unisystem: bool,
    bus_conflicts: bool,
    prg_rom_size: usize,
    chr_mem_size: usize,
    mirroring: Mirroring,
    mapper: Mapper,
    data: Vec<u8>,
}

impl INES {
    pub fn new(path: &str) -> Result<INES, IOError> {
        let file = File::open(path)?;
        let metadata = file.metadata()?;
        if metadata.len() < MIN_INES_SIZE {
            return Err(IOError::new(ErrorKind::Other,
                                    "File is smaller than minimum possible size!"))
        }
        let mut filebytes = file.bytes();
        let header = Header::new(&mut filebytes)?;
        let data = header.fill_mem(&mut filebytes)?;

        Ok(INES {
            vs_unisystem: header.vs_unisystem(),
            bus_conflicts: header.bus_conflicts(),
            prg_rom_size: header.prg_rom_size(),
            chr_mem_size: header.chr_mem_size(),
            mirroring: header.mirroring(),
            mapper: header.mapper(),
            data: data
        })
    }

    pub fn size(&self) -> usize {
        self.prg_rom_size + self.chr_mem_size
    }

    pub fn read(&self, idx: usize) -> CPUResult<u8> {
        let addr = self.mapper.read_address(idx) - self.chr_mem_size;
        Ok(self.data[addr])
    }
    
    pub fn write(&mut self, idx: usize, val: u8) -> CPUResult<()> {
        let addr = self.mapper.write_address(idx)?;
        self.data[addr] = val;
        Ok(())
    }
    
    pub fn mapper(&self) -> Mapper {
        self.mapper.clone()
    }
}
