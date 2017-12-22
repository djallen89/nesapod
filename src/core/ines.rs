use std::fs::File;
use std::io::Bytes;
use std::io::Error as IOError;
use std::io::ErrorKind;
use std::io::prelude::*;
use std::str;

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
        const TRAINER = 0b0000_0100;
        const FOURSCREEN = 0b0000_1000;
        const LOWERMAPPER = 0b1111_0000;
    }
}

bitflags! {
    pub struct Flags7: u8 {
        const VS_UNISYSTEM = 0b0000_0001;
        const PLAYCHOICE10 = 0b0000_0010;
        const NESTWOFORMAT = 0b0000_1100;
        const UPPERMAPPER = 0b1111_0000;
    }
}

bitflags! {
    pub struct Flags9: u8 {
        const TV_SYSTEM = 0b0000_0001;
        const RESERVED = 0b1111_1110;
    }
}

#[derive(Clone, Copy)]
pub enum ChrMem {
    ROM,
    RAM
}

pub struct Header {
    prg_rom: u8,
    chr_rom: Option<u8>,
    chr_ram: Option<u8>,
    flags_6: u8,
    flags_7: u8,
    prg_ram: u8,
    flags_9: u8,
}

impl Header {
    pub fn new(reader: &mut Bytes<File>) -> Result<Header, IOError> {
        let mut header = Header {
            prg_rom: 0,
            chr_rom: None,
            chr_ram: None,
            flags_6: 0,
            flags_7: 0,
            prg_ram: 0,
            flags_9: 0
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
            0 => header.chr_ram = Some(1),
            x => header.chr_rom = Some(x)
        };
        header.flags_6 = reader.next().unwrap()?;
        header.flags_7 = reader.next().unwrap()?;
        header.prg_ram = match reader.next().unwrap()? {
            0 => 1,
            x => x //compatibility, see PRG RAM circuit
        };
        header.flags_9 = reader.next().unwrap()?;
        let mut padding = [0u8; 6];
        for n in 0..6 {
            padding[n] = reader.next().unwrap()?;
        }

        //discard 10flags
        if padding[1..] != [0, 0, 0, 0, 0] { 
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
        match (self.chr_rom, self.chr_ram) {
            (Some(x), None) | (None, Some(x)) => x as usize * 8192,
            (Some(_), Some(_)) | (None, None) => panic!("Expected either only rom or only ram size!")           
        }
    }

    pub fn chr_mem_type(&self) -> ChrMem {
        match (self.chr_rom.is_some(), self.chr_ram.is_some()) {
            (true, false) => ChrMem::ROM,
            (false, true) => ChrMem::RAM,
            (_, _) => panic!("Expected either only rom or only ram to be present!")
        }
    }

    pub fn make_mem(&self, prg: bool, filebytes: &mut Bytes<File>) -> Result<Vec<u8>, IOError> {
        let size = if prg {
            let size = self.prg_rom_size();
            size
        } else {
            let size = self.chr_mem_size();
            size
        };

        let mut data = Vec::with_capacity(size);
        for i in 0..size {
            let hexpair = filebytes.next().unwrap()?;
            data.push(hexpair);
        }
        Ok(data)
    }

    pub fn prg_rom_data(&self, filebytes: &mut Bytes<File>) -> Result<Vec<u8>, IOError> {
        self.make_mem(true, filebytes)
    }

    pub fn chr_mem_data(&self, filebytes: &mut Bytes<File>) -> Result<Vec<u8>, IOError> {
        self.make_mem(false, filebytes)
    }
}    

pub struct INES {
    header: Header,
    prg_rom_data: Vec<u8>,
    chr_mem_data: Vec<u8>,
    chr_mem_type: ChrMem,
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
        let prg_rom_data = header.prg_rom_data(&mut filebytes)?;
        let chr_mem_data = header.chr_mem_data(&mut filebytes)?;
        let chr_mem_type = header.chr_mem_type();

        Ok(INES {
            header: header,
            prg_rom_data: prg_rom_data,
            chr_mem_data: chr_mem_data,
            chr_mem_type: chr_mem_type
        })
    }

    pub fn chr_mem_type(&self) -> ChrMem {
        self.chr_mem_type
    }

    pub fn size(&self) -> usize {
        16 + self.header.prg_rom_size() + self.header.chr_mem_size()
    }
}
