pub mod cpu;
pub mod ines;
pub mod ppu;
pub mod apu;
pub mod constants;
pub mod types;
pub mod memory_bus;

#[cfg(feature = "debug")]
pub use super::debug::Debug;

use self::cpu::CPU;
use self::ines::INES;
use self::ppu::PPU;

pub enum Interrupt {
    IRQ,
    NMI,
    Nil
}

pub struct Memory<'a> {
    cpu_ram: &'a mut [u8; 2048],
    io_regs: &'a mut [u8; 32],
    ppu: &'a mut PPU,
    cart: &'a mut INES
}

impl<'a> Memory<'a> {
    #[inline(always)]
    pub fn new(ram: &'a mut [u8; 2048], io: &'a mut [u8; 32],
               ppu: &'a mut PPU, cart: &'a mut INES) -> Memory<'a> {
        Memory {
            cpu_ram: ram,
            io_regs: io,
            ppu: ppu,
            cart: cart
        }
    }
    
    pub fn read(&mut self, address: u16) -> u8 {
        match address {
            0x0000 ... 0x1FFF => self.cpu_ram[(address % 2048) as usize],
            0x2000 ... 0x3FFF => self.ppu.read(address, &mut self.cart),
            0x4000 ... 0x4013 |
            0x4015 ... 0x401F => self.io_regs[(address - 0x4000) as usize],
            0x4014 => self.ppu.oam_dma_read(),
            0x4020 ... 0xFFFF | _ => self.cart.read(address),
        }
    }

    pub fn write(&mut self, address: u16, val: u8) {
        match address as usize {
            0x0000 ... 0x1FFF => {
                self.cpu_ram[(address % 2048) as usize] = val;
            },
            0x2000 ... 0x3FFF => {
                self.ppu.write(address, val, &mut self.cart);
            },
            0x4014 => {
                //read from val*0x0100 ... val*0x0100 + 255
                let page = (val as u16) << 8;
                for b in page .. page + 256 {
                    let val = self.read(b);
                    self.ppu.oam_dma_write(val);
                }
                //self.counter -= 514;
            }
            0x4000 ... 0x4013 | 0x4015 ... 0x401F => {
                self.io_regs[(address - 0x4000) as usize] = val;
            },
            0x4020 ... 0xFFFF | _ => {
                self.cart.write(address, val)
            },
        }
    }
}

pub struct NESCore {
    cpu: CPU,
    ppu: PPU,
    cpu_ram: [u8; 2048],
    io_regs: [u8; 32],
    cart: INES
}

impl NESCore {
    pub fn power_up(cart: INES) -> NESCore {
        NESCore {
            cpu: CPU::new(),
            ppu: PPU::init(cart.mirroring()),
            cpu_ram: [0u8; 2048],
            io_regs: [0u8; 32],
            cart: cart
        }
    }
}
