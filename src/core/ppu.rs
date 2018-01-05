pub const VISIBLE_SCANLINES: usize = 240;
pub const PRE_VBLANK_LINE: usize = 1;
pub const VBLANK_LINES: usize = 20;

bitflags! {
    pub struct PPUCTRL: u8 {
        const INIT = 0;
        const NAMETABLE_MSB = 0b0000_0011;
        const VRAM_INCREMENT =  0b0000_0100;
        const SPRITE_TABLE =  0b0000_1000;
        const BG_TABLE =  0b0001_0000;
        const SPRITE_SIZE =  0b0010_0000;
        const PPU_MS_SELECT =  0b0100_0000;
        const VBLANK_NMI =  0b1000_0000;
    }
}

impl PPUCTRL {
    pub fn base_nametable_addres(&self) -> u16 {
        0x2000 + 0x0400 * ((*self & PPUCTRL::NAMETABLE_MSB).bits as u16)
    }
    pub fn vram_inc(&self) -> u8 {
        if (*self &  PPUCTRL::VRAM_INCREMENT) == PPUCTRL::VRAM_INCREMENT {
            1
        } else {
            32
        }
    }
}

bitflags! {
    pub struct PPUMASK: u8 {
        const INIT = 0;
        const GRAY = 0b0000_0001;
        const BGL8 = 0b0000_0010;
        const SPL8 = 0b0000_0100;
        const BGALL = 0b0000_1000;
        const SPALL = 0b0001_0000;
        const RED = 0b0010_0000;
        const GRN = 0b0100_0000;
        const BLU = 0b1000_0000;
    }
}

bitflags! {
    pub struct PPUSTATUS: u8 {
        const SPRITE_OVERFLOW = 0b0010_0000;
        const SPRITE_HIT = 0b0100_0000;
        const VBLANK = 0b1000_0000;
    }
}

pub struct PPU {
    ppu_ctrl: PPUCTRL,
    ppu_mask: PPUMASK,
    ppu_status: PPUSTATUS,
    oam_addr: u8,
    oam_data: u8,
    ppu_gen_latch: u8,
    ppu_scroll: u8,
    ppu_addr: u8,
    ppu_data: u8,
    oam_dma: u8,
    vram_addr: u16,
    video_ram: [u8; 2048],
    oam_ram: [u8; 256],
}

impl PPU {
    pub fn init() -> PPU {
        PPU { 
            ppu_ctrl: PPUCTRL::INIT,
            ppu_mask: PPUMASK::INIT,
            ppu_status: PPUSTATUS::VBLANK | PPUSTATUS::SPRITE_OVERFLOW,
            oam_addr: 0x00,
            oam_data: 0x00,
            ppu_gen_latch: 0x00,
            ppu_scroll: 0x00,
            ppu_addr: 0x00,
            ppu_data: 0x00,
            oam_dma: 0,
            vram_addr: 0,
            video_ram: [0xFF; 2048],
            oam_ram: [0x00; 256]
        }
    }

    fn _reset() {
        
    }

    pub fn read(&mut self, address: u16) -> u8 {
        match address % 8 {
            0 => self.ppu_gen_latch,
            1 => self.ppu_gen_latch,
            2 => {
                let val = self.ppu_status.bits | (self.ppu_gen_latch & 0b00011111);
                self.ppu_gen_latch = val;
                self.ppu_gen_latch
            },
            3 => self.ppu_gen_latch,
            4 => {
                let val = self.oam_ram[self.oam_addr as usize];
                self.ppu_gen_latch = val;
                self.ppu_gen_latch
            },
            5 => self.ppu_gen_latch,
            6 => self.ppu_gen_latch,
            7 | _ => {
                self.ppu_gen_latch = self.video_ram[self.vram_addr as usize];
                self.vram_increment();
                self.ppu_gen_latch
            },
        }
    }

    pub fn write(&mut self, address: u16, val: u8) {
        self.ppu_gen_latch = val;
        match address % 8 {
            0 => self.ppu_ctrl.bits = val,
            1 => self.ppu_mask.bits = val,
            2 => {},
            3 => self.oam_addr = val,
            4 => {
                self.oam_addr = self.oam_addr.wrapping_add(1);
                self.oam_data = val;
            },
            5 => self.ppu_scroll = val,
            6 => self.ppu_addr = val,
            7 | _ => {
                self.video_ram[self.vram_addr as usize];
                self.vram_increment();
            }
        }
    }

    pub fn oam_dma_write(&mut self, val: u8) {
        self.oam_ram[self.oam_dma as usize] = val;
        self.oam_dma += 1;
    }

    fn vram_increment(&mut self) {
        let inc = (self.ppu_ctrl & PPUCTRL::VRAM_INCREMENT).bits as u16;
        self.vram_addr += inc
    }
}
