use core::ines::INES;
use core::ines::Mirroring;

pub const VISIBLE_SCANLINES: usize = 240;
pub const SCANLINE_LENGTH: usize = 256;
pub const IMAGE_SIZE: usize = VISIBLE_SCANLINES * SCANLINE_LENGTH;

/*
pub const NES_RGB: [u32; 64] = [
    0x7C7C7C, 0x0000FC, 0x0000BC, 0x4428BC, 0x940084, 0xA80020, 0xA81000, 0x881400,
    0x503000, 0x007800, 0x006800, 0x005800, 0x004058, 0x000000, 0x000000, 0x000000,
    0xBCBCBC, 0x0078F8, 0x0058F8, 0x6844FC, 0xD800CC, 0xE40058, 0xF83800, 0xE45C10,
    0xAC7C00, 0x00B800, 0x00A800, 0x00A844, 0x008888, 0x000000, 0x000000, 0x000000,
    0xF8F8F8, 0x3CBCFC, 0x6888FC, 0x9878F8, 0xF878F8, 0xF85898, 0xF87858, 0xFCA044,
    0xF8B800, 0xB8F818, 0x58D854, 0x58F898, 0x00E8D8, 0x787878, 0x000000, 0x000000,
    0xFCFCFC, 0xA4E4FC, 0xB8B8F8, 0xD8B8F8, 0xF8B8F8, 0xF8A4C0, 0xF0D0B0, 0xFCE0A8,
    0xF8D878, 0xD8F878, 0xB8F8B8, 0xB8F8D8, 0x00FCFC, 0xF8D8F8, 0x000000, 0x000000
];
*/
pub const NES_RGB: [(u8, u8, u8); 64] = [
    (0x7C, 0x7C, 0x7C), (0x00, 0x00, 0xFC), (0x00, 0x00, 0xBC), (0x44, 0x28, 0xBC), (0x94, 0x00, 0x84), (0xA8, 0x00, 0x20), (0xA8, 0x10, 0x00), (0x88, 0x14, 0x00),
    (0x50, 0x30, 0x00), (0x00, 0x78, 0x00), (0x00, 0x68, 0x00), (0x00, 0x58, 0x00), (0x00, 0x40, 0x58), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xBC, 0xBC, 0xBC), (0x00, 0x78, 0xF8), (0x00, 0x58, 0xF8), (0x68, 0x44, 0xFC), (0xD8, 0x00, 0xCC), (0xE4, 0x00, 0x58), (0xF8, 0x38, 0x00), (0xE4, 0x5C, 0x10),
    (0xAC, 0x7C, 0x00), (0x00, 0xB8, 0x00), (0x00, 0xA8, 0x00), (0x00, 0xA8, 0x44), (0x00, 0x88, 0x88), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xF8, 0xF8, 0xF8), (0x3C, 0xBC, 0xFC), (0x68, 0x88, 0xFC), (0x98, 0x78, 0xF8), (0xF8, 0x78, 0xF8), (0xF8, 0x58, 0x98), (0xF8, 0x78, 0x58), (0xFC, 0xA0, 0x44),
    (0xF8, 0xB8, 0x00), (0xB8, 0xF8, 0x18), (0x58, 0xD8, 0x54), (0x58, 0xF8, 0x98), (0x00, 0xE8, 0xD8), (0x78, 0x78, 0x78), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00),
    (0xFC, 0xFC, 0xFC), (0xA4, 0xE4, 0xFC), (0xB8, 0xB8, 0xF8), (0xD8, 0xB8, 0xF8), (0xF8, 0xB8, 0xF8), (0xF8, 0xA4, 0xC0), (0xF0, 0xD0, 0xB0), (0xFC, 0xE0, 0xA8),
    (0xF8, 0xD8, 0x78), (0xD8, 0xF8, 0x78), (0xB8, 0xF8, 0xB8), (0xB8, 0xF8, 0xD8), (0x00, 0xFC, 0xFC), (0xF8, 0xD8, 0xF8), (0x00, 0x00, 0x00), (0x00, 0x00, 0x00)
];

bitflags! {
    pub struct PPUCTRL: u8 {
        const INIT = 0;
        const NT = 0b0000_0011;
        const INCR =  0b0000_0100;
        const SPRITE_TABLE =  0b0000_1000;
        const BG_TABLE =  0b0001_0000;
        const SPRITE_SIZE =  0b0010_0000;
        const _SLAVE =  0b0100_0000;
        const NMI_ENABLE =  0b1000_0000;
    }
}

bitflags! {
    pub struct PPUMASK: u8 {
        const INIT = 0;
        const GRAY = 0b0000_0001;
        const BG_LEFT = 0b0000_0010;
        const SPRITE_LEFT = 0b0000_0100;
        const BG_ENABLE = 0b0000_1000;
        const SP_ENABLE = 0b0001_0000;
        const RED = 0b0010_0000;
        const GREEN = 0b0100_0000;
        const BLUE = 0b1000_0000;
    }
}

impl PPUMASK {
    pub fn is_flag_set(&self, flag: PPUMASK) -> bool {
        *self & flag == flag
    }

    pub fn is_gray(&self) -> bool {
        self.is_flag_set(PPUMASK::GRAY)
    }

    pub fn is_bg_enabled(&self) -> bool {
        self.is_flag_set(PPUMASK::BG_ENABLE)
    }

    pub fn is_sp_enabled(&self) -> bool {
        self.is_flag_set(PPUMASK::SP_ENABLE)
    }

    pub fn is_bg_left(&self) -> bool {
        self.is_flag_set(PPUMASK::BG_LEFT)
    }

    pub fn is_sp_left(&self) -> bool {
        self.is_flag_set(PPUMASK::SPRITE_LEFT)
    }
}

bitflags! {
    pub struct PPUSTATUS: u8 {
        const _BUS = 0b0001_1111;
        const SPRITE_OVERFLOW = 0b0010_0000;
        const SPRITE_HIT = 0b0100_0000;
        const VBLANK = 0b1000_0000;
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Scanline {
    VISIBLE,
    POST,
    NMI,
    PRE
}

#[derive(Debug, Copy, Clone)]
pub struct Sprite {
    pub id: u8,
    pub x: u8,
    pub y: u8,
    pub tile: u8,
    pub attr: u8,
    pub data_low: u8,
    pub data_high: u8
}

impl Sprite {
    pub fn new() -> Sprite {
        Sprite {
            id: 64,
            x: 0xFF,
            y: 0xFF,
            tile: 0xFF,
            attr: 0xFF,
            data_low: 0,
            data_high: 0
        }
    }

    pub fn set(&mut self, m: usize, y: u8, tile: u8, attr: u8, x: u8) {
        self.id = m as u8;
        self.y = y;
        self.tile = tile;
        self.attr = attr;
        self.x = x;
    }
}

pub struct Addr {
    val: u16
}

impl Addr {
    pub fn new(v: u16) -> Addr {
        Addr {
            val: v
        }
    }

    pub fn coarse_x(&self) -> u8 {
        (self.val & 0b0000_0000_0001_1111) as u8
    }

    pub fn set_coarse_x(&mut self, val: u8) {
        self.val &= !0b1_1111;
        self.val |= (val & 0b0001_1111) as u16;
    }

    pub fn incr_coarse_x(&mut self) {
        let res = ((self.val & 0b1_1111) + 1) & 0b1_1111;
        self.val &= !0b1_1111;
        self.val |= res;
    }
    
    pub fn coarse_y(&self) -> u8 {
        ((self.val & 0b0000_0011_1110_0000) >> 5) as u8
    }

    pub fn set_coarse_y(&mut self, val: u8) {
        self.val &= !0b0000_0011_1110_000;
        self.val |= ((val & 0b0001_1111) as u16) << 5;
    }

    pub fn incr_coarse_y(&mut self) {
        let res = ((self.val & 0b0000_0011_1110_0000) + 1) & 0b0000_0011_1110_0000;
        self.val &= !0b0000_0011_1110_0000;
        self.val |= res;
    }

    pub fn nametable_raw(&self) -> u16 {
        self.val & 0b0000_1100_0000_0000
    }
    
    pub fn nametable(&self) -> u8 {
        (self.nametable_raw() >> 10) as u8
    }

    pub fn set_nametable(&mut self, nt: u8) {
        self.val &= !0b0000_1100_0000_0000;
        self.val |= ((nt & 0b0000_0011) as u16) << 10;
    }

    pub fn xor_nametable(&mut self, val: u16) {
        let nt = (self.val & 0b0000_1100_0000_0000) ^
            ((val << 10) & 0b0000_1100_0000_0000);
        self.val &= !0b0000_1100_0000_0000;
        self.val |= nt;
    }
    
    pub fn fine_y(&self) -> u8 {
        (self.val & 0b0111_0000_0000_0000 >> 12) as u8
    }

    pub fn set_fine_y(&mut self, val: u8) {
        self.val |= (val as u16) << 12
    }

    pub fn incr_fine_y(&mut self) {
        self.val += 0b0001_0000_0000_0000;
        self.val &= 0b0111_1111_1111_1111;
    }

    pub fn low(&self) -> u8 {
        (self.val & 0x00FF) as u8
    }

    pub fn set_low(&mut self, val: u8) {
        self.val |= val as u16
    }

    pub fn high(&self) -> u8 {
        ((self.val & 0x7F00) >> 8) as u8
    }

    pub fn set_high(&mut self, val: u8) {
        self.val |= (val as u16) << 8;
    }

    pub fn addr(&self) -> u16 {
        self.val & 0b0011_1111_1111_1111
    }

    pub fn get(&self) -> u16 {
        self.val & 0x7FFF
    }

    pub fn set(&mut self, new: u16) {
        self.val = new;
    }
}

pub struct PPU {
    mirroring: Mirroring,
    
    ppu_ctrl: PPUCTRL,
    ppu_mask: PPUMASK,
    ppu_status: PPUSTATUS,

    ppu_scroll: u8,
    ppu_addr: u8,

    oam_addr: u8,
    oam_data: u8,
    oam_dma: u8,

    vram_addr: Addr,
    temp_addr: Addr,
    fine_x: u8,
    
    ci_ram: [u8; 2048],
    cg_ram: [u8; 32],
    oam_ram: [u8; 256],
    
    sprite_ram: [Sprite; 8],
    secondary_oam: [Sprite; 8],

    latch_nametable: u8,
    latch_attr: u8,
    latch_bg_low: u8,
    latch_bg_high: u8,

    shift_attr_low: u8,
    shift_attr_high: u8,
    shift_bg_low: u16,
    shift_bg_high: u16,
    bit_attr_low: bool,
    bit_attr_high: bool,
    
    image: [(u8, u8, u8); IMAGE_SIZE],
    
    scanline: isize,
    dot: isize,
    odd_frame: bool,

    gen_result: u8,
    gen_buffer: u8,
    gen_latch: bool,

    signal_cpu_nmi: bool,
    signal_new_frame: bool,

    work_addr: u16
}

impl PPU {
    pub fn init(mirror: Mirroring) -> PPU {
        PPU {
            mirroring: mirror,
                
            ppu_ctrl: PPUCTRL::INIT,
            ppu_mask: PPUMASK::INIT,
            ppu_status: PPUSTATUS::VBLANK | PPUSTATUS::SPRITE_OVERFLOW,

            ppu_scroll: 0x00,
            ppu_addr: 0x00,

            oam_addr: 0x00,
            oam_data: 0x00,
            oam_dma: 0,
            
            vram_addr: Addr { val: 0 },
            temp_addr: Addr { val: 0 },
            fine_x: 0,
            
            ci_ram: [0xFF; 2048],
            cg_ram: [0xFF; 32],
            oam_ram: [0x00; 256],
            
            sprite_ram: [Sprite::new(); 8],
            secondary_oam: [Sprite::new(); 8],

            latch_nametable: 0,
            latch_attr: 0,
            latch_bg_low: 0,
            latch_bg_high: 0,

            shift_attr_low: 0,
            shift_attr_high: 0,
            shift_bg_low: 0,
            shift_bg_high: 0,
            bit_attr_low: false,
            bit_attr_high: false,

            image: [(0, 0, 0); IMAGE_SIZE],
                      
            scanline: 0,
            dot: 0,
            odd_frame: false,

            gen_buffer: 0,
            gen_result: 0,
            gen_latch: false,

            signal_cpu_nmi: false,
            signal_new_frame: false,

            work_addr: 0
        }
    }

    pub fn signal_new_frame(&self) -> bool {
        self.signal_new_frame
    }

    pub fn signal_nmi(&self) -> bool {
        self.signal_cpu_nmi
    }

    pub fn clear_nmi_signal(&mut self) {
        self.signal_cpu_nmi = false;
    }

    pub fn clear_frame_signal(&mut self) {
        self.signal_new_frame = false;
    }

    #[inline(always)]
    fn palette_ram_addr(&self, addr: &mut u16) {
        if (*addr & 0x13) == 0x10 {
            *addr &= !0x10;
        } 
    }

    fn internal_read(&self, addr: &mut u16, cart: &mut INES) -> u8 {
        match *addr {
            0x0000 ... 0x1FFF => cart.ppu_read(*addr),
            0x2000 ... 0x3EFF => self.ci_ram[cart.nametable_mirror(*addr)  as usize],
            0x3F00 ... 0x3FFF => {
                self.palette_ram_addr(addr);
                let rhs = if !self.ppu_mask.is_gray() {
                    0x30
                } else {
                    0xFF
                };
                self.cg_ram[(*addr & 0x1F) as usize] & rhs
            },
            _ => 0
        }
    }

    fn internal_write(&mut self, addr: &mut u16, val: u8, cart: &mut INES) {
        match *addr {
            0x0000 ... 0x1FFF => cart.ppu_write(*addr, val),
            0x2000 ... 0x3EFF => self.ci_ram[cart.nametable_mirror(*addr) as usize] = val,
            0x3F00 ... 0x3FFF => {
                self.palette_ram_addr(addr);
                self.cg_ram[(*addr & 0x1F) as usize] = val;
            },
            _ => {},
        }
    }

    fn reset(&mut self) {
        self.odd_frame = false;
        self.scanline = 0;
        self.dot = 0;
        self.ppu_ctrl.bits = 0;
        self.ppu_mask.bits = 0;
        self.ppu_status.bits = 0;
        self.image = [(0, 0, 0); IMAGE_SIZE];
        self.ci_ram = [0xFF; 2048];
        self.oam_ram = [0x00; 256];
    }

    pub fn read(&mut self, address: u16, cart: &mut INES) -> u8 {
        match address % 8 {
            2 => {
                let val = self.ppu_status.bits | (self.gen_result & 0x1F);
                self.gen_result = val;
                self.ppu_status.bits &= !(0x01);
                self.gen_latch = false;
            },
            4 => {
                let val = self.oam_ram[self.oam_addr as usize];
                self.gen_result = val;
            },
            7 => {
                if self.vram_addr.addr() <= 0x3EFF {
                    let buf = self.gen_buffer;
                    self.gen_result = buf;
                    let mut addr = address;
                    let new_buf = self.internal_read(&mut addr, cart);
                    self.gen_buffer = new_buf;
                } else {
                    let mut addr = address;
                    let res = self.internal_read(&mut addr, cart);
                    self.vram_addr.val = addr;
                    self.gen_result = res;
                    self.gen_buffer = res;
                }
                self.vram_increment();
            },
            _ => {},
        }

        self.gen_result
    }

    pub fn write(&mut self, address: u16, val: u8, cart: &mut INES) {
        self.gen_result = val;
        match address % 8 {
            0 => {
                self.ppu_ctrl.bits = val;
                let nt = (self.ppu_ctrl & PPUCTRL::NT).bits;
                self.temp_addr.set_nametable(nt);
            },
            1 => self.ppu_mask.bits = val,
            3 => self.oam_addr = val,
            4 => {
                self.oam_addr += 1;
                self.oam_ram[self.oam_addr as usize];
                //self.oam_data = val;
            },
            5 => {
                let not_latch = !self.gen_latch;
                if not_latch {
                    self.fine_x = val & 7;
                    self.temp_addr.set_coarse_x(val >> 3);
                } else {
                    self.temp_addr.set_fine_y(val & 7);
                    self.temp_addr.set_coarse_y(val >> 3);
                }
                self.gen_latch = not_latch;
            },
            6 => {
                let not_latch = !self.gen_latch;
                if not_latch {
                    self.temp_addr.set_high(val & 0x3F);
                } else {
                    self.temp_addr.set_low(val);
                    let addr = self.temp_addr.get();
                    self.vram_addr = Addr { val: addr };
                }
                self.gen_latch = not_latch;
            },
            7 => {
                let mut addr = self.vram_addr.val;
                self.internal_write(&mut addr, val, cart);
                self.vram_addr.val = addr;
                self.vram_increment();
            }
            _ => {},
        }
    }

    pub fn oam_dma_read(&self) -> u8 {
        self.oam_ram[self.oam_dma as usize]
    }

    pub fn oam_dma_write(&mut self, val: u8) {
        self.oam_ram[self.oam_dma as usize] = val;
        self.oam_dma += 1;
    }

    fn vram_increment(&mut self) {
        let increment = if (self.ppu_ctrl & PPUCTRL::INCR) == PPUCTRL::INCR {
            32
        } else {
            1
        };
        self.vram_addr.val += increment;
        if self.vram_addr.val >= 2047 {
            self.vram_addr.val -= 2047
        }
    }

    fn nametable_address(&self) -> u16 {
        0x2000 | (self.vram_addr.get() & 0xFFF)
    }

    fn attr_address(&self) -> u16 {
        0x23C0 | self.vram_addr.nametable_raw()
            | ((self.vram_addr.coarse_y() / 4) as u16) << 3
            | (self.vram_addr.coarse_x() / 4) as u16
    }

    fn bg_address(&self) -> u16 {
        (((self.ppu_ctrl & PPUCTRL::BG_TABLE).bits as u16) >> 4) * 0x1000
            + ((self.latch_nametable << 3) as u16)
            + self.vram_addr.fine_y() as u16
    }

    fn horizontal_scroll(&mut self) {
        let rendering = self.is_rendering();
        if rendering {
            if self.vram_addr.coarse_x() == 31 {
                let new = self.vram_addr.get() ^ 0x41F;
                self.vram_addr.set(new);
            } else {
                self.vram_addr.incr_coarse_x();
            }
        }
    }
    
    fn vertical_scroll(&mut self) {
        let rendering = self.is_rendering();
        if rendering {
            if self.vram_addr.fine_y() < 7 {

                self.vram_addr.incr_fine_y();
                
            } else {

                self.vram_addr.set_fine_y(0);
                
                if self.vram_addr.coarse_y() == 31 {
                    
                    self.vram_addr.set_coarse_y(0);
                    
                } else if self.vram_addr.coarse_y() == 29 {
                    
                    self.vram_addr.set_coarse_y(0);
                    self.vram_addr.xor_nametable(0b10);
                } else {

                    self.vram_addr.incr_coarse_y();
                }
            }
        }
    }

    fn horizontal_update(&mut self) {
        if self.is_rendering() {
            let addr = (self.vram_addr.get() & !0x041F) | (self.temp_addr.get() & 0x041F);
            self.vram_addr.set(addr);
        }
    }

    fn vertical_update(&mut self) {
        if self.is_rendering() {
            let addr = (self.vram_addr.get() & !0x7BE0) | (self.temp_addr.get() & 0x7BE0);
            self.vram_addr.set(addr);
        }
    }

    fn reload_shift(&mut self) {
        let rhs = (self.shift_bg_low & 0xFF00) | (self.latch_bg_low as u16);
        self.shift_bg_low = rhs;
        let rhs = (self.shift_bg_high & 0xFF00) | (self.latch_bg_high as u16);
        self.shift_bg_high = rhs;

        let low = (self.latch_attr & 1) != 0;
        self.bit_attr_low = low;
        let high = (self.latch_attr & 2) != 0;
        self.bit_attr_high = high;
    }

    fn clear_sec_oam(&mut self) {
        for i in 1 .. 8 {
            self.secondary_oam[i] = Sprite::new()
        }
    }

    fn eval_sprites(&mut self) {
        let mut n = 0;
        'outer: for m in 0 .. 64 {
            let line = if self.scanline == 261 {
                -1 - ((self.oam_ram[m * 4] as usize) as isize)
            } else {
                self.scanline - ((self.oam_ram[m * 4] as usize) as isize)
            };

            if line >= 0 && line < (self.sprite_height() as isize) {
                let y = self.oam_ram[m * 4];
                let tile = self.oam_ram[m * 4 + 1];
                let attr = self.oam_ram[m * 4 + 2];
                let x = self.oam_ram[m * 4 + 3];
                if n < 8 {
                    self.secondary_oam[n].set(m, y, tile, attr, x);
                }

                n += 1;
                if n > 8 {
                    self.ppu_status |= PPUSTATUS::SPRITE_OVERFLOW;
                    break 'outer
                }
            }
        }
    }

    fn load_sprites(&mut self, cart: &mut INES) {
        let mut addr;
        for i in 0 .. 8 {
            let sp_i = self.secondary_oam[i].clone();
            self.sprite_ram[i] = sp_i;
            let sp_h = self.sprite_height();

            if sp_h == 16 {
                addr = (((self.sprite_ram[i].tile & 1) as u16) * 0x1000)
                    + (((self.sprite_ram[i].tile & !1) as u16) * 16);
            } else {
                addr = ((((self.ppu_ctrl & PPUCTRL::SPRITE_TABLE).bits >> 3)  as u16) * 0x1000) +
                    (self.sprite_ram[i].tile as u16) * 16;
            }

            let mut sprite_y = ((self.scanline as usize)
                                .wrapping_sub(self.sprite_ram[i].y as usize)
                                 % (sp_h as usize)) as u8;
            if sp_i.attr & 0x80 != 0 {
                sprite_y ^= sp_h - 1;
            }

            addr += sprite_y as u16;
            let low = self.internal_read(&mut addr, cart);
            self.sprite_ram[i].data_low = low;
            addr += 8;
            let high = self.internal_read(&mut addr, cart);
            self.sprite_ram[i].data_high = high;
        }
    }

    fn pixel(&mut self, cart: &mut INES) {
        let mut palette = 0;
        let mut obj_palette = 0;
        let mut obj_priority = false;
        let x = self.dot - 2;

        if self.scanline < 240 && x >= 0 && x < 256 {
            if self.ppu_mask.is_bg_enabled() && !(!self.ppu_mask.is_bg_left() && x < 8) {
                palette = (bit_n(self.shift_bg_high, 15 - self.fine_x) << 1)
                    | bit_n(self.shift_bg_low, 15 - self.fine_x);

                if palette != 0 {
                    palette |= ((bit_n(self.shift_bg_high, 7 - self.fine_x) << 1)
                                | bit_n(self.shift_bg_low, 7 - self.fine_x)) << 2;
                }
            }

            if true { //self.ppu_mask.is_sp_enabled() && !(!self.ppu_mask.is_sp_left() && x < 8) {
                let mut i: isize = 8;
                while i >= 1 {
                    i -= 1;
                    let sprite = self.sprite_ram[i as usize];
                    if sprite.id == 64 {
                        continue
                    }

                    let mut sprite_x = x - (sprite.x as isize);
                    if sprite_x >= 8 {                    
                        continue
                    }

                    if sprite.attr & 0x40 != 0 {
                        sprite_x ^= 7;
                    }

                    let mut sprite_palette =
                        (bit_n(sprite.data_high as u16, 7 - (sprite_x as u8)) << 1) 
                        | bit_n(sprite.data_low as u16, 7 - (sprite_x as u8));

                    if sprite_palette == 0 {
                        continue
                    }

                    if sprite.id == 0 && palette != 0 && x != 255 {
                        self.ppu_status |= PPUSTATUS::SPRITE_HIT;
                    }

                    sprite_palette |= (sprite.attr & 3) << 2;
                    obj_palette = sprite_palette + 16;
                    obj_priority = sprite.attr & 0x20 != 0;
                }

                if obj_palette != 0 && (palette == 0 || obj_priority == false) {
                    palette = obj_palette;
                }

                let rgb_idx = if self.is_rendering() {
                    self.internal_read(&mut 0x3F00, cart) + palette
                } else {
                    self.internal_read(&mut 0x3F00, cart)
                };
                let img_idx = (self.scanline as usize) * 256 + (x as usize);
                self.image[img_idx] = NES_RGB[(rgb_idx % 64) as usize];
                //self.image[img_idx] = (self.scanline as u8, x as u8, 2 * (x as u8))
            }
        }
        self.shift_bg_low <<= 1;
        self.shift_bg_high <<= 1;
        self.shift_attr_low = (self.shift_attr_low << 1) | if self.bit_attr_low {
            1
        } else {
            0
        };
        self.shift_attr_high = (self.shift_attr_high << 1) | if self.bit_attr_high {
            1
        } else {
            0
        };
    }

    fn is_rendering(&self) -> bool {
        (self.ppu_mask & PPUMASK::BG_ENABLE == PPUMASK::BG_ENABLE) ||
            (self.ppu_mask & PPUMASK::SP_ENABLE == PPUMASK::SP_ENABLE)
    }

    fn sprite_height(&self) -> u8 {
        if (self.ppu_ctrl & PPUCTRL::SPRITE_SIZE) == PPUCTRL::SPRITE_SIZE {
            16
        } else {
            8
        }
    }

    fn scanline_cycle(&mut self, s: Scanline, cart: &mut INES) {
        match (s, self.dot) {
            (Scanline::NMI, 1) => {
                self.ppu_status |= PPUSTATUS::VBLANK;            
                if self.ppu_ctrl & PPUCTRL::NMI_ENABLE == PPUCTRL::NMI_ENABLE {
                    self.signal_cpu_nmi = true;
                }
            },
            
            (Scanline::POST,    0) => self.signal_new_frame = true,
            
            (Scanline::VISIBLE, x) |
            (Scanline::PRE,     x) => {
                match x {
                    1 => {
                        self.clear_sec_oam();
                        if s == Scanline::PRE {
                            self.ppu_status &= !(PPUSTATUS::SPRITE_OVERFLOW | PPUSTATUS::SPRITE_HIT);
                        }
                    },
                    257 => self.eval_sprites(),
                    321 => self.load_sprites(cart),
                    _ => {}
                }

                match x {
                    1 => {
                        let addr = self.nametable_address();
                        self.work_addr = addr;
                        if s == Scanline::PRE {
                            self.ppu_status &= !PPUSTATUS::VBLANK;
                        }
                    },
                    2 ... 255 | 322 ... 337 => {
                        self.pixel(cart);
                        match x % 8 {
                            1 => {
                                let addr = self.nametable_address();
                                self.work_addr = addr;
                                self.reload_shift();
                            },
                            2 => {
                                let mut addr = self.work_addr;
                                let nt = self.internal_read(&mut addr, cart);
                                self.work_addr = addr;
                                self.latch_nametable = nt;
                            },
                            3 => {
                                let addr = self.attr_address();
                                self.work_addr = addr;
                            },
                            4 => {
                                let mut addr = self.work_addr;
                                let mut at = self.internal_read(&mut addr, cart);
                                self.work_addr = addr;
                                if self.vram_addr.coarse_y() & 2 != 0 {
                                    at >>= 4;
                                }
                                if self.vram_addr.coarse_x() & 2 != 0 {
                                    at >>= 2;
                                }
                                self.latch_attr = at;
                            },
                            5 => {
                                let addr = self.bg_address();
                                self.work_addr = addr
                            },
                            6 => {
                                let mut addr = self.work_addr;
                                let bg_low = self.internal_read(&mut addr, cart);
                                self.work_addr = addr;
                                self.latch_bg_low = bg_low;
                            },
                            7 => {
                                self.work_addr += 8;
                            },
                            0 | _ => {
                                let mut addr = self.work_addr;
                                let bg_hi = self.internal_read(&mut addr, cart);
                                self.work_addr = addr;
                                self.latch_bg_high = bg_hi;
                                self.horizontal_scroll();
                            }
                        }
                    },
                    256 => {
                        self.pixel(cart);
                        let mut addr = self.work_addr;
                        let bg_hi = self.internal_read(&mut addr, cart);
                        self.work_addr = addr;
                        self.latch_bg_high = bg_hi;
                        self.vertical_scroll();
                    },
                    257 => {
                        self.pixel(cart);
                        self.reload_shift();
                        self.horizontal_update();
                    },
                    280 ... 304 if s == Scanline::PRE => self.vertical_update(),
                    321 | 339 => {
                        let addr = self.nametable_address();
                        self.work_addr = addr;
                    },
                    338 => {
                        let mut addr = self.work_addr;
                        let nt = self.internal_read(&mut addr, cart);
                        self.work_addr = addr;
                        self.latch_nametable = nt;
                    },
                    340 => {
                        let mut addr = self.work_addr;
                        let nt = self.internal_read(&mut addr, cart);
                        self.work_addr = addr;
                        self.latch_nametable = nt;
                    },
                    _ => {}
                }
            },
            _ => {},
        }
    }

    pub fn step(&mut self, cart: &mut INES) {
        match self.scanline {
            000 ... 239 => self.scanline_cycle(Scanline::VISIBLE, cart),
            240         => self.scanline_cycle(Scanline::POST, cart),
            241 ... 260 => self.scanline_cycle(Scanline::NMI, cart),
            261         => self.scanline_cycle(Scanline::PRE, cart),
            _ => panic!("undefined territory")
        }

        self.dot += 1;
        if self.dot > 340 {
            self.dot %= 341;
            self.scanline += 1;
            if self.scanline > 261 {
                self.scanline = 0;
                let odd_frame = !self.odd_frame;
                self.odd_frame = odd_frame;
            }
        }
    }

    pub fn image(&self) -> &[(u8, u8, u8); IMAGE_SIZE] {
        &self.image
    }
}

fn bit_n(x: u16, n: u8) -> u8 {
    ((x >> n) & 1) as u8
}
