use core::ines::INES;

pub const SCANLINES: usize = 262;
pub const VISIBLE_SCANLINES: usize = 240;
pub const SCANLINE_LENGTH: usize = 256;
pub const SCANLINE_CYCLES: usize = 341;
pub const IMAGE_SIZE: usize = SCANLINES * SCANLINE_LENGTH;
//
pub const NTSC_PALETTE: [(u8, u8, u8); 56] = [
    (84,  84,  84),   (0,  30, 116),    (8,  16, 144),  (48,   0, 136),  (68,   0, 100),  (92,   0,  48),  (84,   4,   0),  (60,  24,   0),
    (32,  42,   0),   (8,  58,   0),    (0,  64,   0),   (0,  60,   0),   (0,  50,  60),   (0,   0,   0), (152, 150, 152),   (8,  76, 196),
    (48,  50, 236),  (92,  30, 228),  (136,  20, 176), (160,  20, 100), (152,  34,  32), (120,  60,   0),  (84,  90,   0),  (40, 114,   0),
    (8, 124,   0),   (0, 118,  40),     (0, 102, 120),   (0,   0,   0),  (236, 238, 236),  (76, 154, 236), (120, 124, 236), (176,  98, 236),
    (228,  84, 236), (236,  88, 180), (236, 106, 100), (212, 136,  32), (160, 170,   0), (116, 196,   0),  (76, 208,  32),  (56, 204, 108),
    (56, 180, 204),  (60,  60,  60),  (236, 238, 236), (168, 204, 236), (188, 188, 236), (212, 178, 236), (236, 174, 236), (236, 174, 212),
    (236, 180, 176), (228, 196, 144), (204, 210, 120), (180, 222, 120), (168, 226, 144), (152, 226, 180), (160, 214, 228), (160, 162, 160)
];


/* PPU MEMORY MAP
 * 0000 ... 0FFF | 0x1000 | Pattern Table 0
 * 1000 ... 1FFF | 0x1000 | Pattern Table 1
 * 2000 ... 23FF | 0x0400 | Nametable 0
 * 2400 ... 27FF | 0x0400 | Nametable 1
 * 2800 ... 2BFF | 0x0400 | Nametable 2
 * 2C00 ... 2FFF | 0x0400 | Nametable 3
 * 3000 ... 3EFF | 0x0F00 | Mirrors of 0x2000 ... 0x2FFFF
 * 3F00 ... 3F1F | 0x0020 | Pattern RAM indices
 * 3F20 ... 3FFF | 0x00E0 | Mirrors of 0x3F00 ... 0x3F1F
 */

/* OAM :
 * 00 ... 0C | 0x40 | Sprite Y coordinate
 * 01 ... 0D | 0x40 | Sprite tile #
 * 02 ... 0E | 0x40 | Sprite attribute
 * 03 ... 0F | 0x40 | Sprite X coordinate
 */

/* Palettes:
 * 3F00          | Univeral Background Color
 * 3F01 ... 3F03 | Background palette 0
 * 3F05 ... 3F07 | Background palette 1
 * 3F09 ... 3F0B | Background palette 2
 * 3F0D ... 3F0F | Background palette 3
 * 3F11 ... 3F13 | Sprite palette 0
 * 3F15 ... 3F17 | Sprite palette 1
 * 3F19 ... 3F1B | Sprite palette 2
 * 3F1D ... 3F1F | Sprite palette 3
 *
 * $3F1{0,4,8,C} mirror 3F0{0,4,8,C}
 *
 * Palette index decomposition:
 * 43210
 * |||||
 * |||++- Pixel value from tile data
 * |++--- Palette number from attribute table or OAM
 * +----- Background/Sprite select
 *
 * Palette value decomposition:
 * 76543210
 * ||||||||
 * ||||++++- Hue (phase, determines NTSC/PAL chroma)
 * ||++----- Value (voltage, determines NTSC/PAL luma)
 * ++------- Unimplemented, reads back as 0
 */

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

struct OAM {
    pub y: u8,
    idx: u8,
    attributes: u8,
    pub x: u8
}

impl OAM {
    pub fn new(oam_ram: &[u8, 4]) -> OAM {
        OAM {
            y: oam_ram[0],
            idx: oam_ram[1],
            attributes: oam_ram[2],
            x: oam_ram[3]
        }
    }

    pub fn tile_idx(&self) -> u8 {
        self.idx & 0b1111_1110
    }

    pub fn tile_bank(&self) -> u16 {
        if self.idx & 1 == 1 {
            0x0000
        } else {
            0x1000
        }
    }

    pub fn attr_palette(&self) -> u8 {
        (self.attributes & 0b0000_0011) + 4
    }

    pub fn priority(&self) -> bool {
        self.attributes & 0b0010_0000 != 0b0010_0000
    }

    pub fn flip_horizontal(&self) -> bool {
        self.attributes & 0b0100_0000 == 0b0100_0000
    }

    pub fn flip_vertical(&self) -> bool {
        self.attributes & 0b1000_0000 == 0b1000_0000
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
    cycles: u32,
    even_frame: bool,
    image: [u8; IMAGE_SIZE]
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
            oam_ram: [0x00; 256],
            cycles: 0,
            even_frame: false,
            image: [0; IMAGE_SIZE]
        }
    }

    fn _reset(&mut self) {
        self.ppu_ctrl = PPUCTRL::INIT;
        self.ppu_mask = PPUMASK::INIT;
        self.ppu_gen_latch = 0x00;
        self.ppu_data = 0x00;
        self.even_frame = false;
    }

    pub fn is_vblank(&self) -> bool {
        self.ppu_status & PPUSTATUS::VBLANK == PPUSTATUS::VBLANK
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
        let increment = if (self.ppu_ctrl & PPUCTRL::VRAM_INCREMENT) == PPUCTRL::VRAM_INCREMENT {
            1
        } else {
            32
        };
        self.vram_addr += increment;
        if self.vram_addr >= 2047 {
            self.vram_addr -= 2047
        }
    }

    fn base_nametable(&self) -> u16 {
        0x2000 + 0x400 * ((self.ppu_ctrl & PPUCTRL::NAMETABLE_MSB).bits as u16)
    }

    fn pattern_table(&self, flag: PPUCTRL) -> u16 {
        if (self.ppu_ctrl & flag) == flag {
            0x1000
        } else {
            0x0000
        }
    }

    fn sprite_pattern_table(&self) -> u16 {
        self.pattern_table(PPUCTRL::SPRITE_TABLE)
    }

    fn background_pattern_table(&self) -> u16 {
        self.pattern_table(PPUCTRL::BG_TABLE)
    }

    fn sprite_width(&self) -> u8 {
        if (self.ppu_ctrl & PPUCTRL::SPRITE_SIZE) == PPUCTRL::SPRITE_SIZE {
            16
        } else {
            8
        }
    }
    
    fn is_nmi_generated(&self) -> bool {
        (self.ppu_ctrl & PPUCTRL::VBLANK_NMI) == PPUCTRL::VBLANK_NMI
    }

    fn scroll_x(&self) -> u8 {
        (self.ppu_scroll & 0xF0) >> 4
    }

    fn scroll_y(&self) -> u8 {
        (self.ppu_scroll & 0x0F)
    }

    /* | BG pixel |Sprite pixel | Priority | Output    |
     * |        0 |           0 |        X | BG ($3F00)|
     * |        0 |         1-3 |        X | Sprite    |
     * |      1-3 |           0 |        X | BG        |
     * |      1-3 |         1-3 |        0 | Sprite    |
     * |      1-3 |         1-3 |        1 | BG        |
     */
    fn display_oam(&self, bg: u8, sp: u8, priority: bool) -> bool {
        match (bg, sp, priority) {
            (1 ... 3, 1 ... 3, true) => true,
            (1 ... 3, 1 ... 3, false) => false,
            (      0,       0, _) => false,
            (      0, 1 ... 3, _) => true,
            (_,             _, _) => false,
        }
    }

    fn oam_pick_sprites(scanline: u8) -> Vec<OAM> {
        let mut sprite_list = Vec::new();
        let mut i = 0;
        while i < 253 {
            if self.oam_ram[i] == scanline {
                sprite_list.push(OAM::new(self.oam_ram[i .. i + 4]));
            }
            i += 4;
        }
        
        if sprite_list.len() < 8 {
            
        }
    }

    fn render_scanline(&mut self, cart: &mut INES, line: u8) {
        let sprites = self.oam_pick_sprites();
    }

    pub fn render(&mut self, cart: &mut INES) {
        for scanline in 0 .. VISIBLE_SCANLINES {
            self.render_scanline(scanline);
        }
    }
}
