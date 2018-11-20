mod instruction;

use std::{u8, u16, fmt};
use self::instruction::*;
use super::Interrupt;
use super::Memory;


pub const FLAG_C: u8 = 0b0000_0001;
pub const FLAG_Z: u8 = 0b0000_0010;
pub const FLAG_I: u8 = 0b0000_0100;
pub const FLAG_D: u8 = 0b0000_1000;
pub const FLAG_S: u8 = 0b0001_0000;
pub const FLAG_B: u8 = 0b0010_0000;
pub const FLAG_V: u8 = 0b0100_0000;
pub const FLAG_N: u8 = 0b1000_0000;

pub const POWERUP_SP: u8 = 0xFD;
pub const STACK_PAGE: u16 = 0x0100;

pub const NMI_VECTOR: u16 = 0xFFFA;
pub const RES_VECTOR: u16 = 0xFFFC;
pub const IRQ_VECTOR: u16 = 0xFFFE;

#[inline(always)]
pub fn split(u: u16) -> (u8, u8) {
    let lower = (u & 0xFF) as u8;
    let upper = (u >> 8) as u8;
    (lower, upper)
}

#[inline(always)]
pub fn combine_bytes(lower: u8, upper: u8) -> u16 {
    ((upper as u16) << 8) + (lower as u16)
}

pub struct CPU {
    // Program Counter
    pub pcl: u8,
    pub pch: u8,
    
    // Processor Status Register
    pub flag_c: bool,
    pub flag_z: bool,
    pub flag_i: bool,
    pub flag_d: bool,
    pub flag_b: bool,
    pub flag_s: bool,
    pub flag_v: bool,
    pub flag_n: bool,
    
    // Primary Registers
    pub acc: u8,
    pub xir: u8,
    pub yir: u8,
    pub sp: u8,
}

impl fmt::Display for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let foo = format!("PC: {:04X}, P: {:08b}, A: {:02X}, X: {:02X}, Y: {:02X}, SP: {:02X}",
                          combine_bytes(self.pcl, self.pch),
                          self.flags_as_byte(),
                          self.acc,
                          self.xir,
                          self.yir,
                          self.sp);

        write!(f, "{}", foo)
    }
}

impl<'a> CPU {
    pub fn new() -> CPU {
        CPU {
            pcl: 0,
            pch: 0,
            flag_c: false,
            flag_z: false,
            flag_i: true,
            flag_d: false,
            flag_b: false,
            flag_s: true,
            flag_v: false,
            flag_n: false,
            acc: 0,
            xir: 0,
            yir: 0,
            sp: 0//POWERUP_SP,
        }
    }
    
    pub fn reset(&mut self, membox: &mut Memory) {
        self.sp = self.sp.wrapping_sub(3);// -= 3;
        //let (pcl_s, pch_s) = split(RES_VECTOR);
        self.pcl = membox.read(RES_VECTOR);
        self.pch = membox.read(RES_VECTOR + 1);
        self.flag_i = true;
    }

    pub fn increment_pc(&mut self) {
        self.pcl = self.pcl.wrapping_add(1);
        if self.pcl == 0 {
            self.pch = self.pch.wrapping_add(1);
        }
    }

    pub fn decrement_pc(&mut self) {
        self.pcl = self.pcl.wrapping_sub(1);
        if self.pcl == 255 {
            self.pch = self.pch.wrapping_sub(1);
        }
    }

    pub fn read_pc(&mut self, membox: &'a mut Memory) -> u8 {
        let addr = combine_bytes(self.pcl, self.pch);
        self.increment_pc();
        membox.read(addr)
    }
    
    #[inline(always)]
    fn set_zn(&mut self, val: u8) {
        self.flag_z = val == 0;
        self.flag_n = val > 127;
    }

    #[inline(always)]
    fn set_czn(&mut self, val: u8, carry: bool) {
        self.set_zn(val);
        self.flag_c = carry;
    }

    #[inline(always)]
    fn set_cznv(&mut self, val: u8, carry: bool, overflow: bool) {
        self.set_czn(val, carry);
        self.flag_v = overflow;
    }

    fn flags_as_byte(&self) -> u8 {
        (self.flag_c as u8) |
        ((self.flag_z as u8) << 1) |
        ((self.flag_i as u8) << 2) |
        ((self.flag_d as u8) << 3) |
        ((self.flag_b as u8) << 5) |
        ((self.flag_s as u8) << 4) |
        ((self.flag_v as u8) << 6) |
        ((self.flag_n as u8) << 7) 
    }

    fn set_flags(&mut self, flags: u8) {
        self.flag_c = flags & 1 == 1;
        self.flag_z = (flags >> 1) & 1 == 1;
        self.flag_i = (flags >> 2) & 1 == 1;
        self.flag_d = (flags >> 3) & 1 == 1;
        self.flag_s = (flags >> 4) & 1 == 1;
        self.flag_b = (flags >> 5) & 1 == 1;
        self.flag_v = (flags >> 6) & 1 == 1;
        self.flag_n = (flags >> 7) & 1 == 1;
    }

    fn stack_push(&mut self, membox: &mut Memory, val: u8) {
        let addr = (self.sp as u16) + STACK_PAGE;
        membox.cpu_ram[addr as usize] = val;
        let new_sp = self.sp.wrapping_sub(1);
        self.sp = new_sp;
    }

    fn stack_pop(&mut self, membox: &Memory) -> u8 {
        let new_sp = self.sp.wrapping_add(1);
        let addr = (new_sp as u16)  + STACK_PAGE;
        self.sp = new_sp;
        let val = membox.cpu_ram[addr as usize];
        val
    }

    fn stack_push_pc(&mut self, membox: &mut Memory) {
        let addr = (self.sp as u16) + STACK_PAGE;
        membox.cpu_ram[addr as usize] = self.pch;
        membox.cpu_ram[(addr as usize).wrapping_sub(1)] = self.pcl;
        let new_sp = self.sp.wrapping_sub(2);
        self.sp = new_sp;
    }

    fn stack_pop_pc(&mut self, membox: &mut Memory) {
        let addr = (self.sp as u16) + STACK_PAGE;
        self.pcl = membox.cpu_ram[(addr as usize).wrapping_add(1)];
        self.pch = membox.cpu_ram[(addr as usize).wrapping_add(2)];
        let new_sp = self.sp.wrapping_add(2);
        self.sp = new_sp;
    }

    pub fn print_stack(&self, membox: &Memory) {
        print!("    ");
        for n in 0 .. 16 {
            print!("{:02X} ", n);
        }
        println!("");
        for n in 0 .. 16 {
            print!("{:02X}: ", n);
            for m in 0 .. 16 {
                print!("{:02X} ", membox.cpu_ram[n * 16 + m + 0x0100])
            }
            println!("");
        }
    }

    fn interrupt(&mut self, membox: &mut Memory, vector: u16) {
        self.stack_push_pc(membox);
        let flags = self.flags_as_byte();
        self.flag_i = true;
        self.stack_push(membox, flags);
        self.pcl = membox.read(vector);
        self.pch = membox.read(vector + 1);
    }

    pub fn exec(&mut self, membox: &mut Memory, interrupt: Interrupt) {
        match interrupt {
            Interrupt::IRQ => return self.interrupt(membox, IRQ_VECTOR),
            Interrupt::NMI => return self.interrupt(membox, NMI_VECTOR),
            Interrupt::Nil => {} // do nothing
        }
        
        let opcode = self.read_pc(membox);
        match opcode {
            0x00 => brk_imp(self, membox),
            0x01 => ora_ixi(self, membox),
            0x05 => ora_zpg(self, membox),
            0x06 => asl_zpg(self, membox),
            0x08 => php_imp(self, membox),
            0x09 => ora_imm(self, membox),
            0x0A => asl_acc(self, membox),
            0x0D => ora_abs(self, membox),
            0x0E => asl_abs(self, membox),
            
            0x10 => bpl_rel(self, membox),
            0x11 => ora_iyi(self, membox),
            0x15 => ora_zpx(self, membox),
            0x16 => asl_zpx(self, membox),
            0x18 => clc_imp(self, membox),
            0x19 => ora_aby(self, membox),
            0x1D => ora_abx(self, membox),
            0x1E => asl_abx(self, membox),
            
            0x20 => jsr_imp(self, membox),
            0x21 => and_ixi(self, membox),
            0x24 => bit_zpg(self, membox),
            0x25 => and_zpg(self, membox),
            0x26 => rol_zpg(self, membox),
            0x28 => plp_imp(self, membox),
            0x29 => and_imm(self, membox),
            0x2A => rol_acc(self, membox),
            0x2C => bit_abs(self, membox),
            0x2D => and_abs(self, membox),
            0x2E => rol_abs(self, membox),
            
            0x30 => bmi_rel(self, membox),
            0x31 => and_iyi(self, membox),
            0x35 => and_zpx(self, membox),
            0x36 => rol_zpx(self, membox),
            0x38 => sec_imp(self, membox),
            0x39 => and_aby(self, membox),
            0x3D => and_abx(self, membox),
            0x3E => rol_abx(self, membox),
            
            0x40 => rti_imp(self, membox),
            0x41 => eor_ixi(self, membox),
            0x45 => eor_zpg(self, membox),
            0x46 => lsr_zpg(self, membox),
            0x48 => pha_imp(self, membox),
            0x49 => eor_imm(self, membox),
            0x4A => lsr_acc(self, membox),
            0x4C => jmp_abs(self, membox),
            0x4D => eor_abs(self, membox),
            0x4E => lsr_abs(self, membox),
            
            0x50 => bvc_rel(self, membox),
            0x51 => eor_iyi(self, membox),
            0x55 => eor_zpx(self, membox),
            0x56 => lsr_zpx(self, membox),
            0x58 => cli_imp(self, membox),
            0x59 => eor_aby(self, membox),
            0x5D => eor_abx(self, membox),
            0x5E => lsr_abx(self, membox),
            
            0x60 => rts_imp(self, membox),
            0x61 => adc_ixi(self, membox),
            0x65 => adc_zpg(self, membox),
            0x66 => ror_zpg(self, membox),
            0x68 => pla_imp(self, membox),
            0x69 => adc_imm(self, membox),
            0x6A => ror_acc(self, membox),
            0x6C => jmp_ind(self, membox),
            0x6D => adc_abs(self, membox),
            0x6E => ror_abs(self, membox),
            
            0x70 => bvs_rel(self, membox),
            0x71 => adc_iyi(self, membox),
            0x75 => adc_zpx(self, membox),
            0x76 => ror_zpx(self, membox),
            0x78 => sei_imp(self, membox),
            0x79 => adc_aby(self, membox),
            0x7D => adc_abx(self, membox),
            0x7E => ror_abx(self, membox),
            
            0x81 => sta_ixi(self, membox),
            0x84 => sty_zpg(self, membox),
            0x85 => sta_zpg(self, membox),
            0x86 => stx_zpg(self, membox),
            0x88 => dey_imp(self, membox),
            0x8A => txa_imp(self, membox),
            0x8C => sty_abs(self, membox),
            0x8D => sta_abs(self, membox),
            0x8E => stx_abs(self, membox),
            
            0x90 => bcc_rel(self, membox),
            0x91 => sta_iyi(self, membox),
            0x94 => sty_zpx(self, membox),
            0x95 => sta_zpx(self, membox),
            0x96 => stx_zpy(self, membox),
            0x98 => tya_imp(self, membox),
            0x99 => sta_aby(self, membox),
            0x9A => txs_imp(self, membox),
            0x9D => sta_abx(self, membox),
            
            0xA0 => ldy_imm(self, membox),
            0xA1 => lda_ixi(self, membox),
            0xA2 => ldx_imm(self, membox),
            0xA4 => ldy_zpg(self, membox),
            0xA5 => lda_zpg(self, membox),
            0xA6 => ldx_zpg(self, membox),
            0xA8 => tay_imp(self, membox),
            0xA9 => lda_imm(self, membox),
            0xAA => tax_imp(self, membox),
            0xAC => ldy_abs(self, membox),
            0xAD => lda_abs(self, membox),
            0xAE => ldx_abs(self, membox),
            
            0xB0 => bcs_rel(self, membox),
            0xB1 => lda_iyi(self, membox),
            0xB4 => ldy_zpx(self, membox),
            0xB5 => lda_zpx(self, membox),
            0xB6 => ldx_zpy(self, membox),
            0xB8 => clv_imp(self, membox),
            0xB9 => lda_aby(self, membox),
            0xBA => tsx_imp(self, membox),
            0xBC => ldy_abx(self, membox),
            0xBD => lda_abx(self, membox),
            0xBE => ldx_aby(self, membox),
            
            0xC0 => cpy_imm(self, membox),
            0xC1 => cmp_ixi(self, membox),
            0xC4 => cpy_zpg(self, membox),
            0xC5 => cmp_zpg(self, membox),
            0xC6 => dec_zpg(self, membox),
            0xC8 => iny_imp(self, membox),
            0xC9 => cmp_imm(self, membox),
            0xCA => dex_imp(self, membox),
            0xCC => cpy_abs(self, membox),
            0xCD => cmp_abs(self, membox),
            0xCE => dec_abs(self, membox),
            
            0xD0 => bne_rel(self, membox),
            0xD1 => cmp_iyi(self, membox),
            0xD5 => cmp_zpx(self, membox),
            0xD6 => dec_zpx(self, membox),
            0xD8 => cld_imp(self, membox),
            0xD9 => cmp_aby(self, membox),
            0xDE => dec_abx(self, membox),
            
            0xE0 => cpx_imm(self, membox),
            0xE1 => sbc_ixi(self, membox),
            0xE4 => cpx_zpg(self, membox),
            0xE5 => sbc_zpg(self, membox),
            0xE6 => inc_zpg(self, membox),
            0xE8 => inx_imp(self, membox),
            0xE9 => sbc_imm(self, membox),
            0xEA => nop_imp(self, membox),
            0xEC => cpx_abs(self, membox),
            0xED => sbc_abs(self, membox),
            0xEE => inc_abs(self, membox),
            
            0xF0 => beq_rel(self, membox),
            0xF1 => sbc_iyi(self, membox),
            0xF5 => sbc_zpx(self, membox),
            0xF6 => inc_zpx(self, membox),
            0xF8 => sed_imp(self, membox),
            0xF9 => sbc_aby(self, membox),
            0xFD => sbc_abx(self, membox),
            0xFE => inc_abx(self, membox),
            _ => hlt_imp(self, membox),
        }
    }
}
