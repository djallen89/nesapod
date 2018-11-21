/* Store Operations */
/* STA zpg  STX zpg  STY zpg */

use super::read_one_byte;
use super::read_two_bytes;
use super::super::CPU;
use super::super::super::Memory;
use super::super::combine_bytes;
use super::super::split;

#[cfg(feature = "debug")]
pub fn two_byte_write(cpu: &mut CPU,
                      membox: &mut Memory,
                      adl: u8,
                      adh: u8,
                      val: u8) {
    let eff_addr = combine_bytes(adl, adh);
    let old_val = membox.read(eff_addr);
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = old_val;
    //println!("2BW: Address is {:04X}, val is {:02X}", eff_addr, val);
    membox.write(eff_addr, val);
}

#[cfg(not(feature = "debug"))]
pub fn two_byte_write(_cpu: &mut CPU,
                      membox: &mut Memory,
                      adl: u8,
                      adh: u8,
                      val: u8) {
    let eff_addr = combine_bytes(adl, adh);
    membox.write(eff_addr, val);
}

#[inline(always)]
#[cfg(feature = "debug")]
pub fn one_byte_write(cpu: &mut CPU,
                      membox: &mut Memory,
                      adl: u8,
                      val: u8) {
    
    let old_val = membox.cpu_ram[adl as usize];
    cpu.last_eff_addr = adl as u16;
    cpu.last_val = old_val;
    //println!("1BW: Address is {:04X}, val is {:02X}", adl, val);
    membox.cpu_ram[adl as usize] = val;
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
pub fn one_byte_write(membox: &mut Memory, addr: u8, val: u8) {
    membox.cpu_ram[adl as usize] = val;
}

pub fn sta_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = read_one_byte(cpu, membox);
    let val = cpu.acc;
    one_byte_write(cpu, membox, adl, val);
}


pub fn stx_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = read_one_byte(cpu, membox);
    let val = cpu.xir;
    one_byte_write(cpu, membox, adl, val);
}


pub fn sty_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = read_one_byte(cpu, membox);
    let val = cpu.yir;
    one_byte_write(cpu, membox, adl, val);
}

/* STA abs  STX abs  STY abs */
pub fn sta_abs(cpu: &mut CPU, membox: &mut Memory) {
    let (adl, adh) = read_two_bytes(cpu, membox);
    let val = cpu.acc;
    two_byte_write(cpu, membox, adl, adh, val);
}


pub fn stx_abs(cpu: &mut CPU, membox: &mut Memory) {
    let (adl, adh) = read_two_bytes(cpu, membox);
    let val = cpu.xir;
    two_byte_write(cpu, membox, adl, adh, val);
}


pub fn sty_abs(cpu: &mut CPU, membox: &mut Memory) {
    let (adl, adh) = read_two_bytes(cpu, membox);
    let val = cpu.yir;
    two_byte_write(cpu, membox, adl, adh, val);
}

/* STA ixi  --- ---  --- ---  */
pub fn sta_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let mut bal = read_one_byte(cpu, membox);
    bal = bal.wrapping_add(cpu.xir);
    let adl = membox.read(bal as u16);
    bal = bal.wrapping_add(1);
    let adh = membox.read(bal as u16);
    let val = cpu.acc;
    two_byte_write(cpu, membox, adl, adh, val);
}

/* STA abx  --- ---  --- ---  
 * STA aby  --- ---  --- ---*/
#[cfg(feature = "debug")]
pub fn sta_abx(cpu: &mut CPU, membox: &mut Memory) {
    let (bal, bah) = read_two_bytes(cpu, membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.xir as u16);
    let val = membox.read(eff_addr);
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = val;
    //cpu.msg = format!("${:04X} = #${:02X}", eff_addr, val);
    membox.write(eff_addr, cpu.acc);
}

#[cfg(not(feature = "debug"))]
pub fn sta_abx(cpu: &mut CPU, membox: &mut Memory) {
    let (bal, bah) = read_two_bytes(cpu, membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.xir as u16);
    membox.write(eff_addr, cpu.acc);
}

#[cfg(feature = "debug")]
pub fn sta_aby(cpu: &mut CPU, membox: &mut Memory) {
    let (bal, bah) = read_two_bytes(cpu, membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.yir as u16);
    let val = membox.read(eff_addr);
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = val;
    //cpu.msg = format!("${:04X} = #${:02X}", eff_addr, val);
    membox.write(eff_addr, cpu.acc);
}

#[cfg(not(feature = "debug"))]
pub fn sta_aby(cpu: &mut CPU, membox: &mut Memory) {
    let (bal, bah) = read_two_bytes(cpu, membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.yir as u16);
    membox.write(eff_addr, cpu.acc);
}

/* STA zpx  --- ---  STY zpx
 * --- ---  STX zpy  --- ---*/
pub fn sta_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = read_one_byte(cpu, membox);
    let adl = bal.wrapping_add(cpu.xir);
    let val = cpu.acc;
    two_byte_write(cpu, membox, adl, 0, val);
}


pub fn sty_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = read_one_byte(cpu, membox);
    let adl = bal.wrapping_add(cpu.xir);
    let val = cpu.yir;
    two_byte_write(cpu, membox, adl, 0, val);
}


pub fn stx_zpy(cpu: &mut CPU, membox: &mut Memory) {
    let bal = read_one_byte(cpu, membox);
    let adl = bal.wrapping_add(cpu.yir);
    let val = cpu.xir;
    two_byte_write(cpu, membox, adl, 0, val);
}

/* STA iyi  --- ---  --- ---*/
pub fn sta_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let ial = read_one_byte(cpu, membox);
    let bal = membox.cpu_ram[ial as usize];
    let bah = membox.cpu_ram[ial.wrapping_add(1) as usize];
    let addr = combine_bytes(bal, bah);
    let (adl, adh) = split(addr + (cpu.yir as u16));
    let val = cpu.acc;
    two_byte_write(cpu, membox, adl, adh, val);
}
