/* Store Operations */
/* STA zpg  STX zpg  STY zpg */

use super::super::CPU;
use super::super::super::Memory;
use super::super::combine_bytes;

pub fn two_byte_write(membox: &mut Memory, adl: u8, adh: u8, val: u8) {
    let eff_addr = combine_bytes(adl, adh);
    membox.write(eff_addr, val);
}


pub fn sta_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    membox.cpu_ram[adl as usize] = cpu.acc;
}


pub fn stx_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    membox.cpu_ram[adl as usize] = cpu.xir;
}


pub fn sty_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    membox.cpu_ram[adl as usize] = cpu.yir;
}

/* STA abs  STX abs  STY abs */


pub fn sta_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    two_byte_write(membox, adl, adh, cpu.acc);
}


pub fn stx_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    two_byte_write(membox, adl, adh, cpu.xir);
}


pub fn sty_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    two_byte_write(membox, adl, adh, cpu.yir);
}

/* STA ixi  --- ---  --- ---  */


pub fn sta_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.xir);
    let adh = bal.wrapping_add(cpu.xir).wrapping_add(1);
    two_byte_write(membox, adl, adh, cpu.acc);
}

/* STA abx  --- ---  --- ---  
 * STA aby  --- ---  --- ---*/


pub fn sta_abx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.xir as u16);
    membox.write(eff_addr, cpu.acc);
}


pub fn sta_aby(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.yir as u16);
    membox.write(eff_addr, cpu.acc);
}

/* STA zpx  --- ---  STY zpx
 * --- ---  STX zpy  --- ---*/


pub fn sta_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.xir);
    two_byte_write(membox, adl, 0, cpu.acc);
}


pub fn sty_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.xir);
    two_byte_write(membox, adl, 0, cpu.yir);
}


pub fn stx_zpy(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.yir);
    two_byte_write(membox, adl, 0, cpu.xir);
}

/* STA iyi  --- ---  --- ---*/


pub fn sta_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let ial = cpu.read_pc(membox);
    let bal = membox.cpu_ram[ial as usize];
    let bah = membox.cpu_ram[ial.wrapping_add(1) as usize];
    let adl = bal.wrapping_add(cpu.yir);
    let adh = bah;
    two_byte_write(membox, adl, adh, cpu.acc);
}
