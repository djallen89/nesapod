use std::u8;

use super::super::CPU;
use super::super::FLAG_V;
use super::super::super::Memory;
use super::super::combine_bytes;

/* Internal Execution On Memory Data
 *  ADC, SBC, 
 *  AND, EOR, ORA, BIT, 
 *  CMP, CPX, CPY,
 *  LDA, LDX, LDY
 */

/* Immediate Mode */

#[inline(always)]
pub fn adc_imm(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = cpu.read_pc(membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_imm(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = cpu.read_pc(membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_imm(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = cpu.read_pc(membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_imm(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = cpu.read_pc(membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_imm(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = cpu.read_pc(membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn cmp_imm(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = cpu.read_pc(membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn cpx_imm(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.xir;
    let rhs = cpu.read_pc(membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn cpy_imm(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.yir;
    let rhs = cpu.read_pc(membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_imm(cpu: &mut CPU, membox: &mut Memory) {
    cpu.acc = load_imm(cpu, membox);
}

#[inline(always)]
pub fn ldx_imm(cpu: &mut CPU, membox: &mut Memory) {
    cpu.xir = load_imm(cpu, membox);
}

#[inline(always)]
pub fn ldy_imm(cpu: &mut CPU, membox: &mut Memory) {
    cpu.yir = load_imm(cpu, membox);
}

/* Zero Page */

#[inline(always)]
pub fn adc_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpg_read(cpu, membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpg_read(cpu, membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpg_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpg_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpg_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn bit_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let val = zpg_read(cpu, membox);
    bit(cpu, val);
}

#[inline(always)]
pub fn cmp_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = zpg_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn cpx_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.xir;
    let rhs = zpg_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn cpy_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.yir;
    let rhs = zpg_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_zpg(cpu: &mut CPU, membox: &mut Memory) {
    cpu.acc = load_zpg(cpu, membox);
}

#[inline(always)]
pub fn ldx_zpg(cpu: &mut CPU, membox: &mut Memory) {
    cpu.xir = load_zpg(cpu, membox);
}

#[inline(always)]
pub fn ldy_zpg(cpu: &mut CPU, membox: &mut Memory) {
    cpu.yir = load_zpg(cpu, membox);
}

/* Absolute Addressing */

#[inline(always)]
pub fn adc_abs(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = abs_read(cpu, membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_abs(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = abs_read(cpu, membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_abs(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = abs_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_abs(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = abs_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_abs(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = abs_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn bit_abs(cpu: &mut CPU, membox: &mut Memory) {
    let val = abs_read(cpu, membox);
    bit(cpu, val);
}

#[inline(always)]
pub fn cmp_abs(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = abs_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn cpx_abs(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.xir;
    let rhs = abs_read(cpu, membox);
    compare(cpu, lhs, rhs);
}
    
#[inline(always)]
pub fn cpy_abs(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.yir;
    let rhs = abs_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_abs(cpu: &mut CPU, membox: &mut Memory) {
    cpu.acc = load_abs(cpu, membox);
}

#[inline(always)]
pub fn ldx_abs(cpu: &mut CPU, membox: &mut Memory) {
    cpu.xir = load_abs(cpu, membox);
}

#[inline(always)]
pub fn ldy_abs(cpu: &mut CPU, membox: &mut Memory) {
    cpu.yir = load_abs(cpu, membox);
}

/* Indirect, X */


#[inline(always)]
pub fn adc_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn cmp_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = ixi_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_ixi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = ixi_read(cpu, membox);
    cpu.set_zn(rhs);
    cpu.acc = rhs;
}

/* Absolute, X and Absolute, Y */
#[inline(always)]
pub fn adc_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn adc_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn sbc_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn and_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn eor_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn ora_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn cmp_aby(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    cpu.set_zn(rhs);
    cpu.acc = rhs;
}

#[inline(always)]
pub fn lda_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    cpu.set_zn(rhs);
    cpu.acc = rhs;
}

#[inline(always)]
pub fn ldx_aby(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.yir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    cpu.set_zn(rhs);
    cpu.xir = rhs;
}

#[inline(always)]
pub fn ldy_abx(cpu: &mut CPU, membox: &mut Memory) {
    let ireg = cpu.xir;
    let rhs = ab_ir_read(cpu, membox, ireg);
    cpu.set_zn(rhs);
    cpu.yir = rhs;
}

/* Zero Page, X and Zero Page, Y */
#[inline(always)]
pub fn adc_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn cmp_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = zpx_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    cpu.set_zn(rhs);
    cpu.acc = rhs;
}

#[inline(always)]
pub fn ldy_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = zpx_read(cpu, membox);
    cpu.set_zn(rhs);
    cpu.yir = rhs;
}

/* Indirect, Y */
#[inline(always)]
pub fn adc_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    adc_internal(cpu, rhs);
}

#[inline(always)]
pub fn sbc_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    adc_internal(cpu, !rhs);
}

#[inline(always)]
pub fn and_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc & rhs });
}

#[inline(always)]
pub fn eor_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc ^ rhs });
}

#[inline(always)]
pub fn ora_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    logical(cpu, rhs, &|acc, rhs| { acc | rhs });
}

#[inline(always)]
pub fn cmp_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let lhs = cpu.acc;
    let rhs = iyi_read(cpu, membox);
    compare(cpu, lhs, rhs);
}

#[inline(always)]
pub fn lda_iyi(cpu: &mut CPU, membox: &mut Memory) {
    let rhs = iyi_read(cpu, membox);
    cpu.set_zn(rhs);
    cpu.acc = rhs;
}

#[inline(always)]
pub fn ldx_zpy(cpu: &mut CPU, membox: &mut Memory) {
    let base_addr = cpu.read_pc(membox);
    let eff_addr = base_addr.wrapping_add(cpu.yir);
    let rhs = membox.cpu_ram[eff_addr as usize];
    cpu.set_zn(rhs);
    cpu.yir = rhs;
}

#[inline(always)]
pub fn adc_internal(cpu: &mut CPU, rhs: u8) {
    let carry = cpu.flag_c as u8;
    let lhs = cpu.acc;
    let next_carry = (rhs == 255 && cpu.flag_c) || u8::MAX - rhs - carry < lhs;
    let result = lhs.wrapping_add(rhs.wrapping_add(carry));
    let overflow = ((!(lhs ^ rhs) & 0b1000_0000) == 0b1000_0000) &&
        ((lhs ^ result) & 0b1000_0000) == 0b1000_0000;
    cpu.set_cznv(result, next_carry, overflow);
    cpu.acc = result;
}

#[inline(always)]
pub fn compare(cpu: &mut CPU, lhs: u8, rhs: u8) {
    let result = lhs.wrapping_sub(rhs);
    let carry = lhs >= rhs;
    cpu.set_czn(result, carry);
}

#[inline(always)]
pub fn logical(cpu: &mut CPU, rhs: u8, op: &Fn(u8, u8) -> u8) {
    let result = op(cpu.acc, rhs);
    cpu.set_zn(result);
    cpu.acc = result;
}

#[inline(always)]
pub fn load_imm(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let rhs = cpu.read_pc(membox);
    cpu.set_zn(rhs);
    rhs
}

#[inline(always)]
pub fn zpg_read(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let eff_addr = cpu.read_pc(membox);
    membox.cpu_ram[eff_addr as usize]
}

#[inline(always)]
pub fn load_zpg(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let rhs = zpg_read(cpu, membox);
    cpu.set_zn(rhs);
    rhs
}

#[inline(always)]
pub fn two_byte_read(membox: &mut Memory, adl: u8, adh: u8) -> u8 {
    let eff_addr = combine_bytes(adl, adh);
    membox.read(eff_addr)
}

#[inline(always)]
pub fn abs_read(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    two_byte_read(membox, adl, adh)
}

#[inline(always)]
pub fn bit(cpu: &mut CPU, val: u8) {
    cpu.flag_z = cpu.acc & val == 0;
    cpu.flag_n = val > 127;
    cpu.flag_v = (val & FLAG_V) == FLAG_V;
}

#[inline(always)]
pub fn load_abs(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let rhs = abs_read(cpu, membox);
    cpu.set_zn(rhs);
    rhs
}

#[inline(always)]
pub fn ixi_read(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let mut base_addr = cpu.read_pc(membox);
    base_addr = base_addr.wrapping_add(cpu.xir);
    let adl = membox.cpu_ram[base_addr as usize];
    let adh = membox.cpu_ram[base_addr.wrapping_add(1) as usize];
    two_byte_read(membox, adl, adh)
}

#[inline(always)]
pub fn ab_ir_read(cpu: &mut CPU, membox: &mut Memory, ireg: u8) -> u8 {
    let base_addr_lo = cpu.read_pc(membox);
    let base_addr_hi = cpu.read_pc(membox);
    let adl = base_addr_lo.wrapping_add(ireg);
    //carry results if base_addr_lo + ireg > 255
    let c = if ireg > (255 - base_addr_lo) {
        1
    } else {
        0
    };
    let adh = base_addr_hi + c;
    two_byte_read(membox, adl, adh)
}

#[inline(always)]
pub fn zpx_read(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let base_addr = cpu.read_pc(membox);
    let eff_addr = base_addr.wrapping_add(cpu.xir);
    membox.cpu_ram[eff_addr as usize]
}


#[inline(always)]
pub fn iyi_read(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let mut intermediate_addr = cpu.read_pc(membox);
    let base_addr_lo = membox.cpu_ram[intermediate_addr as usize];
    intermediate_addr = intermediate_addr.wrapping_add(1);
    let base_addr_hi = membox.cpu_ram[intermediate_addr as usize];
    let adl = base_addr_lo.wrapping_add(cpu.yir);
    let c = if cpu.yir > (255 - base_addr_lo) {
        1
    } else {
        0
    };
    let adh = base_addr_hi + c;
    two_byte_read(membox, adl, adh)
}
