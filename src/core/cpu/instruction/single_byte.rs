/* SINGLE BYTE INSTRUCTIONS */

use super::super::CPU;
use super::super::super::Memory;

pub fn asl_acc(cpu: &mut CPU, _membox: &mut Memory) {
    let carry = cpu.acc < 127;
    cpu.acc <<= 1;
    let res = cpu.acc;
    cpu.set_czn(res, carry);
}


pub fn clc_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_c = false;
}


pub fn cld_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_d = false;
}


pub fn cli_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_i = false;
}


pub fn clv_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_v = false;
}


pub fn dex_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.xir -= 1;
    let res = cpu.xir;
    cpu.set_zn(res);
}


pub fn dey_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.yir -= 1;
    let res = cpu.yir;
    cpu.set_zn(res);
}


pub fn inx_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.xir += 1;
    let res = cpu.xir;
    cpu.set_zn(res);
}


pub fn iny_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.yir += 1;
    let res = cpu.yir;
    cpu.set_zn(res);
}


pub fn lsr_acc(cpu: &mut CPU, _membox: &mut Memory) {
    let carry = (cpu.acc & 0b0000_0001) == 0b0000_0001;
    cpu.acc <<= 1;
    let res = cpu.acc;
    cpu.set_czn(res, carry);
}


pub fn nop_imp(_cpu: &mut CPU, _membox: &mut Memory) {
    ();
}


pub fn rol_acc(cpu: &mut CPU, _membox: &mut Memory) {
    let old_carry = cpu.flag_c as u8;
    let next_carry = (cpu.acc & 0b1000_0000) == 0b1000_0000;
    let res = (cpu.acc << 1) | old_carry;
    cpu.set_czn(res, next_carry);
}


pub fn ror_acc(cpu: &mut CPU, _membox: &mut Memory) {
    let old_carry = cpu.flag_c as u8;
    let next_carry = (cpu.acc & 0b0000_0001) == 0b0000_0001;
    let res = (cpu.acc >> 1) | (old_carry << 7);
    cpu.set_czn(res, next_carry);
}


pub fn sec_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_c = true;
}


pub fn sed_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_d = true;
}


pub fn sei_imp(cpu: &mut CPU, _membox: &mut Memory) {
    cpu.flag_i = true;
}


pub fn tax_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.acc;
    cpu.xir = val;
    cpu.set_zn(val);
}

pub fn tay_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.acc;
    cpu.yir = val;
    cpu.set_zn(val);
}

pub fn tsx_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.sp;
    cpu.xir = val;
    cpu.set_zn(val);
}

pub fn txa_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.xir;
    cpu.acc = val;
    cpu.set_zn(val);
}

pub fn txs_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.xir;
    cpu.sp = val;
    cpu.set_zn(val);
}

pub fn tya_imp(cpu: &mut CPU, _membox: &mut Memory) {
    let val = cpu.yir;
    cpu.acc = val;
    cpu.set_zn(val);
}
