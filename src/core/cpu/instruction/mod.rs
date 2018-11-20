mod single_byte;
mod internal;
mod store;
mod rmw;

use super::CPU;
use super::FLAG_S;
use super::FLAG_B;
use super::IRQ_VECTOR;
use super::super::Memory;
pub use self::single_byte::*;
pub use self::internal::*;
pub use self::store::*;
pub use self::rmw::*;

pub fn hlt_imp(cpu: &mut CPU, membox: &mut Memory) {
    cpu.decrement_pc();
    let opcode = cpu.read_pc(membox);
    panic!(format!("Illegal instruction: {:02X}", opcode));
}

pub fn php_imp(cpu: &mut CPU, membox: &mut Memory) {
    let flags = cpu.flags_as_byte() | FLAG_S | FLAG_B;
    cpu.stack_push(membox, flags);
}

pub fn pha_imp(cpu: &mut CPU, membox: &mut Memory) {
    let val = cpu.acc;
    cpu.stack_push(membox, val);
}

pub fn plp_imp(cpu: &mut CPU, membox: &mut Memory) {
    let flags = cpu.stack_pop(membox);
    cpu.set_flags(flags);
}

pub fn pla_imp(cpu: &mut CPU, membox: &mut Memory) {
    let acc = cpu.stack_pop(membox);
    cpu.acc = acc;
}

pub fn jsr_imp(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    cpu.stack_push_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.pcl = adl;
    cpu.pch = adh;
}

pub fn brk_imp(cpu: &mut CPU, membox: &mut Memory) {
    cpu.increment_pc();
    cpu.flag_s = true;
    cpu.interrupt(membox, IRQ_VECTOR);
}

pub fn rti_imp(cpu: &mut CPU, membox: &mut Memory) {
    plp_imp(cpu, membox);
    cpu.stack_pop_pc(membox);
}

pub fn jmp_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.pcl = adl;
    cpu.pch = adh;
}

pub fn jmp_ind(cpu: &mut CPU, membox: &mut Memory) {
    let ial = cpu.read_pc(membox) as u16;
    let iah = cpu.read_pc(membox) as u16;
    let adl = membox.read((iah << 8) + ial);
    let adh = membox.read((iah << 8) + ial + 1);
    cpu.pcl = adl;
    cpu.pch = adh;
}

pub fn rts_imp(cpu: &mut CPU, membox: &mut Memory) {
    cpu.stack_pop_pc(membox);
    cpu.increment_pc();
}

#[inline(always)]
pub fn branch(cpu: &mut CPU, membox: &mut Memory, flag: bool) {
    let offset = cpu.read_pc(membox);
    if !flag {
        return;
    }
    
    if offset > 127 {
        //subtract one from pch if pcl < 1 + !offset
        if cpu.pcl < 1 + !offset {
            cpu.pch = cpu.pch.wrapping_sub(1);
        }
    } else {
        //carry if pcl + offset > 255 -> offset > 255 - pcl
        if offset > 255 - cpu.pcl {
            cpu.pch = cpu.pch.wrapping_add(1);
        }
    }
    
    cpu.pcl = cpu.pcl.wrapping_add(offset);
}

pub fn bcc_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = !cpu.flag_c;
    branch(cpu, membox, flag);
}

pub fn bcs_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = cpu.flag_c;
    branch(cpu, membox, flag);
}

pub fn beq_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = cpu.flag_z;
    branch(cpu, membox, flag);
}

pub fn bmi_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = cpu.flag_n;
    branch(cpu, membox, flag);
}

pub fn bne_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = !cpu.flag_z;
    branch(cpu, membox, flag);
}

pub fn bpl_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = !cpu.flag_n;
    branch(cpu, membox, flag);
}

pub fn bvc_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = !cpu.flag_v;
    branch(cpu, membox, flag);
}

pub fn bvs_rel(cpu: &mut CPU, membox: &mut Memory) {
    let flag = cpu.flag_v;
    branch(cpu, membox, flag);
}
