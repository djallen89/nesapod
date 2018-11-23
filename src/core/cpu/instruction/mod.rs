mod single_byte;
mod internal;
mod store;
mod rmw;

use super::CPU;
use super::FLAG_S;
use super::FLAG_B;
use super::IRQ_VECTOR;
use super::combine_bytes;
use super::super::Memory;
pub use self::single_byte::*;
pub use self::internal::*;
pub use self::store::*;
pub use self::rmw::*;

#[inline(always)]
#[cfg(feature = "debug")]
pub fn read_one_byte(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let rhs = cpu.read_pc(membox);
    cpu.byte_1 = rhs;
    rhs
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
pub fn read_one_byte(cpu: &mut CPU, membox: &mut Memory) -> u8 {
    let rhs = cpu.read_pc(membox);
    rhs
}

#[inline(always)]
#[cfg(feature = "debug")]
pub fn read_two_bytes(cpu: &mut CPU, membox: &mut Memory) -> (u8, u8) {
    let lo = cpu.read_pc(membox);
    let hi = cpu.read_pc(membox);
    cpu.byte_1 = lo;
    cpu.byte_2 = hi;
    (lo, hi)
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
pub fn read_two_bytes(cpu: &mut CPU, membox: &mut Memory) -> (u8, u8) {
    let lo = cpu.read_pc(membox);
    let hi = cpu.read_pc(membox);
    (lo, hi)
}

pub fn hlt_imp(cpu: &mut CPU, _membox: &mut Memory) {
    panic!(format!("Illegal instruction: PC = {:02X}{:02X}, Op = {:02X}",
                   cpu.pch,
                   cpu.pcl,
                   cpu.last_op
    ));
    //cpu.increment_pc();
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
    let flags = cpu.stack_pop(membox) & !FLAG_B;
    cpu.set_flags(flags);
}

pub fn pla_imp(cpu: &mut CPU, membox: &mut Memory) {
    let acc = cpu.stack_pop(membox);
    cpu.set_zn(acc);
    cpu.acc = acc;
}

#[cfg(feature = "debug")]
pub fn jsr_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    cpu.stack_push_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.byte_1 = adl;
    cpu.byte_2 = adh;
    cpu.pcl = adl;
    cpu.pch = adh;
}

#[cfg(not(feature = "debug"))]
pub fn jsr_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    cpu.stack_push_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.pcl = adl;
    cpu.pch = adh;
}

pub fn rts_imp(cpu: &mut CPU, membox: &mut Memory) {
    cpu.stack_pop_pc(membox);
    cpu.increment_pc();
}

pub fn brk_imp(cpu: &mut CPU, membox: &mut Memory) {
    cpu.increment_pc();
    cpu.stack_push_pc(membox);
    cpu.flag_s = true;
    let flags = cpu.flags_as_byte() | FLAG_S | FLAG_B;
    cpu.stack_push(membox, flags);
    cpu.flag_i = true;
    cpu.pcl = membox.read(IRQ_VECTOR);
    cpu.pch = membox.read(IRQ_VECTOR + 1);
}

pub fn rti_imp(cpu: &mut CPU, membox: &mut Memory) {
    plp_imp(cpu, membox);
    cpu.stack_pop_pc(membox);
}

#[cfg(feature = "debug")]
pub fn jmp_abs(cpu: &mut CPU, membox: &mut Memory) {
    let (adl, adh) = read_two_bytes(cpu, membox);
    let addr = combine_bytes(adl, adh);
    cpu.last_eff_addr = addr;
    cpu.pcl = adl;
    cpu.pch = adh;
}

#[cfg(not(feature = "debug"))]
pub fn jmp_abs(cpu: &mut CPU, membox: &mut Memory) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.pcl = adl;
    cpu.pch = adh;
}

#[cfg(feature = "debug")]
pub fn jmp_ind(cpu: &mut CPU, membox: &mut Memory) {
    let ial = cpu.read_pc(membox);
    cpu.byte_1 = ial;
    let iah = cpu.read_pc(membox);
    cpu.byte_2 = iah;
    let adl = membox.read(combine_bytes(ial, iah));
    let adh = membox.read(combine_bytes(ial.wrapping_add(1), iah));
    cpu.pcl = adl;
    cpu.pch = adh;
}

#[cfg(not(feature = "debug"))]
pub fn jmp_ind(cpu: &mut CPU, membox: &mut Memory) {
    let ial = cpu.read_pc(membox);
    let iah = cpu.read_pc(membox);
    let adl = membox.read(combine_bytes(ial, iah));
    let adh = membox.read(combine_bytes(ial.wrapping_add(1), iah));
    cpu.pcl = adl;
    cpu.pch = adh;
}

#[inline(always)]
#[cfg(feature = "debug")]
pub fn branch(cpu: &mut CPU, membox: &mut Memory, flag: bool) {
    let offset = cpu.read_pc(membox);
    cpu.byte_1 = offset;

    let pcl;
    let mut pch = cpu.pch;
    
    if offset > 127 {
        //subtract one from pch if pcl < 1 + !offset
        if cpu.pcl < 1 + !offset {
            pch = cpu.pch.wrapping_sub(1);
        }
    } else {
        //carry if pcl + offset > 255 -> offset > 255 - pcl
        if offset > 255 - cpu.pcl {
            pch = cpu.pch.wrapping_add(1);
        }
    }

    pcl = cpu.pcl.wrapping_add(offset);
    cpu.last_eff_addr = combine_bytes(pcl, pch);
    
    if flag {
        cpu.pcl = pcl;
        cpu.pch = pch;
    }
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
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

#[cfg(feature = "debug")]
pub fn snop_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let addr = cpu.read_pc(membox);
    let data = membox.read(addr as u16);
    cpu.byte_1 = addr;
    cpu.last_val = data;
}

#[cfg(not(feature = "debug"))]
pub fn snop_zpg(cpu: &mut CPU, membox: &mut Memory) {
    let _ = cpu.read_pc();
}

#[cfg(feature = "debug")]
pub fn snop_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.xir);
    let data = membox.read(adl as u16);
    cpu.byte_1 = bal;
    cpu.last_eff_addr = adl as u16;
    cpu.last_val = data;
}

#[cfg(not(feature = "debug"))]
pub fn snop_zpx(cpu: &mut CPU, membox: &mut Memory) {
    let _ = cpu.read_pc(membox);
}

#[cfg(feature = "debug")]
pub fn snop_abs(cpu: &mut CPU, membox: &mut Memory) {
    let abl = cpu.read_pc(membox);
    let abh = cpu.read_pc(membox);
    let addr = combine_bytes(abl, abh);
    let data = membox.read(addr);
    cpu.byte_1 = abl;
    cpu.byte_2 = abh;
    cpu.last_eff_addr = addr;
    cpu.last_val = data;
}    

#[cfg(not(feature = "debug"))]
pub fn snop_abs(cpu: &mut CPU, membox: &mut Memory) {
    let _ = cpu.read_pc();
    let _ = cpu.read_pc();
}

#[cfg(feature = "debug")]
pub fn snop_imm(cpu: &mut CPU, membox: &mut Memory) {
    let val = cpu.read_pc(membox);
    cpu.byte_1 = val;
}

#[cfg(not(feature = "debug"))]
pub fn snop_imm(cpu: &mut CPU, membox: &mut Memory) {
    let _ = cpu.read_pc();
}

#[cfg(feature = "debug")]
pub fn snop_abx(cpu: &mut CPU, membox: &mut Memory) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    let adl = bal.wrapping_add(cpu.xir);
    let c = if cpu.xir > (255 - bal) {
        1
    } else {
        0
    };
    let adh = bah.wrapping_add(c);
    let eff_addr = combine_bytes(adl, adh);
    let data = membox.read(eff_addr);
    cpu.byte_1 = bal;
    cpu.byte_2 = bah;
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = data;
}

#[cfg(not(feature = "debug"))]
pub fn snop_abx(cpu: &mut CPU, membox: &mut Memory) {
    let _ = cpu.read_pc();
    let _ = cpu.read_pc();
}

