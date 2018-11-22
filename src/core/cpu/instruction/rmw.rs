use super::super::CPU;
use super::super::super::Memory;
use super::super::combine_bytes;
use super::internal::logical;
use super::internal::adc_internal;

/* Read - Modify - Write Operations */
/* ASL, LSR *
 * DEC, ROL *
 * INC, ROR */

#[inline(always)]
fn asl(cpu: &mut CPU, data: u8) -> u8 {
    let carry = data > 127;
    let res = data << 1;
    cpu.set_czn(res, carry);
    res
}

#[inline(always)]
fn lsr(cpu: &mut CPU, data: u8) -> u8 {
    let carry = (data & 0b000_0001) == 0b0000_0001;
    let res = data >> 1;
    cpu.set_czn(res, carry);
    res
}

#[inline(always)]
fn rol(cpu: &mut CPU, data: u8) -> u8 {
    let old_carry = cpu.flag_c as u8;
    let next_carry = data > 127;
    let res = (data << 1) | old_carry;
    cpu.set_czn(res, next_carry);
    res
}

#[inline(always)]
fn ror(cpu: &mut CPU, data: u8) -> u8 {
    let old_carry = if cpu.flag_c {
        0b1000_0000
    } else {
        0
    };
    let next_carry = (data & 0b0000_0001) == 0b0000_0001;
    let res = (data >> 1) | old_carry;
    cpu.set_czn(res, next_carry);
    res
}

#[inline(always)]
fn dec(cpu: &mut CPU, data: u8) -> u8 {
    let res = data.wrapping_sub(1);
    cpu.set_zn(res);
    res
}

#[inline(always)]
fn inc(cpu: &mut CPU, data: u8) -> u8 {
    let res = data.wrapping_add(1);
    cpu.set_zn(res);
    res
}

#[inline(always)]
fn dcm(cpu: &mut CPU, data: u8) -> u8 {
    use super::internal::compare;
    let lhs = cpu.acc;
    let res = data.wrapping_sub(1);
    compare(cpu, lhs, res);
    res
}

#[inline(always)]
fn ins(cpu: &mut CPU, data: u8) -> u8 {
    let res = data.wrapping_add(1);
    adc_internal(cpu, !res);
    res
}

#[inline(always)]
fn aso(cpu: &mut CPU, data: u8) -> u8 {
    let res = asl(cpu, data);
    logical(cpu, res, &|acc, rhs| { acc | rhs });
    res
}

#[inline(always)]
fn rla(cpu: &mut CPU, data: u8) -> u8 {
    let res = rol(cpu, data);
    logical(cpu, res, &|acc, rhs| { acc & rhs });
    res
}

#[inline(always)]
fn lse(cpu: &mut CPU, data: u8) -> u8 {
    let res = lsr(cpu, data);
    logical(cpu, res, &|acc, rhs| { acc ^ rhs });
    res
}

#[inline(always)]
fn rra(cpu: &mut CPU, data: u8) -> u8 {
    let res = ror(cpu, data);
    adc_internal(cpu, res);
    res
}

/* ASL zpg  LSR zpg  DEC zpg  INC zpg */

#[cfg(feature = "debug")]
#[inline(always)]
fn rmw_zpg(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let adl = cpu.read_pc(membox);
    cpu.byte_1 = adl;
    let data = membox.cpu_ram[adl as usize];
    cpu.last_eff_addr = adl as u16;
    cpu.last_val = data;
    membox.cpu_ram[adl as usize] = op(cpu, data);
}

#[cfg(not(feature = "debug"))]
#[inline(always)]
fn rmw_zpg(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let adl = cpu.read_pc(membox) as usize;
    let data = membox.cpu_ram[adl];
    membox.cpu_ram[adl] = op(cpu, data);
}

#[inline(always)]
pub fn asl_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &asl);
}

#[inline(always)]
pub fn lsr_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &lsr);
}

#[inline(always)]
pub fn rol_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &rol);
}

#[inline(always)]
pub fn ror_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &ror);
}

#[inline(always)]
pub fn dec_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &dec);
}

#[inline(always)]
pub fn inc_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &inc);
}

#[inline(always)]
pub fn dcm_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_zpg(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpg(cpu, membox, &rra);
}

/* ASL abs  LSR abs  DEC abs  INC abs */

#[cfg(feature = "debug")]
#[inline(always)]
fn rmw_abs(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    cpu.byte_1 = adl;
    cpu.byte_2 = adh;
    let addr = combine_bytes(adl, adh);
    let data = membox.read(addr);
    cpu.last_eff_addr = addr;
    cpu.last_val = data;
    //cpu.msg = format!("${:04X} = #${:02X}", addr as u16, data);
    membox.write(addr, op(cpu, data));
}

#[cfg(not(feature = "debug"))]
#[inline(always)]
fn rmw_abs(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let adl = cpu.read_pc(membox);
    let adh = cpu.read_pc(membox);
    let addr = combine_bytes(adl, adh);
    let data = membox.read(addr);
    membox.write(addr, op(cpu, data));
}

#[inline(always)]
pub fn asl_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &asl);
}

#[inline(always)]
pub fn lsr_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &lsr);
}

#[inline(always)]
pub fn rol_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &rol);
}

#[inline(always)]
pub fn ror_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &ror);
}

#[inline(always)]
pub fn dec_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &dec);
}

#[inline(always)]
pub fn inc_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &inc);
}

#[inline(always)]
pub fn dcm_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_abs(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abs(cpu, membox, &rra);
}

/* ASL zpx  LSR zpx  DEC zpx  INC zpx */

#[cfg(feature = "debug")]
#[inline(always)]
fn rmw_zpx(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    cpu.byte_1 = bal;
    let eff_bal = bal.wrapping_add(cpu.xir) as usize;
    let data = membox.cpu_ram[eff_bal];
    cpu.last_eff_addr = eff_bal as u16;
    cpu.last_val = data;
    membox.cpu_ram[eff_bal] = op(cpu, data);
}

#[cfg(not(feature = "debug"))]
#[inline(always)]
fn rmw_zpx(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    let eff_bal = bal.wrapping_add(cpu.xir) as usize;
    let adl = membox.cpu_ram[eff_bal] as usize;
    let data = membox.cpu_ram[adl];
    membox.cpu_ram[adl] = op(cpu, data);
}

#[inline(always)]
pub fn asl_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &asl);
}

#[inline(always)]
pub fn lsr_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &lsr);
}

#[inline(always)]
pub fn rol_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &rol);
}

#[inline(always)]
pub fn ror_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &ror);
}

#[inline(always)]
pub fn dec_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &dec);
}

#[inline(always)]
pub fn inc_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &inc);
}

#[inline(always)]
pub fn dcm_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_zpx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_zpx(cpu, membox, &rra);
}

/* ASL abx  LSR abx  DEC abx  INC abx */

#[cfg(feature = "debug")]
#[inline(always)]
fn rmw_abx(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    cpu.byte_1 = bal;
    cpu.byte_2 = bah;
    let eff_addr = combine_bytes(bal, bah) + (cpu.xir as u16);
    let data = membox.read(eff_addr);
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = data;
    //cpu.msg = format!("${:04X} = #${:02X}", eff_addr as u16, data);
    membox.write(eff_addr, op(cpu, data));
}

#[cfg(not(feature = "debug"))]
#[inline(always)]
fn rmw_abx(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.xir as u16);
    let data = membox.read(eff_addr);
    membox.write(eff_addr, op(cpu, data));
}

#[inline(always)]
pub fn asl_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &asl);
}

#[inline(always)]
pub fn lsr_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &lsr);
}

#[inline(always)]
pub fn rol_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &rol);
}

#[inline(always)]
pub fn ror_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &ror);
}

#[inline(always)]
pub fn dec_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &dec);
}

#[inline(always)]
pub fn inc_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &inc);
}

#[inline(always)]
pub fn dcm_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_abx(cpu: &mut CPU, membox: &mut Memory) {
    rmw_abx(cpu, membox, &rra);
}

#[cfg(feature = "debug")]
#[inline(always)]
fn rmw_aby(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    cpu.byte_1 = bal;
    cpu.byte_2 = bah;
    let eff_addr = combine_bytes(bal, bah) + (cpu.yir as u16);
    let data = membox.read(eff_addr);
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = data;
    //cpu.msg = format!("${:04X} = #${:02X}", eff_addr as u16, data);
    membox.write(eff_addr, op(cpu, data));
}

#[cfg(not(feature = "debug"))]
#[inline(always)]
fn rmw_aby(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    let bal = cpu.read_pc(membox);
    let bah = cpu.read_pc(membox);
    let eff_addr = combine_bytes(bal, bah) + (cpu.yir as u16);
    let data = membox.read(eff_addr);
    membox.write(eff_addr, op(cpu, data));
}

#[inline(always)]
pub fn dcm_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_aby(cpu: &mut CPU, membox: &mut Memory) {
    rmw_aby(cpu, membox, &rra);
}

#[inline(always)]
#[cfg(feature = "debug")]
pub fn rmw_ixi(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    use super::read_one_byte;
    let mut base_addr = read_one_byte(cpu, membox);
    base_addr = base_addr.wrapping_add(cpu.xir);
    let adl = membox.cpu_ram[base_addr as usize];
    let adh = membox.cpu_ram[base_addr.wrapping_add(1) as usize];
    let eff_addr = combine_bytes(adl, adh);
    let data = membox.read(eff_addr);
    membox.write(eff_addr, op(cpu, data));    
    cpu.byte_1 = base_addr;
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = data;
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
pub fn rmw_ixi(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    use super::read_one_byte;
    let mut base_addr = read_one_byte(cpu, membox);
    base_addr = base_addr.wrapping_add(cpu.xir);
    let adl = membox.cpu_ram[base_addr as usize];
    let adh = membox.cpu_ram[base_addr.wrapping_add(1) as usize];
    let eff_addr = combine_bytes(adl, adh);
    let data = membox.read(eff_addr);
    membox.write(eff_addr, op(cpu, data));    
}

#[inline(always)]
pub fn dcm_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_ixi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_ixi(cpu, membox, &rra);
}

#[inline(always)]
#[cfg(feature = "debug")]
pub fn rmw_iyi(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    use super::read_one_byte;
    let mut intermediate_addr = read_one_byte(cpu, membox);
    let bal = membox.cpu_ram[intermediate_addr as usize];
    intermediate_addr = intermediate_addr.wrapping_add(1);
    let bah = membox.cpu_ram[intermediate_addr as usize];
    let adl = bal.wrapping_add(cpu.yir);
    let c = if cpu.yir > (255 - bal) {
        1
    } else {
        0
    };
    let adh = bah.wrapping_add(c);
    let eff_addr = combine_bytes(adl, adh);
    let data = membox.read(eff_addr);
    membox.write(eff_addr, op(cpu, data));
    cpu.byte_1 = intermediate_addr;
    cpu.last_eff_addr = eff_addr;
    cpu.last_val = data;
}

#[inline(always)]
#[cfg(not(feature = "debug"))]
pub fn rmw_iyi(cpu: &mut CPU, membox: &mut Memory, op: &Fn(&mut CPU, u8) -> u8) {
    use super::read_one_byte;
    let mut intermediate_addr = read_one_byte(cpu, membox);
    let bal = membox.cpu_ram[intermediate_addr as usize];
    intermediate_addr = intermediate_addr.wrapping_add(1);
    let bah = membox.cpu_ram[intermediate_addr as usize];
    let adl = bal.wrapping_add(cpu.yir);
    let c = if cpu.yir > (255 - bal) {
        1
    } else {
        0
    };
    let adh = bah.wrapping_add(c);
    let eff_addr = combine_bytes(adl, adh);
    let data = membox.read(eff_addr);
}

#[inline(always)]
pub fn dcm_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &dcm);
}

#[inline(always)]
pub fn ins_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &ins);
}

#[inline(always)]
pub fn aso_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &aso);
}

#[inline(always)]
pub fn rla_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &rla);
}

#[inline(always)]
pub fn lse_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &lse);
}

#[inline(always)]
pub fn rra_iyi(cpu: &mut CPU, membox: &mut Memory) {
    rmw_iyi(cpu, membox, &rra);
}
