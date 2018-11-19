use super::super::CPU;
use super::super::super::Memory;
use super::super::combine_bytes;

/* Read - Modify - Write Operations */
/* ASL, LSR *
 * DEC, ROL *
 * INC, ROR */

#[inline(always)]
fn asl(cpu: &mut CPU, data: u8) -> u8 {
    let carry = (data & 0b1000_0000) == 0b1000_0000;
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
    let old_carry = cpu.flag_c as u8;
    let next_carry = (cpu.acc & 0b0000_0001) == 0b0000_0001;
    let res = (data >> 1) | (old_carry << 7);
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

/* ASL zpg  LSR zpg  DEC zpg  INC zpg */

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

/* ASL abs  LSR abs  DEC abs  INC abs */

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

/* ASL zpx  LSR zpx  DEC zpx  INC zpx */

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

/* ASL abx  LSR abx  DEC abx  INC abx */

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
