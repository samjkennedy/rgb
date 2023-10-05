#![allow(non_snake_case)]
#![allow(non_camel_case_types)]

use crate::mmu::{get_bit, MMU, RAM_START};

pub struct CPU {
    //debug
    pub enable_logging: bool,
    //state
    halted: bool,
    ime: bool,
    remaining_cycles: u8, //horrible name, probably shouldn't exist
    clock_cycles: u64,
    //registers
    AF: u16,
    BC: u16,
    DE: u16,
    HL: u16,
    SP: u16,
    PC: u16,
}

pub enum Flag {
    Z,
    N,
    H,
    C,
}

pub enum WideRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
}

pub enum Register {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Interrupt {
    VBlank,
    LCD_STAT,
    Timer,
    Serial,
    Joypad,
}

fn half_carry(a: u8, b: u8) -> bool {
    return ((a & 0xf) - (b & 0xf)) & 0x10 == 0x10;
}

fn half_carry_16(a: u16, b: u16) -> bool {
    return ((a & 0xfff) + (b & 0xfff)) > 0xfff;
}

impl CPU {
    pub fn new() -> CPU {
        return CPU {
            enable_logging: false,
            halted: false,
            ime: false,
            remaining_cycles: 0,
            clock_cycles: 0,
            AF: 0x01B0,
            BC: 0x0013,
            DE: 0x00D8,
            HL: 0x014D,
            SP: 0xFFFE,
            PC: 0x0100,
        };
    }

    fn fetch_byte(&mut self, mmu: &MMU) -> u8 {
        let byte = mmu.read(self.PC);

        self.PC += 1;

        return byte;
    }

    fn fetch_word(&mut self, mmu: &MMU) -> u16 {
        let lo: u16 = self.fetch_byte(mmu) as u16;
        let hi: u16 = self.fetch_byte(mmu) as u16;

        return (hi << 8) | lo;
    }

    fn handle_interrupt(&mut self, interrupt: Interrupt, mmu: &mut MMU) -> u8 {
        if !self.ime {
            self.halted = false;
            return 0;
        }
        let interrupt_set = mmu.get_interrupt_flag(interrupt);
        if interrupt_set && mmu.interrupt_enabled(interrupt) {
            self.halted = false;
            if self.enable_logging {
                println!("Handling {:?}", interrupt);
            }

            //The CPU automatically disables all the other interrupts by setting IME=0 when it services an interrupt.
            self.ime = false;
            mmu.set_interrupt_flag(interrupt, false);

            self.SP -= 2;
            mmu.write(self.SP, (self.PC & 0x00FF) as u8);
            mmu.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);

            match interrupt {
                Interrupt::VBlank => self.PC = 0x0040,
                Interrupt::LCD_STAT => self.PC = 0x0048,
                Interrupt::Timer => self.PC = 0x0050,
                Interrupt::Serial => self.PC = 0x0058,
                Interrupt::Joypad => self.PC = 0x0060,
            }
            return 20;
        }
        return 0;
    }

    pub fn tick(&mut self, mmu: &mut MMU, ticks: u64) {
        self.clock_cycles += 1;

        //Timer
        let tac = mmu.read(0xFF07);
        let timer_enable = get_bit(tac, 2) > 0;
        let input_clock_select = (get_bit(tac, 0) << 1) | get_bit(tac, 0);

        let div_frequency: u64 = 16384; //different on SGB TODO
        let timer_frequency: u64 = match input_clock_select {
            0b00 => 4096,
            0b01 => 262144,
            0b10 => 65536,
            0b11 => 16384,
            _ => panic!("unexpected value in TAC"),
        };

        //update DIV register
        if ticks % div_frequency == 0 {
            mmu.ram[(0xFF04 - RAM_START) as usize] += 1;
        }

        //update TIMA register
        if timer_enable && ticks % timer_frequency == 0 {
            let tima = mmu.read(0xFF05);
            //This timer is incremented at the clock frequency specified by the TAC register ($FF07).
            //When the value overflows (exceeds $FF) it is reset to the value specified in TMA (FF06) and an interrupt is requested
            if tima == 0xFF {
                mmu.set_interrupt_flag(Interrupt::Timer, true);
                let tma = mmu.read(0xFF06);
                mmu.write(0xFF05, tma);
            } else {
                mmu.write(0xFF05, tima + 1);
            }
        }

        //Interrupts
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::VBlank, mmu);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::LCD_STAT, mmu);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Timer, mmu);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Serial, mmu);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Joypad, mmu);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.next_op(mmu);
        }

        self.remaining_cycles -= 1;
    }

    fn next_op(&mut self, mmu: &mut MMU) -> u8 {
        if !self.halted {
            let opcode = self.fetch_byte(mmu);

            // if self.enable_logging {
            //     self.PC -= 1; //just to make the logging better
            //     self.log_state(opcode);
            //     self.PC += 1;
            // }

            return match opcode {
                //0x00
                0x00 => self.nop(),
                0x01 => self.ld_imm_wide(WideRegister::BC, mmu),
                0x02 => self.ld_ind_wide(WideRegister::BC, mmu),
                0x03 => self.inc_wide(WideRegister::BC),
                0x04 => self.inc(Register::B),
                0x05 => self.dec(Register::B),
                0x06 => self.ld_imm(Register::B, mmu),
                0x07 => self.rlca(),
                0x08 => self.ld_sp(mmu),
                0x09 => self.add_hl_wide(WideRegister::BC),
                0x0A => self.ld_a_ind(WideRegister::BC, mmu),
                0x0B => self.dec_wide(WideRegister::BC),
                0x0C => self.inc(Register::C),
                0x0D => self.dec(Register::C),
                0x0E => self.ld_imm(Register::C, mmu),
                0x0F => self.rrca(),
                //0x10
                0x10 => self.stop(mmu),
                0x11 => self.ld_imm_wide(WideRegister::DE, mmu),
                0x12 => self.ld_ind_wide(WideRegister::DE, mmu),
                0x13 => self.inc_wide(WideRegister::DE),
                0x14 => self.inc(Register::D),
                0x15 => self.dec(Register::D),
                0x16 => self.ld_imm(Register::D, mmu),
                0x17 => self.rla(),
                0x18 => self.jr(mmu),
                0x19 => self.add_hl_wide(WideRegister::DE),
                0x1A => self.ld_a_ind(WideRegister::DE, mmu),
                0x1B => self.dec_wide(WideRegister::DE),
                0x1C => self.inc(Register::E),
                0x1D => self.dec(Register::E),
                0x1E => self.ld_imm(Register::E, mmu),
                0x1F => self.rra(),
                //0x20
                0x20 => self.jr_cond(Flag::Z, false, mmu),
                0x21 => self.ld_imm_wide(WideRegister::HL, mmu),
                0x22 => self.ldi_hl_a(mmu),
                0x23 => self.inc_wide(WideRegister::HL),
                0x24 => self.inc(Register::H),
                0x25 => self.dec(Register::H),
                0x26 => self.ld_imm(Register::H, mmu),
                0x27 => self.daa(),
                0x28 => self.jr_cond(Flag::Z, true, mmu),
                0x29 => self.add_hl_wide(WideRegister::HL),
                0x2A => self.ldi(Register::A, WideRegister::HL, mmu),
                0x2B => self.dec_wide(WideRegister::HL),
                0x2C => self.inc(Register::L),
                0x2D => self.dec(Register::L),
                0x2E => self.ld_imm(Register::L, mmu),
                0x2F => self.cpl(),
                //0x30
                0x30 => self.jr_cond(Flag::C, false, mmu),
                0x31 => self.ld_imm_wide(WideRegister::SP, mmu),
                0x32 => self.ldd(WideRegister::HL, Register::A, mmu),
                0x33 => self.inc_wide(WideRegister::SP),
                0x34 => self.inc_hl(mmu),
                0x35 => self.dec_hl(mmu),
                0x36 => self.ld_imm_ind(WideRegister::HL, mmu),
                0x37 => self.scf(),
                0x38 => self.jr_cond(Flag::C, true, mmu),
                0x39 => self.add_hl_wide(WideRegister::SP),
                0x3A => self.ldd_a_hl(mmu),
                0x3B => self.dec_wide(WideRegister::SP),
                0x3C => self.inc(Register::A),
                0x3D => self.dec(Register::A),
                0x3E => self.ld_imm(Register::A, mmu),
                0x3F => self.ccf(),
                //0x40
                0x40 => self.ld(Register::B, Register::B),
                0x41 => self.ld(Register::B, Register::C),
                0x42 => self.ld(Register::B, Register::D),
                0x43 => self.ld(Register::B, Register::E),
                0x44 => self.ld(Register::B, Register::H),
                0x45 => self.ld(Register::B, Register::L),
                0x46 => self.ld_hl(Register::B, mmu),
                0x47 => self.ld(Register::B, Register::A),
                0x48 => self.ld(Register::C, Register::B),
                0x49 => self.ld(Register::C, Register::C),
                0x4A => self.ld(Register::C, Register::D),
                0x4B => self.ld(Register::C, Register::E),
                0x4C => self.ld(Register::C, Register::H),
                0x4D => self.ld(Register::C, Register::L),
                0x4E => self.ld_hl(Register::C, mmu),
                0x4F => self.ld(Register::C, Register::A),
                //0x50
                0x50 => self.ld(Register::D, Register::B),
                0x51 => self.ld(Register::D, Register::C),
                0x52 => self.ld(Register::D, Register::D),
                0x53 => self.ld(Register::D, Register::E),
                0x54 => self.ld(Register::D, Register::H),
                0x55 => self.ld(Register::D, Register::L),
                0x56 => self.ld_hl(Register::D, mmu),
                0x57 => self.ld(Register::D, Register::A),
                0x58 => self.ld(Register::E, Register::B),
                0x59 => self.ld(Register::E, Register::C),
                0x5A => self.ld(Register::E, Register::D),
                0x5B => self.ld(Register::E, Register::E),
                0x5C => self.ld(Register::E, Register::H),
                0x5D => self.ld(Register::E, Register::L),
                0x5E => self.ld_hl(Register::E, mmu),
                0x5F => self.ld(Register::E, Register::A),
                //0x60
                0x60 => self.ld(Register::H, Register::B),
                0x61 => self.ld(Register::H, Register::C),
                0x62 => self.ld(Register::H, Register::D),
                0x63 => self.ld(Register::H, Register::E),
                0x64 => self.ld(Register::H, Register::H),
                0x65 => self.ld(Register::H, Register::L),
                0x66 => self.ld_hl(Register::H, mmu),
                0x67 => self.ld(Register::H, Register::A),
                0x68 => self.ld(Register::L, Register::B),
                0x69 => self.ld(Register::L, Register::C),
                0x6A => self.ld(Register::L, Register::D),
                0x6B => self.ld(Register::L, Register::E),
                0x6C => self.ld(Register::L, Register::H),
                0x6D => self.ld(Register::L, Register::L),
                0x6E => self.ld_hl(Register::L, mmu),
                0x6F => self.ld(Register::L, Register::A),
                //0x70
                0x70 => self.ld_ind(Register::B, mmu),
                0x71 => self.ld_ind(Register::C, mmu),
                0x72 => self.ld_ind(Register::D, mmu),
                0x73 => self.ld_ind(Register::E, mmu),
                0x74 => self.ld_ind(Register::H, mmu),
                0x75 => self.ld_ind(Register::L, mmu),
                0x76 => self.halt(),
                0x77 => self.ld_ind(Register::A, mmu),
                0x78 => self.ld(Register::A, Register::B),
                0x79 => self.ld(Register::A, Register::C),
                0x7A => self.ld(Register::A, Register::D),
                0x7B => self.ld(Register::A, Register::E),
                0x7C => self.ld(Register::A, Register::H),
                0x7D => self.ld(Register::A, Register::L),
                0x7E => self.ld_hl(Register::A, mmu),
                0x7F => self.ld(Register::A, Register::A),
                //0x80
                0x80 => self.add(Register::B),
                0x81 => self.add(Register::C),
                0x82 => self.add(Register::D),
                0x83 => self.add(Register::E),
                0x84 => self.add(Register::H),
                0x85 => self.add(Register::L),
                0x86 => self.add_a_hl(mmu),
                0x87 => self.add(Register::A),
                0x88 => self.adc(Register::B),
                0x89 => self.adc(Register::C),
                0x8A => self.adc(Register::D),
                0x8B => self.adc(Register::E),
                0x8C => self.adc(Register::H),
                0x8D => self.adc(Register::L),
                0x8E => self.adc_hl(mmu),
                0x8F => self.adc(Register::A),
                //0x90
                0x90 => self.sub(Register::B),
                0x91 => self.sub(Register::C),
                0x92 => self.sub(Register::D),
                0x93 => self.sub(Register::E),
                0x94 => self.sub(Register::H),
                0x95 => self.sub(Register::L),
                0x96 => self.sub_hl(mmu),
                0x97 => self.sub(Register::A),
                0x98 => self.sbc(Register::B),
                0x99 => self.sbc(Register::C),
                0x9A => self.sbc(Register::D),
                0x9B => self.sbc(Register::E),
                0x9C => self.sbc(Register::H),
                0x9D => self.sbc(Register::L),
                0x9E => self.sbc_hl(mmu),
                0x9F => self.sbc(Register::A),
                //0xA0
                0xA0 => self.and(Register::B),
                0xA1 => self.and(Register::C),
                0xA2 => self.and(Register::D),
                0xA3 => self.and(Register::E),
                0xA4 => self.and(Register::H),
                0xA5 => self.and(Register::L),
                0xA6 => self.and_hl(mmu),
                0xA7 => self.and(Register::A),
                0xA8 => self.xor(Register::B),
                0xA9 => self.xor(Register::C),
                0xAA => self.xor(Register::D),
                0xAB => self.xor(Register::E),
                0xAC => self.xor(Register::H),
                0xAD => self.xor(Register::L),
                0xAE => self.xor_hl(mmu),
                0xAF => self.xor(Register::A),
                //0xB0
                0xB0 => self.or(Register::B),
                0xB1 => self.or(Register::C),
                0xB2 => self.or(Register::D),
                0xB3 => self.or(Register::E),
                0xB4 => self.or(Register::H),
                0xB5 => self.or(Register::L),
                0xB6 => self.or_hl(mmu),
                0xB7 => self.or(Register::A),
                0xB8 => self.cp(Register::B),
                0xB9 => self.cp(Register::C),
                0xBA => self.cp(Register::D),
                0xBB => self.cp(Register::E),
                0xBC => self.cp(Register::H),
                0xBD => self.cp(Register::L),
                0xBE => self.cp_hl(mmu),
                0xBF => self.cp(Register::A),
                //0xC0
                0xC0 => self.ret_cond(Flag::Z, false, mmu),
                0xC1 => self.pop(WideRegister::BC, mmu),
                0xC2 => self.jp_imm(Flag::Z, false, mmu),
                0xC4 => self.call_cond(Flag::Z, false, mmu),
                0xC8 => self.ret_cond(Flag::Z, true, mmu),
                0xC9 => self.ret(mmu),
                0xC3 => self.jp(mmu),
                0xC5 => self.push(WideRegister::BC, mmu),
                0xC6 => self.add_imm(mmu),
                0xC7 => self.rst(0x00, mmu),
                0xCA => self.jp_imm(Flag::Z, true, mmu),
                0xCB => self.prefix_cb(mmu),
                0xCC => self.call_cond(Flag::Z, true, mmu),
                0xCD => self.call(mmu),
                0xCE => self.adc_a_imm(mmu),
                0xCF => self.rst(0x08, mmu),
                //0xD0
                0xD0 => self.ret_cond(Flag::C, false, mmu),
                0xD1 => self.pop(WideRegister::DE, mmu),
                0xD2 => self.jp_imm(Flag::C, false, mmu),
                0xD4 => self.call_cond(Flag::C, false, mmu),
                0xD5 => self.push(WideRegister::DE, mmu),
                0xD6 => self.sub_imm(mmu),
                0xD7 => self.rst(0x10, mmu),
                0xD8 => self.ret_cond(Flag::C, true, mmu),
                0xD9 => self.reti(mmu),
                0xDA => self.jp_imm(Flag::C, true, mmu),
                0xDC => self.call_cond(Flag::C, true, mmu),
                0xDE => self.sbc_a_imm(mmu),
                0xDF => self.rst(0x18, mmu),
                //0xE0
                0xE0 => self.ld_io_n(mmu),
                0xE1 => self.pop(WideRegister::HL, mmu),
                0xE2 => self.ld_io_c(mmu),
                0xE5 => self.push(WideRegister::HL, mmu),
                0xE6 => self.and_imm(mmu),
                0xE7 => self.rst(0x20, mmu),
                0xE8 => self.add_sp(mmu),
                0xE9 => self.jp_hl(),
                0xEA => self.ld_nn_a(mmu),
                0xEE => self.xor_imm(mmu),
                0xEF => self.rst(0x28, mmu),
                //0xF0
                0xF0 => self.rd_io_n(mmu),
                0xF1 => self.pop(WideRegister::AF, mmu),
                0xF2 => self.rd_io_c(mmu),
                0xF3 => self.di(),
                0xF5 => self.push(WideRegister::AF, mmu),
                0xF6 => self.or_imm(mmu),
                0xF7 => self.rst(0x30, mmu),
                0xF8 => self.ld_hl_sp_dd(mmu),
                0xF9 => self.ld_sp_hl(),
                0xFA => self.ld_a_nn(mmu),
                0xFB => self.ei(),
                0xFE => self.cp_imm(mmu),
                0xFF => self.rst(0x38, mmu),
                _ => {
                    //self.log_state(opcode);
                    todo!("opcode 0x{:02X?} hasn't been implemented yet!", opcode);
                }
            };
        } else {
            return 0;
        }
    }

    // fn log_state(&mut self, opcode: u8) {
    //     println!(
    //         "OP: 0x{:02X?} | AF={:04X?}, BC={:04X?}, DE={:04X?}, HL={:04X?} | SP={:04X?}, PC={:04X?} | flags: {}{}{}{} | ie: {:02X?} if: {:02X?} ime: {} | clock: {:16?} | TIMA: {:02X?}",
    //         opcode, self.AF, self.BC, self.DE, self.HL, self.SP, self.PC,
    //         if self.get_flag(Flag::Z) {'z'} else {'-'},
    //         if self.get_flag(Flag::N) {'n'} else {'-'},
    //         if self.get_flag(Flag::H) {'h'} else {'-'},
    //         if self.get_flag(Flag::C) {'c'} else {'-'},
    //         mmu.read(0xFFFF), mmu.read(0xFF0F), if self.ime {1} else {0},
    //         self.clock_cycles,
    //         mmu.read(0xFF05)
    //     );
    // }

    fn get_flag(&self, flag: Flag) -> bool {
        match flag {
            Flag::Z => (self.AF & 0b0000000010000000) > 0,
            Flag::N => (self.AF & 0b0000000001000000) > 0,
            Flag::H => (self.AF & 0b0000000000100000) > 0,
            Flag::C => (self.AF & 0b0000000000010000) > 0,
        }
    }

    fn set_flag(&mut self, flag: Flag, value: bool) {
        if value {
            match flag {
                Flag::Z => self.AF |= 0b00000000_10000000,
                Flag::N => self.AF |= 0b00000000_01000000,
                Flag::H => self.AF |= 0b00000000_00100000,
                Flag::C => self.AF |= 0b00000000_00010000,
            }
        } else {
            match flag {
                Flag::Z => self.AF &= 0b11111111_01111111,
                Flag::N => self.AF &= 0b11111111_10111111,
                Flag::H => self.AF &= 0b11111111_11011111,
                Flag::C => self.AF &= 0b11111111_11101111,
            }
        }
    }

    fn get_register(&self, register: &Register) -> u8 {
        return match register {
            Register::A => ((self.AF & 0xFF00) >> 8) as u8,
            Register::B => ((self.BC & 0xFF00) >> 8) as u8,
            Register::C => (self.BC & 0x00FF) as u8,
            Register::D => ((self.DE & 0xFF00) >> 8) as u8,
            Register::E => (self.DE & 0x00FF) as u8,
            Register::H => ((self.HL & 0xFF00) >> 8) as u8,
            Register::L => (self.HL & 0x00FF) as u8,
        };
    }

    fn set_register(&mut self, register: &Register, value: u8) {
        match register {
            Register::A => self.AF = (self.AF & 0x00FF) | ((value as u16) << 8),
            Register::B => self.BC = (self.BC & 0x00FF) | (value as u16) << 8,
            Register::C => self.BC = (self.BC & 0xFF00) | value as u16,
            Register::D => self.DE = (self.DE & 0x00FF) | (value as u16) << 8,
            Register::E => self.DE = (self.DE & 0xFF00) | value as u16,
            Register::H => self.HL = (self.HL & 0x00FF) | (value as u16) << 8,
            Register::L => self.HL = (self.HL & 0xFF00) | value as u16,
        }
    }

    fn set_wide_register(&mut self, wide_register: &WideRegister, value: u16) {
        match wide_register {
            WideRegister::AF => self.AF = value & 0xFFF0,
            WideRegister::BC => self.BC = value,
            WideRegister::DE => self.DE = value,
            WideRegister::HL => self.HL = value,
            WideRegister::SP => self.SP = value,
        }
    }

    fn get_wide_register(&self, wide_register: &WideRegister) -> u16 {
        return match wide_register {
            WideRegister::AF => self.AF,
            WideRegister::BC => self.BC,
            WideRegister::DE => self.DE,
            WideRegister::HL => self.HL,
            WideRegister::SP => self.SP,
        };
    }

    fn nop(&mut self) -> u8 {
        return 4;
    }

    fn ld_imm(&mut self, reg: Register, mmu: &MMU) -> u8 {
        let val = self.fetch_byte(mmu);

        self.set_register(&reg, val);

        return 8;
    }

    fn ld_imm_wide(&mut self, wide_reg: WideRegister, mmu: &MMU) -> u8 {
        let val = self.fetch_word(mmu);

        self.set_wide_register(&wide_reg, val);

        return 12;
    }

    fn xor(&mut self, register: Register) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);

        let val = a ^ r;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 4;
    }

    fn xor_imm(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte(mmu);

        let val = a ^ n;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 8;
    }

    fn xor_hl(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let r = mmu.read(self.HL);

        let val = a ^ r;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 8;
    }

    fn jp(&mut self, mmu: &MMU) -> u8 {
        let address = self.fetch_word(mmu);

        self.PC = address;
        return 16;
    }

    fn ldd(&mut self, wide_reg: WideRegister, reg: Register, mmu: &mut MMU) -> u8 {
        //(HL)=A, HL=HL-1

        let address = self.get_wide_register(&wide_reg);
        mmu.write(address, self.get_register(&reg));

        self.set_wide_register(&wide_reg, address - 1);

        return 8;
    }

    fn inc(&mut self, reg: Register) -> u8 {
        let a: u8 = self.get_register(&reg);

        let value = a + 1;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (1 & 0xf)) & 0x10 == 0x10);

        self.set_register(&reg, value);

        return 4;
    }

    fn inc_hl(&mut self, mmu: &mut MMU) -> u8 {
        let hl = mmu.read(self.HL);

        let value = hl + 1;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((hl & 0xf) + (1 & 0xf)) & 0x10 == 0x10);

        mmu.write(self.HL, value);

        return 12;
    }

    fn dec_hl(&mut self, mmu: &mut MMU) -> u8 {
        let hl = mmu.read(self.HL);

        let value = hl - 1;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((hl & 0xf) - (1 & 0xf)) & 0x10 == 0x10);

        mmu.write(self.HL, value);

        return 12;
    }

    fn dec(&mut self, reg: Register) -> u8 {
        let a: u8 = self.get_register(&reg);

        let val = a - 1;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, half_carry(a, 1));

        self.set_register(&reg, val);

        return 4;
    }

    fn jr(&mut self, mmu: &MMU) -> u8 {
        let offset = self.fetch_byte(mmu) as i8;

        self.PC += offset as u16;

        return 12;
    }

    fn jr_cond(&mut self, flag: Flag, jump_if_true: bool, mmu: &MMU) -> u8 {
        let offset = self.fetch_byte(mmu) as i8;

        if self.get_flag(flag) == jump_if_true {
            self.PC += offset as u16;

            return 12;
        } else {
            return 8;
        }
    }

    fn di(&mut self) -> u8 {
        self.ime = false;

        return 4;
    }

    fn ei(&mut self) -> u8 {
        self.ime = true;

        return 4;
    }

    fn ld_io_n(&mut self, mmu: &mut MMU) -> u8 {
        //ld (FF00+n),A
        //write to io-port n (memory FF00+n)

        let offset = self.fetch_byte(mmu);

        let value = self.get_register(&Register::A);

        mmu.write(0xFF00 + (offset as u16), value);

        return 12;
    }

    fn ld_io_c(&mut self, mmu: &mut MMU) -> u8 {
        //ld (FF00+C),A
        //write to io-port C (memory FF00+C)

        let a = self.get_register(&Register::A);
        let c = self.get_register(&Register::C);

        mmu.write(0xFF00 + c as u16, a);

        return 8;
    }

    fn rd_io_n(&mut self, mmu: &MMU) -> u8 {
        //ld A,(FF00+n)
        //read from io-port n (memory FF00+n)

        let offset = self.fetch_byte(mmu);
        let value = mmu.read(0xFF00 + offset as u16);

        self.set_register(&Register::A, value);

        return 12;
    }

    fn rd_io_c(&mut self, mmu: &MMU) -> u8 {
        //ld A,(FF00+c)
        //read from io-port n (memory FF00+n)

        let offset = self.get_register(&Register::C);
        let value = mmu.read(0xFF00 + offset as u16);

        self.set_register(&Register::A, value);

        return 12;
    }

    fn cp_imm(&mut self, mmu: &MMU) -> u8 {
        //cp n
        //compare A-n
        //z1hc
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte(mmu);

        let result = a - n;

        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, half_carry(a, n));
        self.set_flag(Flag::C, result > a);

        return 8;
    }

    fn ld_imm_ind(&mut self, reg: WideRegister, mmu: &mut MMU) -> u8 {
        let value = self.fetch_byte(mmu);

        mmu.write(self.get_wide_register(&reg), value);

        return 12;
    }

    fn ld_nn_a(&mut self, mmu: &mut MMU) -> u8 {
        //ld (nn),A
        //(nn)=A
        let address = self.fetch_word(mmu);
        let value = self.get_register(&Register::A);

        mmu.write(address, value);

        return 16;
    }

    fn ld_a_nn(&mut self, mmu: &MMU) -> u8 {
        //A=(nn)

        let address = self.fetch_word(mmu);
        let value = mmu.read(address);

        self.set_register(&Register::A, value);

        return 16;
    }

    fn ldi(&mut self, register: Register, wide_register: WideRegister, mmu: &MMU) -> u8 {
        //ldi A,(HL)
        //A=(HL), HL=HL+1

        let hl = self.get_wide_register(&wide_register);
        let value = mmu.read(hl);
        self.set_register(&register, value);

        self.set_wide_register(&wide_register, hl + 1);

        return 8;
    }

    fn call(&mut self, mmu: &mut MMU) -> u8 {
        //call to nn, SP=SP-2, (SP)=PC, PC=nn
        let address = self.fetch_word(mmu);

        self.SP -= 2;
        mmu.write(self.SP, (self.PC & 0x00FF) as u8);
        mmu.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);
        self.PC = address;

        return 24;
    }

    fn call_cond(&mut self, flag: Flag, jump_if_true: bool, mmu: &mut MMU) -> u8 {
        let address = self.fetch_word(mmu);

        if self.get_flag(flag) == jump_if_true {
            self.SP -= 2;
            mmu.write(self.SP, (self.PC & 0x00FF) as u8);
            mmu.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);
            self.PC = address;
            return 24;
        } else {
            return 12;
        }
    }

    fn rst(&mut self, address: u16, mmu: &mut MMU) -> u8 {
        self.SP -= 2;
        mmu.write(self.SP, (self.PC & 0x00FF) as u8);
        mmu.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);

        self.PC = address;

        return 16;
    }

    fn dec_wide(&mut self, wide_register: WideRegister) -> u8 {
        let value = self.get_wide_register(&wide_register);
        self.set_wide_register(&wide_register, value - 1);

        return 8;
    }

    fn ld(&mut self, r: Register, n: Register) -> u8 {
        //r=n
        self.set_register(&r, self.get_register(&n));

        return 4;
    }

    fn or(&mut self, register: Register) -> u8 {
        //A=A | r

        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);
        let value = a | r;

        self.set_register(&Register::A, value);

        //Z 0 0 0
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        return 4;
    }

    fn or_hl(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let r = mmu.read(self.HL);

        let val = a | r;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 8;
    }

    fn cp(&mut self, register: Register) -> u8 {
        //compare A-r
        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);
        let value = a - r;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (r & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 4;
    }

    fn cp_hl(&mut self, mmu: &MMU) -> u8 {
        //compare A-(HL)
        let a = self.get_register(&Register::A);
        let r = mmu.read(self.HL);
        let value = a - r;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (r & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 8;
    }

    fn and(&mut self, register: Register) -> u8 {
        //A=A & r
        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);
        let value = a & r;

        self.set_register(&Register::A, value);

        //Z 0 1 0
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);
        self.set_flag(Flag::C, false);

        return 4;
    }

    fn and_hl(&mut self, mmu: &MMU) -> u8 {
        //A=A & r
        let a = self.get_register(&Register::A);
        let r = mmu.read(self.HL);
        let value = a & r;

        self.set_register(&Register::A, value);

        //Z 0 1 0
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);
        self.set_flag(Flag::C, false);

        return 8;
    }

    fn add(&mut self, register: Register) -> u8 {
        //A=A + r
        let a = self.get_register(&Register::A);

        let r = self.get_register(&register);

        let value = a + r;

        self.set_register(&Register::A, value);

        //Z 0 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (r & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value < a);

        return 4;
    }

    fn add_a_hl(&mut self, mmu: &MMU) -> u8 {
        //A=A+(HL)

        let a = self.get_register(&Register::A);
        let hl = mmu.read(self.HL);

        let value = a + hl;

        self.set_register(&Register::A, value);

        //Z 0 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (hl & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value < a);

        return 8;
    }

    fn ret(&mut self, mmu: &MMU) -> u8 {
        //return, PC=(SP), SP=SP+2

        let pc_lo = mmu.read(self.SP) as u16;
        let pc_hi = mmu.read(self.SP + 1) as u16;

        self.PC = (pc_hi << 8) | pc_lo;
        self.SP += 2;

        return 16;
    }

    fn reti(&mut self, mmu: &MMU) -> u8 {
        //return and enable interrupts
        self.ime = true;

        let pc_lo = mmu.read(self.SP) as u16;
        let pc_hi = mmu.read(self.SP + 1) as u16;

        self.PC = (pc_hi << 8) | pc_lo;
        self.SP += 2;

        return 16;
    }

    fn cpl(&mut self) -> u8 {
        //A = A xor FF

        let a = self.get_register(&Register::A);
        self.set_register(&Register::A, a ^ 0xFF);

        //-11-
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, true);

        return 4;
    }

    fn and_imm(&mut self, mmu: &MMU) -> u8 {
        //A=A & n
        let n = self.fetch_byte(mmu);

        let a = self.get_register(&Register::A);

        let value = a & n;

        self.set_register(&Register::A, value);

        //z010
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);
        self.set_flag(Flag::C, false);

        return 8;
    }

    fn or_imm(&mut self, mmu: &MMU) -> u8 {
        //A=A | n
        let n = self.fetch_byte(mmu);

        let a = self.get_register(&Register::A);

        let value = a | n;

        self.set_register(&Register::A, value);

        //z010
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        return 8;
    }

    fn prefix_cb(&mut self, mmu: &mut MMU) -> u8 {
        let n = self.fetch_byte(mmu);

        return match n {
            //0x00
            0x00 => self.rlc(Register::B),
            0x01 => self.rlc(Register::C),
            0x02 => self.rlc(Register::D),
            0x03 => self.rlc(Register::E),
            0x04 => self.rlc(Register::H),
            0x05 => self.rlc(Register::L),
            0x06 => self.rlc_hl(mmu),
            0x07 => self.rlc(Register::A),
            0x08 => self.rrc(Register::B),
            0x09 => self.rrc(Register::C),
            0x0A => self.rrc(Register::D),
            0x0B => self.rrc(Register::E),
            0x0C => self.rrc(Register::H),
            0x0D => self.rrc(Register::L),
            0x0E => self.rrc_hl(mmu),
            0x0F => self.rrc(Register::A),
            //0x10
            0x10 => self.rl(Register::B),
            0x11 => self.rl(Register::C),
            0x12 => self.rl(Register::D),
            0x13 => self.rl(Register::E),
            0x14 => self.rl(Register::H),
            0x15 => self.rl(Register::L),
            0x16 => self.rl_hl(mmu),
            0x17 => self.rl(Register::A),
            0x18 => self.rr(Register::B),
            0x19 => self.rr(Register::C),
            0x1A => self.rr(Register::D),
            0x1B => self.rr(Register::E),
            0x1C => self.rr(Register::H),
            0x1D => self.rr(Register::L),
            0x1E => self.rr_hl(mmu),
            0x1F => self.rr(Register::A),
            //0x20
            0x20 => self.sla(Register::B),
            0x21 => self.sla(Register::C),
            0x22 => self.sla(Register::D),
            0x23 => self.sla(Register::E),
            0x24 => self.sla(Register::H),
            0x25 => self.sla(Register::L),
            0x26 => self.sla_hl(mmu),
            0x27 => self.sla(Register::A),
            0x28 => self.sra(Register::B),
            0x29 => self.sra(Register::C),
            0x2A => self.sra(Register::D),
            0x2B => self.sra(Register::E),
            0x2C => self.sra(Register::H),
            0x2D => self.sra(Register::L),
            0x2E => self.sra_hl(mmu),
            0x2F => self.sra(Register::A),
            //0x30
            0x30 => self.swap(Register::B),
            0x31 => self.swap(Register::C),
            0x32 => self.swap(Register::D),
            0x33 => self.swap(Register::E),
            0x34 => self.swap(Register::H),
            0x35 => self.swap(Register::L),
            0x36 => self.swap_hl(mmu),
            0x37 => self.swap(Register::A),
            0x38 => self.srl(Register::B),
            0x39 => self.srl(Register::C),
            0x3A => self.srl(Register::D),
            0x3B => self.srl(Register::E),
            0x3C => self.srl(Register::H),
            0x3D => self.srl(Register::L),
            0x3E => self.srl_hl(mmu),
            0x3F => self.srl(Register::A),
            //0x40
            0x40 => self.bit(0, Register::B),
            0x41 => self.bit(0, Register::C),
            0x42 => self.bit(0, Register::D),
            0x43 => self.bit(0, Register::E),
            0x44 => self.bit(0, Register::H),
            0x45 => self.bit(0, Register::L),
            0x46 => self.bit_hl(0, mmu),
            0x47 => self.bit(0, Register::A),
            0x48 => self.bit(1, Register::B),
            0x49 => self.bit(1, Register::C),
            0x4A => self.bit(1, Register::D),
            0x4B => self.bit(1, Register::E),
            0x4C => self.bit(1, Register::H),
            0x4D => self.bit(1, Register::L),
            0x4E => self.bit_hl(1, mmu),
            0x4F => self.bit(1, Register::A),
            //0x50
            0x50 => self.bit(2, Register::B),
            0x51 => self.bit(2, Register::C),
            0x52 => self.bit(2, Register::D),
            0x53 => self.bit(2, Register::E),
            0x54 => self.bit(2, Register::H),
            0x55 => self.bit(2, Register::L),
            0x56 => self.bit_hl(2, mmu),
            0x57 => self.bit(2, Register::A),
            0x58 => self.bit(3, Register::B),
            0x59 => self.bit(3, Register::C),
            0x5A => self.bit(3, Register::D),
            0x5B => self.bit(3, Register::E),
            0x5C => self.bit(3, Register::H),
            0x5D => self.bit(3, Register::L),
            0x5E => self.bit_hl(3, mmu),
            0x5F => self.bit(3, Register::A),
            //0x60
            0x60 => self.bit(4, Register::B),
            0x61 => self.bit(4, Register::C),
            0x62 => self.bit(4, Register::D),
            0x63 => self.bit(4, Register::E),
            0x64 => self.bit(4, Register::H),
            0x65 => self.bit(4, Register::L),
            0x66 => self.bit_hl(4, mmu),
            0x67 => self.bit(4, Register::A),
            0x68 => self.bit(5, Register::B),
            0x69 => self.bit(5, Register::C),
            0x6A => self.bit(5, Register::D),
            0x6B => self.bit(5, Register::E),
            0x6C => self.bit(5, Register::H),
            0x6D => self.bit(5, Register::L),
            0x6E => self.bit_hl(5, mmu),
            0x6F => self.bit(5, Register::A),
            //0x70
            0x70 => self.bit(6, Register::B),
            0x71 => self.bit(6, Register::C),
            0x72 => self.bit(6, Register::D),
            0x73 => self.bit(6, Register::E),
            0x74 => self.bit(6, Register::H),
            0x75 => self.bit(6, Register::L),
            0x76 => self.bit_hl(6, mmu),
            0x77 => self.bit(6, Register::A),
            0x78 => self.bit(7, Register::B),
            0x79 => self.bit(7, Register::C),
            0x7A => self.bit(7, Register::D),
            0x7B => self.bit(7, Register::E),
            0x7C => self.bit(7, Register::H),
            0x7D => self.bit(7, Register::L),
            0x7E => self.bit_hl(7, mmu),
            0x7F => self.bit(7, Register::A),
            //0x80
            0x80 => self.res(0, Register::B),
            0x81 => self.res(0, Register::C),
            0x82 => self.res(0, Register::D),
            0x83 => self.res(0, Register::E),
            0x84 => self.res(0, Register::H),
            0x85 => self.res(0, Register::L),
            0x86 => self.res_hl(0, mmu),
            0x87 => self.res(0, Register::A),
            0x88 => self.res(1, Register::B),
            0x89 => self.res(1, Register::C),
            0x8A => self.res(1, Register::D),
            0x8B => self.res(1, Register::E),
            0x8C => self.res(1, Register::H),
            0x8D => self.res(1, Register::L),
            0x8E => self.res_hl(1, mmu),
            0x8F => self.res(1, Register::A),
            //0x90
            0x90 => self.res(2, Register::B),
            0x91 => self.res(2, Register::C),
            0x92 => self.res(2, Register::D),
            0x93 => self.res(2, Register::E),
            0x94 => self.res(2, Register::H),
            0x95 => self.res(2, Register::L),
            0x96 => self.res_hl(2, mmu),
            0x97 => self.res(2, Register::A),
            0x98 => self.res(3, Register::B),
            0x99 => self.res(3, Register::C),
            0x9A => self.res(3, Register::D),
            0x9B => self.res(3, Register::E),
            0x9C => self.res(3, Register::H),
            0x9D => self.res(3, Register::L),
            0x9E => self.res_hl(3, mmu),
            0x9F => self.res(3, Register::A),
            //0xA0
            0xA0 => self.res(4, Register::B),
            0xA1 => self.res(4, Register::C),
            0xA2 => self.res(4, Register::D),
            0xA3 => self.res(4, Register::E),
            0xA4 => self.res(4, Register::H),
            0xA5 => self.res(4, Register::L),
            0xA6 => self.res_hl(4, mmu),
            0xA7 => self.res(4, Register::A),
            0xA8 => self.res(5, Register::B),
            0xA9 => self.res(5, Register::C),
            0xAA => self.res(5, Register::D),
            0xAB => self.res(5, Register::E),
            0xAC => self.res(5, Register::H),
            0xAD => self.res(5, Register::L),
            0xAE => self.res_hl(5, mmu),
            0xAF => self.res(5, Register::A),
            //0xB0
            0xB0 => self.res(6, Register::B),
            0xB1 => self.res(6, Register::C),
            0xB2 => self.res(6, Register::D),
            0xB3 => self.res(6, Register::E),
            0xB4 => self.res(6, Register::H),
            0xB5 => self.res(6, Register::L),
            0xB6 => self.res_hl(6, mmu),
            0xB7 => self.res(6, Register::A),
            0xB8 => self.res(7, Register::B),
            0xB9 => self.res(7, Register::C),
            0xBA => self.res(7, Register::D),
            0xBB => self.res(7, Register::E),
            0xBC => self.res(7, Register::H),
            0xBD => self.res(7, Register::L),
            0xBE => self.res_hl(7, mmu),
            0xBF => self.res(7, Register::A),
            //0xC0
            0xC0 => self.set(0, Register::B),
            0xC1 => self.set(0, Register::C),
            0xC2 => self.set(0, Register::D),
            0xC3 => self.set(0, Register::E),
            0xC4 => self.set(0, Register::H),
            0xC5 => self.set(0, Register::L),
            0xC6 => self.set_hl(0, mmu),
            0xC7 => self.set(0, Register::A),
            0xC8 => self.set(1, Register::B),
            0xC9 => self.set(1, Register::C),
            0xCA => self.set(1, Register::D),
            0xCB => self.set(1, Register::E),
            0xCC => self.set(1, Register::H),
            0xCD => self.set(1, Register::L),
            0xCE => self.set_hl(1, mmu),
            0xCF => self.set(1, Register::A),
            //0xD0
            0xD0 => self.set(2, Register::B),
            0xD1 => self.set(2, Register::C),
            0xD2 => self.set(2, Register::D),
            0xD3 => self.set(2, Register::E),
            0xD4 => self.set(2, Register::H),
            0xD5 => self.set(2, Register::L),
            0xD6 => self.set_hl(2, mmu),
            0xD7 => self.set(2, Register::A),
            0xD8 => self.set(3, Register::B),
            0xD9 => self.set(3, Register::C),
            0xDA => self.set(3, Register::D),
            0xDB => self.set(3, Register::E),
            0xDC => self.set(3, Register::H),
            0xDD => self.set(3, Register::L),
            0xDE => self.set_hl(3, mmu),
            0xDF => self.set(3, Register::A),
            //0xE0
            0xE0 => self.set(4, Register::B),
            0xE1 => self.set(4, Register::C),
            0xE2 => self.set(4, Register::D),
            0xE3 => self.set(4, Register::E),
            0xE4 => self.set(4, Register::H),
            0xE5 => self.set(4, Register::L),
            0xE6 => self.set_hl(4, mmu),
            0xE7 => self.set(4, Register::A),
            0xE8 => self.set(5, Register::B),
            0xE9 => self.set(5, Register::C),
            0xEA => self.set(5, Register::D),
            0xEB => self.set(5, Register::E),
            0xEC => self.set(5, Register::H),
            0xED => self.set(5, Register::L),
            0xEE => self.set_hl(5, mmu),
            0xEF => self.set(5, Register::A),
            //0xF0
            0xF0 => self.set(6, Register::B),
            0xF1 => self.set(6, Register::C),
            0xF2 => self.set(6, Register::D),
            0xF3 => self.set(6, Register::E),
            0xF4 => self.set(6, Register::H),
            0xF5 => self.set(6, Register::L),
            0xF6 => self.set_hl(6, mmu),
            0xF7 => self.set(6, Register::A),
            0xF8 => self.set(7, Register::B),
            0xF9 => self.set(7, Register::C),
            0xFA => self.set(7, Register::D),
            0xFB => self.set(7, Register::E),
            0xFC => self.set(7, Register::H),
            0xFD => self.set(7, Register::L),
            0xFE => self.set_hl(7, mmu),
            0xFF => self.set(7, Register::A),
        } + 4;
    }

    fn swap(&mut self, register: Register) -> u8 {
        //exchange low/hi-nibble

        let r = self.get_register(&register);

        let hi = r & 0b11110000;
        let lo = r & 0b00001111;

        let value = (lo << 4) | (hi >> 4);

        self.set_register(&register, value);

        //z000
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        return 8;
    }

    fn swap_hl(&mut self, mmu: &mut MMU) -> u8 {
        //exchange low/hi-nibble

        let r = mmu.read(self.HL);

        let hi = r & 0b11110000;
        let lo = r & 0b00001111;

        let value = (lo << 4) | (hi >> 4);

        mmu.write(self.HL, value);

        //z000
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        return 16;
    }

    fn ld_hl_sp_dd(&mut self, mmu: &MMU) -> u8 {
        //HL = SP +/- dd ; dd is 8-bit signed number
        let nn = self.fetch_byte(mmu) as i8 as u16;

        let value = self.SP + nn;

        self.HL = value;

        //00hc
        self.set_flag(Flag::Z, false);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((self.SP ^ nn ^ value) & 0x10) == 0x10);
        self.set_flag(Flag::C, ((self.SP ^ nn ^ value) & 0x100) == 0x100); //TODO: Check this!

        return 12;
    }

    fn ld_sp_hl(&mut self) -> u8 {
        self.SP = self.HL;

        return 8;
    }

    fn pop(&mut self, wide_register: WideRegister, mmu: &MMU) -> u8 {
        //pop rr
        //rr=(SP) SP=SP+2 ; rr may be BC,DE,HL,AF
        let lo = mmu.read(self.SP) as u16;
        let hi = mmu.read(self.SP + 1) as u16;
        self.SP += 2;

        let value = (hi << 8) | lo;

        self.set_wide_register(&wide_register, value);

        return 12;
    }

    fn push(&mut self, wide_register: WideRegister, mmu: &mut MMU) -> u8 {
        //SP=SP-2 (SP)=rr ; rr may be BC,DE,HL,AF

        let value = self.get_wide_register(&wide_register);

        self.SP -= 2;
        mmu.write(self.SP, (value & 0x00FF) as u8);
        mmu.write(self.SP + 1, ((value & 0xFF00) >> 8) as u8);

        return 16;
    }

    fn add_hl_wide(&mut self, rr: WideRegister) -> u8 {
        //HL = HL+rr ; rr may be BC,DE,HL,SP

        let value = self.HL + self.get_wide_register(&rr);

        //-0hc
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, half_carry_16(self.HL, self.get_wide_register(&rr)));
        self.set_flag(Flag::C, value < self.HL);

        self.HL = value;

        return 8;
    }

    fn ld_hl(&mut self, register: Register, mmu: &MMU) -> u8 {
        //r=(HL)

        let value = mmu.read(self.HL);
        self.set_register(&register, value);

        return 8;
    }

    fn inc_wide(&mut self, wide_register: WideRegister) -> u8 {
        //rr = rr+1 ; rr may be BC,DE,HL,SP

        self.set_wide_register(&wide_register, self.get_wide_register(&wide_register) + 1);

        return 8;
    }

    fn jp_hl(&mut self) -> u8 {
        self.PC = self.HL;
        return 4;
    }

    fn res(&mut self, bit: u8, register: Register) -> u8 {
        let mut a = self.get_register(&register);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        mask = !mask;

        a &= mask;

        self.set_register(&register, a);

        return 8;
    }

    fn res_hl(&mut self, bit: u8, mmu: &mut MMU) -> u8 {
        let mut a = mmu.read(self.HL);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        mask = !mask;

        a &= mask;

        mmu.write(self.HL, a);

        return 16;
    }

    fn set(&mut self, bit: u8, register: Register) -> u8 {
        let mut a = self.get_register(&register);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        a |= mask;

        self.set_register(&register, a);
        return 8;
    }

    fn set_hl(&mut self, bit: u8, mmu: &mut MMU) -> u8 {
        let mut a = mmu.read(self.HL);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        a |= mask;

        mmu.write(self.HL, a);
        return 16;
    }

    fn ld_ind_wide(&mut self, wide_register: WideRegister, mmu: &mut MMU) -> u8 {
        let value = self.get_register(&Register::A);
        let address = self.get_wide_register(&wide_register);

        mmu.write(address, value);

        return 8;
    }

    fn ld_a_ind(&mut self, wide_register: WideRegister, mmu: &MMU) -> u8 {
        //A=(DE)
        let value = mmu.read(self.get_wide_register(&wide_register));
        self.set_register(&Register::A, value);

        return 8;
    }

    fn ldi_hl_a(&mut self, mmu: &mut MMU) -> u8 {
        //(HL)=A, HL=HL+1

        mmu.write(self.HL, self.get_register(&Register::A));
        self.HL += 1;

        return 8;
    }

    fn ldd_a_hl(&mut self, mmu: &MMU) -> u8 {
        //A=(HL), HL=HL-1

        let hl = mmu.read(self.HL);
        self.set_register(&Register::A, hl);
        self.HL -= 1;

        return 8;
    }

    fn jp_imm(&mut self, flag: Flag, jump_if_true: bool, mmu: &MMU) -> u8 {
        let address = self.fetch_word(mmu);

        if self.get_flag(flag) == jump_if_true {
            self.PC = address;
            return 16;
        } else {
            return 12;
        }
    }

    fn ret_cond(&mut self, flag: Flag, return_if_true: bool, mmu: &MMU) -> u8 {
        if self.get_flag(flag) == return_if_true {
            let pc_lo = mmu.read(self.SP) as u16;
            let pc_hi = mmu.read(self.SP + 1) as u16;

            self.PC = (pc_hi << 8) | pc_lo;
            self.SP += 2;
            return 20;
        } else {
            return 8;
        }
    }

    fn ld_ind(&mut self, register: Register, mmu: &mut MMU) -> u8 {
        //(HL)=r

        mmu.write(self.HL, self.get_register(&register));

        return 8;
    }

    fn srl(&mut self, register: Register) -> u8 {
        //shift right logical (b7=0)
        let r = self.get_register(&register);

        let result: u8 = r >> 1;

        self.set_register(&register, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, (r & 1) == 1);

        return 8;
    }

    fn srl_hl(&mut self, mmu: &mut MMU) -> u8 {
        //shift right logical (b7=0)
        let r = mmu.read(self.HL);

        let result: u8 = r >> 1;

        mmu.write(self.HL, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, (r & 1) == 1);

        return 16;
    }

    fn sla(&mut self, register: Register) -> u8 {
        //shift left arithmetic (b0=0)
        let r = self.get_register(&register);
        let msb = r & 0b1000_0000;

        let result: u8 = r << 1;

        self.set_register(&register, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, msb > 1);

        return 8;
    }

    fn sla_hl(&mut self, mmu: &mut MMU) -> u8 {
        //shift left arithmetic (b0=0)
        let r = mmu.read(self.HL);
        let msb = r & 0b1000_0000;

        let result: u8 = r << 1;

        mmu.write(self.HL, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, msb > 1);

        return 16;
    }

    fn sra(&mut self, register: Register) -> u8 {
        //shift right arithmetic (b0=0)
        let r = self.get_register(&register);
        let msb = r & 0b1000_0000;
        let lsb = r & 0b0000_0001;

        let result: u8 = (r >> 1) | msb;

        self.set_register(&register, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, lsb > 0);

        return 8;
    }

    fn sra_hl(&mut self, mmu: &mut MMU) -> u8 {
        //shift right arithmetic (b0=0)
        let r = mmu.read(self.HL);
        let msb = r & 0b1000_0000;
        let lsb = r & 0b0000_0001;

        let result: u8 = (r >> 1) | msb;

        mmu.write(self.HL, result);

        //z00c
        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, lsb > 0);

        return 16;
    }

    fn rlc(&mut self, register: Register) -> u8 {
        let r = self.get_register(&register);

        let msb = r & 0b1000_0000;

        let res = (r << 1) | (r >> 7);

        self.set_register(&register, res);

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, msb > 0);

        return 8;
    }

    fn rlc_hl(&mut self, mmu: &mut MMU) -> u8 {
        let r = mmu.read(self.HL);

        let msb = r & 0b1000_0000;

        let res = (r << 1) | (r >> 7);

        mmu.write(self.HL, res);

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, msb > 0);

        return 16;
    }

    fn rrc(&mut self, register: Register) -> u8 {
        let r = self.get_register(&register);

        let lsb = r & 0b0000_0001;

        let res = (r >> 1) | (r << 7);

        self.set_register(&register, res);

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, lsb > 0);

        return 8;
    }

    fn rrc_hl(&mut self, mmu: &mut MMU) -> u8 {
        let r = mmu.read(self.HL);

        let lsb = r & 0b0000_0001;

        let res = (r >> 1) | (r << 7);

        mmu.write(self.HL, res);

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, lsb > 0);

        return 16;
    }

    fn rr(&mut self, register: Register) -> u8 {
        let r = self.get_register(&register);
        let mut carry = self.get_flag(Flag::C);

        let lsb = r & 1;

        let mut res = r >> 1;
        if carry {
            res |= 0b1000_0000;
        }

        self.set_register(&register, res);
        carry = lsb > 0;

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 8;
    }

    fn rl(&mut self, register: Register) -> u8 {
        let r = self.get_register(&register);
        let mut carry = self.get_flag(Flag::C);

        let msb = r & 0b1000_0000;

        let mut res = r << 1;
        if carry {
            res |= 0b0000_0001;
        }

        self.set_register(&register, res);
        carry = msb > 0;

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 8;
    }

    fn rr_hl(&mut self, mmu: &mut MMU) -> u8 {
        let r = mmu.read(self.HL);
        let mut carry = self.get_flag(Flag::C);

        let lsb = r & 1;

        let mut res = r >> 1;
        if carry {
            res |= 0b1000_0000;
        }

        mmu.write(self.HL, res);
        carry = lsb > 0;

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 16;
    }

    fn rl_hl(&mut self, mmu: &mut MMU) -> u8 {
        let r = mmu.read(self.HL);
        let mut carry = self.get_flag(Flag::C);

        let msb = r & 0b1000_0000;

        let mut res = r << 1;
        if carry {
            res |= 0b0000_0001;
        }

        mmu.write(self.HL, res);
        carry = msb > 0;

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 16;
    }

    fn rra(&mut self) -> u8 {
        self.rr(Register::A);

        self.set_flag(Flag::Z, false);

        return 0;
    }

    fn rla(&mut self) -> u8 {
        self.rl(Register::A);

        self.set_flag(Flag::Z, false);

        return 0;
    }

    fn stop(&mut self, mmu: &MMU) -> u8 {
        self.fetch_byte(mmu);

        return 8;
        //low power standby mode???
    }

    fn add_imm(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte(mmu);

        let value = a + n;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (n & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value < a);

        return 8;
    }

    fn sub_imm(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte(mmu);

        let value = a - n;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (n & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 8;
    }

    fn adc(&mut self, register: Register) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a + r + cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (r & 0xf) + (cy)) > 0xF);
        self.set_flag(Flag::C, a as u32 + r as u32 + cy as u32 > 255);

        return 4;
    }

    fn adc_a_imm(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.fetch_byte(mmu);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a + r + cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (r & 0xf) + (cy & 0xf)) > 0xF);
        self.set_flag(Flag::C, a as u32 + r as u32 + cy as u32 > 255);

        return 8;
    }

    fn sbc_a_imm(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.fetch_byte(mmu);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a - r - cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (r & 0xf) - (cy & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value >= a && (r > 0 || cy > 0));

        return 8;
    }

    fn sub(&mut self, register: Register) -> u8 {
        //A=A - r
        let a = self.get_register(&Register::A);

        let r = self.get_register(&register);

        let value = a - r;

        self.set_register(&Register::A, value);

        //Z 1 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (r & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 4;
    }

    fn sub_hl(&mut self, mmu: &MMU) -> u8 {
        //A=A-(HL)
        let a = self.get_register(&Register::A);

        let hl = mmu.read(self.HL);

        let value = a - hl;

        self.set_register(&Register::A, value);

        //Z 1 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (hl & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 8;
    }

    fn adc_hl(&mut self, mmu: &MMU) -> u8 {
        let a = self.get_register(&Register::A);
        let r = mmu.read(self.HL);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a + r + cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (r & 0xf) + (cy & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, a as u32 + r as u32 + cy as u32 > 255);

        return 8;
    }

    fn sbc(&mut self, register: Register) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.get_register(&register);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a - r - cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (r & 0xf) - (cy & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value >= a && (r > 0 || cy > 0));

        return 4;
    }

    fn sbc_hl(&mut self, mmu: &MMU) -> u8 {
        //A=A-(HL)-cy
        let a = self.get_register(&Register::A);
        let hl = mmu.read(self.HL);
        let cy = self.get_flag(Flag::C) as u8;
        let value = a - hl - cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(
            Flag::H,
            ((a & 0xf) - (hl & 0xf) - (cy & 0xf)) & 0x10 == 0x10,
        );
        self.set_flag(Flag::C, value >= a && (hl > 0 || cy > 0));

        return 8;
    }

    fn rlca(&mut self) -> u8 {
        //rotate A left
        let a = self.get_register(&Register::A);
        let msb = a & 0b1000_0000;
        let value = (a << 1) | (a >> 7);

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, false);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, msb > 0);

        return 4;
    }

    fn rrca(&mut self) -> u8 {
        //rotate A right
        let a = self.get_register(&Register::A);
        let lsb = a & 0b0000_0001;
        let value = (a >> 1) | (a << 7);

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, false);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, lsb > 0);

        return 4;
    }

    fn ld_sp(&mut self, mmu: &mut MMU) -> u8 {
        //(nn)=SP
        let address = self.fetch_word(mmu);
        mmu.write(address, (self.SP & 0x00FF) as u8);
        mmu.write(address + 1, ((self.SP & 0xFF00) >> 8) as u8);

        return 20;
    }

    fn daa(&mut self) -> u8 {
        let mut a = self.get_register(&Register::A);

        let mut carry = false;
        if !self.get_flag(Flag::N) {
            if self.get_flag(Flag::C) || a > 0x99 {
                a += 0x60;
                carry = true;
            }
            if self.get_flag(Flag::H) || (a & 0x0F) > 0x09 {
                a += 0x06;
            }
        } else if self.get_flag(Flag::C) {
            carry = true;
            if self.get_flag(Flag::H) {
                a += 0x9A;
            } else {
                a += 0xA0;
            }
        } else if self.get_flag(Flag::H) {
            a += 0xFA;
        }

        self.set_register(&Register::A, a);

        self.set_flag(Flag::Z, a == 0);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 4;
    }

    fn scf(&mut self) -> u8 {
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, true);

        return 4;
    }

    fn ccf(&mut self) -> u8 {
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, self.get_flag(Flag::C) ^ true);

        return 4;
    }

    fn halt(&mut self) -> u8 {
        self.halted = true;
        //println!("HALT!");
        return 4;
    }

    fn add_sp(&mut self, mmu: &MMU) -> u8 {
        //SP = SP +/- dd ; dd is 8-bit signed number

        let dd = self.fetch_byte(mmu) as i8;

        let before = self.SP;
        self.SP += dd as u16;

        self.set_flag(Flag::Z, false);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((before & 0xf) + (dd as u16 & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, self.SP & 0x00FF < before & 0xFF);

        return 16;
    }

    fn bit(&mut self, bit: u8, register: Register) -> u8 {
        let r = self.get_register(&register);

        self.set_flag(Flag::Z, (r & (1 << bit)) == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);

        return 8;
    }

    fn bit_hl(&mut self, bit: u8, mmu: &MMU) -> u8 {
        let r = mmu.read(self.HL);

        self.set_flag(Flag::Z, (r & (1 << bit)) == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);

        return 16;
    }
}
