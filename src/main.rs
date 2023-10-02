#![allow(arithmetic_overflow)]

use std::env;
use std::fmt::Display;
use std::process::exit;

#[derive(Clone)]
struct Cart {
    name: String,
    rom: Vec<u8>,
}

impl Cart {
    fn load(bytes: Vec<u8>) -> Cart {
        //TODO: perform checksum, get the ROM name etc etc
        let title = &bytes[0x134..0x143];

        //let cart_type = bytes[0x147];
        // if cart_type != 0x00 {
        //     panic!("Unsupported cart type: 0x{:02X}", cart_type);
        // }

        return Cart {
            name: std::str::from_utf8(&title.to_vec())
                .unwrap()
                .trim_matches(char::from(0))
                .to_string(),
            rom: bytes,
        };
    }
}

const RAM_START: u16 = 0x8000;

struct Color {
    r: u8,
    g: u8,
    b: u8,
}

impl Color {
    fn from(hex: u32) -> Color {
        return Color {
            r: ((hex & 0xFF000000) >> 24) as u8,
            g: ((hex & 0x00FF0000) >> 16) as u8,
            b: ((hex & 0x0000FF00) >> 08) as u8,
        };
    }
}

struct Palette {
    black: u32,
    dark: u32,
    light: u32,
    white: u32,
}

// struct PPU {
//     pixels: [u32; 256 * 256],
// }

// impl PPU {
//     fn get_frame(&self) -> [u8; 160 * 144 * 3] {
//         return [0; 160 * 144 * 3];
//     }
// }

#[derive(Debug)]
enum AddressingMode {
    Mode8000,
    Mode8800,
}

struct Screen {
    palette: Palette,
    pixels: [u32; 256 * 256],
}

impl Screen {
    fn new(palette: Palette) -> Screen {
        return Screen {
            palette: palette,
            pixels: [0; 256 * 256],
        };
    }
}

struct CPU {
    //debug
    enable_logging: bool,
    //hardware
    joypad: Joypad,
    screen: Screen,
    //state
    halted: bool,
    ime: bool,
    remaining_cycles: u8, //horrible name
    clock_cycles: u64,
    //registers
    AF: u16,
    BC: u16,
    DE: u16,
    HL: u16,
    SP: u16,
    PC: u16,
    //memory
    rom: Vec<u8>,
    ram: [u8; 0x8000],
}

enum Flag {
    Z,
    N,
    H,
    C,
}

enum WideRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
}

#[derive(Debug)]
enum Register {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Interrupt {
    VBlank,
    LCD_STAT,
    Timer,
    Serial,
    Joypad,
}

enum JoypadButton {
    Right,
    Left,
    Up,
    Down,
    A,
    B,
    Select,
    Start,
}

struct Joypad {
    state: u8,
}
enum ButtonMode {
    Action,
    Direction,
}
impl Joypad {
    fn new() -> Joypad {
        return Joypad { state: 0 };
    }

    fn read(&self, mode: ButtonMode) -> u8 {
        return match mode {
            ButtonMode::Action => !((self.state & 0xF0) >> 4),
            ButtonMode::Direction => !(self.state & 0x0F),
        };
    }
}

fn half_carry(a: u8, b: u8) -> bool {
    return ((a & 0xf) - (b & 0xf)) & 0x10 == 0x10;
}

fn half_carry_16(a: u16, b: u16) -> bool {
    return ((a & 0xfff) + (b & 0xfff)) > 0xfff;
}

fn get_bit(byte: u8, bit: u8) -> u8 {
    return if byte & (1 << bit) > 0 { 1 } else { 0 };
}

impl CPU {
    //Maybe should make an MMU? kind of what these functions are though
    fn read(&self, address: u16) -> u8 {
        if address < RAM_START {
            return self.rom[address as usize];
        }
        return match address {
            0xE000..=0xFDFF => self.ram[(address - RAM_START - 0x2000) as usize], //Mirror RAM
            0xFF00 => {
                let p1 = self.ram[(address - RAM_START) as usize];

                if get_bit(p1, 4) == 0 {
                    return p1 & (0xF0 | self.joypad.read(ButtonMode::Direction));
                } else if get_bit(p1, 5) == 0 {
                    return p1 & (0xF0 | self.joypad.read(ButtonMode::Action));
                } else {
                    return p1;
                }
            }
            0xFF01 => 0xFF,
            _ => self.ram[(address - RAM_START) as usize],
        };
    }

    fn write(&mut self, address: u16, value: u8) {
        if address < RAM_START {
            return;
        }
        let ram_address = (address - RAM_START) as usize;
        match address {
            0xE000..=0xFDFF => {
                //mirror ram, do not write
            }
            0xFEA0..=0xFEFF => {
                if self.enable_logging {
                    println!("attempted to write to restricted section 0x{:04X}", address);
                }
            }
            0xFF00 => {
                //pins 0-3 are read only
                self.ram[ram_address] = (value & 0xF0) | (self.ram[ram_address] & 0x0F);
            }
            0xFF04 => self.ram[ram_address] = 0x00,
            0xFF0F => (), //shouldn't write to 0xFF0F manually
            0xFF46 => {
                self.dma_transfer(value);
                self.ram[ram_address] = value;
            }
            _ => self.ram[ram_address] = value,
        }
    }

    fn dma_transfer(&mut self, lower_byte: u8) {
        //TODO: this may need to be smarter with timings and memory access restrictions
        for offset in 0x00..=0x9F {
            let source_address = lower_byte as u16 * 0x100 + offset;
            let dest_address = 0xFE00 + offset;

            self.write(dest_address, self.read(source_address));
        }

        //self.clock_cycles += 160; //better to implement as a subroutine tbh
    }

    fn fetch_byte(&mut self) -> u8 {
        let byte = self.read(self.PC);

        self.PC += 1;

        return byte;
    }

    fn fetch_word(&mut self) -> u16 {
        let lo: u16 = self.fetch_byte() as u16;
        let hi: u16 = self.fetch_byte() as u16;

        return (hi << 8) | lo;
    }

    fn press_button(&mut self, button: JoypadButton) {
        let joypad_state = self.read(0xFF00);

        let button_ord = button as u8;
        self.joypad.state |= 1 << button_ord;

        //should only fire an interrupt if the input is selected by bits 5/6
        let select = joypad_state ^ 0b0011_0000;
        let select_bit = ((button_ord / 4) + 1) << 4;

        if (select & select_bit) > 0 {
            self.set_interrupt_flag(Interrupt::Joypad, true);
        }
    }

    fn release_button(&mut self, button: JoypadButton) {
        let button_ord = button as u8;
        self.joypad.state &= !(1 << button_ord);
    }

    fn interrupt_enabled(&self, interrupt: Interrupt) -> bool {
        let ie = self.read(0xFFFF);
        let interrupt_mask = 1 << interrupt as u8;

        // if self.enable_logging {
        //     println!("ie: {:08b}, ie&mask: {:08b}", ie, ie & interrupt_mask);
        // }
        return (ie & interrupt_mask) > 0;
    }

    fn set_interrupt_flag(&mut self, interrupt: Interrupt, set: bool) {
        let interrupt_mask = 1 << interrupt as u8;
        let if_interrupt_flags = self.read(0xFF0F);

        if set {
            self.ram[0xFF0F - RAM_START as usize] = if_interrupt_flags | interrupt_mask;
        } else {
            self.ram[0xFF0F - RAM_START as usize] = if_interrupt_flags & !interrupt_mask;
        }
    }

    fn get_interrupt_flag(&self, interrupt: Interrupt) -> bool {
        let interrupt_mask = 1 << interrupt as u8;
        return (self.read(0xFF0F) & interrupt_mask) > 0;
    }

    fn handle_interrupt(&mut self, interrupt: Interrupt) -> u8 {
        if !self.ime {
            self.halted = false;
            return 0;
        }
        let interrupt_set = self.get_interrupt_flag(interrupt);
        if interrupt_set && self.interrupt_enabled(interrupt) {
            self.halted = false;
            if self.enable_logging {
                println!("Handling {:?}", interrupt);
            }

            //The CPU automatically disables all the other interrupts by setting IME=0 when it services an interrupt.
            self.ime = false;
            self.set_interrupt_flag(interrupt, false);

            self.SP -= 2;
            self.write(self.SP, (self.PC & 0x00FF) as u8);
            self.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);

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

    fn tick(&mut self, ticks: u64) {
        self.clock_cycles += 1;

        //Timer
        let tac = self.read(0xFF07);
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
            self.ram[(0xFF04 - RAM_START) as usize] += 1;
        }

        //update TIMA register
        if timer_enable && ticks % timer_frequency == 0 {
            let tima = self.read(0xFF05);
            //This timer is incremented at the clock frequency specified by the TAC register ($FF07).
            //When the value overflows (exceeds $FF) it is reset to the value specified in TMA (FF06) and an interrupt is requested
            if tima == 0xFF {
                self.set_interrupt_flag(Interrupt::Timer, true);
                let tma = self.read(0xFF06);
                self.write(0xFF05, tma);
            } else {
                self.write(0xFF05, tima + 1);
            }
        }

        //Interrupts
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::VBlank);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::LCD_STAT);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Timer);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Serial);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.handle_interrupt(Interrupt::Joypad);
        }
        if self.remaining_cycles == 0 {
            self.remaining_cycles = self.next_op();
        }

        self.remaining_cycles -= 1;
    }

    fn next_op(&mut self) -> u8 {
        if !self.halted {
            let opcode = self.fetch_byte();

            // if self.enable_logging {
            //     self.PC -= 1; //just to make the logging better
            //     self.log_state(opcode);
            //     self.PC += 1;
            // }

            return match opcode {
                //0x00
                0x00 => self.nop(),
                0x01 => self.ld_imm_wide(WideRegister::BC),
                0x02 => self.ld_ind_wide(WideRegister::BC),
                0x03 => self.inc_wide(WideRegister::BC),
                0x04 => self.inc(Register::B),
                0x05 => self.dec(Register::B),
                0x06 => self.ld_imm(Register::B),
                0x07 => self.rlca(),
                0x08 => self.ld_sp(),
                0x09 => self.add_hl_wide(WideRegister::BC),
                0x0A => self.ld_a_ind(WideRegister::BC),
                0x0B => self.dec_wide(WideRegister::BC),
                0x0C => self.inc(Register::C),
                0x0D => self.dec(Register::C),
                0x0E => self.ld_imm(Register::C),
                0x0F => self.rrca(),
                //0x10
                0x10 => self.stop(),
                0x11 => self.ld_imm_wide(WideRegister::DE),
                0x12 => self.ld_ind_wide(WideRegister::DE),
                0x13 => self.inc_wide(WideRegister::DE),
                0x14 => self.inc(Register::D),
                0x15 => self.dec(Register::D),
                0x16 => self.ld_imm(Register::D),
                0x17 => self.rla(),
                0x18 => self.jr(),
                0x19 => self.add_hl_wide(WideRegister::DE),
                0x1A => self.ld_a_ind(WideRegister::DE),
                0x1B => self.dec_wide(WideRegister::DE),
                0x1C => self.inc(Register::E),
                0x1D => self.dec(Register::E),
                0x1E => self.ld_imm(Register::E),
                0x1F => self.rra(),
                //0x20
                0x20 => self.jr_cond(Flag::Z, false),
                0x21 => self.ld_imm_wide(WideRegister::HL),
                0x22 => self.ldi_hl_a(),
                0x23 => self.inc_wide(WideRegister::HL),
                0x24 => self.inc(Register::H),
                0x25 => self.dec(Register::H),
                0x26 => self.ld_imm(Register::H),
                0x27 => self.daa(),
                0x28 => self.jr_cond(Flag::Z, true),
                0x29 => self.add_hl_wide(WideRegister::HL),
                0x2A => self.ldi(Register::A, WideRegister::HL),
                0x2B => self.dec_wide(WideRegister::HL),
                0x2C => self.inc(Register::L),
                0x2D => self.dec(Register::L),
                0x2E => self.ld_imm(Register::L),
                0x2F => self.cpl(),
                //0x30
                0x30 => self.jr_cond(Flag::C, false),
                0x31 => self.ld_imm_wide(WideRegister::SP),
                0x32 => self.ldd(WideRegister::HL, Register::A),
                0x33 => self.inc_wide(WideRegister::SP),
                0x34 => self.inc_hl(),
                0x35 => self.dec_hl(),
                0x36 => self.ld_imm_ind(WideRegister::HL),
                0x37 => self.scf(),
                0x38 => self.jr_cond(Flag::C, true),
                0x39 => self.add_hl_wide(WideRegister::SP),
                0x3A => self.ldd_a_hl(),
                0x3B => self.dec_wide(WideRegister::SP),
                0x3C => self.inc(Register::A),
                0x3D => self.dec(Register::A),
                0x3E => self.ld_imm(Register::A),
                0x3F => self.ccf(),
                //0x40
                0x40 => self.ld(Register::B, Register::B),
                0x41 => self.ld(Register::B, Register::C),
                0x42 => self.ld(Register::B, Register::D),
                0x43 => self.ld(Register::B, Register::E),
                0x44 => self.ld(Register::B, Register::H),
                0x45 => self.ld(Register::B, Register::L),
                0x46 => self.ld_hl(Register::B),
                0x47 => self.ld(Register::B, Register::A),
                0x48 => self.ld(Register::C, Register::B),
                0x49 => self.ld(Register::C, Register::C),
                0x4A => self.ld(Register::C, Register::D),
                0x4B => self.ld(Register::C, Register::E),
                0x4C => self.ld(Register::C, Register::H),
                0x4D => self.ld(Register::C, Register::L),
                0x4E => self.ld_hl(Register::C),
                0x4F => self.ld(Register::C, Register::A),
                //0x50
                0x50 => self.ld(Register::D, Register::B),
                0x51 => self.ld(Register::D, Register::C),
                0x52 => self.ld(Register::D, Register::D),
                0x53 => self.ld(Register::D, Register::E),
                0x54 => self.ld(Register::D, Register::H),
                0x55 => self.ld(Register::D, Register::L),
                0x56 => self.ld_hl(Register::D),
                0x57 => self.ld(Register::D, Register::A),
                0x58 => self.ld(Register::E, Register::B),
                0x59 => self.ld(Register::E, Register::C),
                0x5A => self.ld(Register::E, Register::D),
                0x5B => self.ld(Register::E, Register::E),
                0x5C => self.ld(Register::E, Register::H),
                0x5D => self.ld(Register::E, Register::L),
                0x5E => self.ld_hl(Register::E),
                0x5F => self.ld(Register::E, Register::A),
                //0x60
                0x60 => self.ld(Register::H, Register::B),
                0x61 => self.ld(Register::H, Register::C),
                0x62 => self.ld(Register::H, Register::D),
                0x63 => self.ld(Register::H, Register::E),
                0x64 => self.ld(Register::H, Register::H),
                0x65 => self.ld(Register::H, Register::L),
                0x66 => self.ld_hl(Register::H),
                0x67 => self.ld(Register::H, Register::A),
                0x68 => self.ld(Register::L, Register::B),
                0x69 => self.ld(Register::L, Register::C),
                0x6A => self.ld(Register::L, Register::D),
                0x6B => self.ld(Register::L, Register::E),
                0x6C => self.ld(Register::L, Register::H),
                0x6D => self.ld(Register::L, Register::L),
                0x6E => self.ld_hl(Register::L),
                0x6F => self.ld(Register::L, Register::A),
                //0x70
                0x70 => self.ld_ind(Register::B),
                0x71 => self.ld_ind(Register::C),
                0x72 => self.ld_ind(Register::D),
                0x73 => self.ld_ind(Register::E),
                0x74 => self.ld_ind(Register::H),
                0x75 => self.ld_ind(Register::L),
                0x76 => self.halt(),
                0x77 => self.ld_ind(Register::A),
                0x78 => self.ld(Register::A, Register::B),
                0x79 => self.ld(Register::A, Register::C),
                0x7A => self.ld(Register::A, Register::D),
                0x7B => self.ld(Register::A, Register::E),
                0x7C => self.ld(Register::A, Register::H),
                0x7D => self.ld(Register::A, Register::L),
                0x7E => self.ld_hl(Register::A),
                0x7F => self.ld(Register::A, Register::A),
                //0x80
                0x80 => self.add(Register::B),
                0x81 => self.add(Register::C),
                0x82 => self.add(Register::D),
                0x83 => self.add(Register::E),
                0x84 => self.add(Register::H),
                0x85 => self.add(Register::L),
                0x86 => self.add_a_hl(),
                0x87 => self.add(Register::A),
                0x88 => self.adc(Register::B),
                0x89 => self.adc(Register::C),
                0x8A => self.adc(Register::D),
                0x8B => self.adc(Register::E),
                0x8C => self.adc(Register::H),
                0x8D => self.adc(Register::L),
                0x8E => self.adc_hl(),
                0x8F => self.adc(Register::A),
                //0x90
                0x90 => self.sub(Register::B),
                0x91 => self.sub(Register::C),
                0x92 => self.sub(Register::D),
                0x93 => self.sub(Register::E),
                0x94 => self.sub(Register::H),
                0x95 => self.sub(Register::L),
                0x96 => self.sub_hl(),
                0x97 => self.sub(Register::A),
                0x98 => self.sbc(Register::B),
                0x99 => self.sbc(Register::C),
                0x9A => self.sbc(Register::D),
                0x9B => self.sbc(Register::E),
                0x9C => self.sbc(Register::H),
                0x9D => self.sbc(Register::L),
                0x9E => self.sbc_hl(),
                0x9F => self.sbc(Register::A),
                //0xA0
                0xA0 => self.and(Register::B),
                0xA1 => self.and(Register::C),
                0xA2 => self.and(Register::D),
                0xA3 => self.and(Register::E),
                0xA4 => self.and(Register::H),
                0xA5 => self.and(Register::L),
                0xA6 => self.and_hl(),
                0xA7 => self.and(Register::A),
                0xA8 => self.xor(Register::B),
                0xA9 => self.xor(Register::C),
                0xAA => self.xor(Register::D),
                0xAB => self.xor(Register::E),
                0xAC => self.xor(Register::H),
                0xAD => self.xor(Register::L),
                0xAE => self.xor_hl(),
                0xAF => self.xor(Register::A),
                //0xB0
                0xB0 => self.or(Register::B),
                0xB1 => self.or(Register::C),
                0xB2 => self.or(Register::D),
                0xB3 => self.or(Register::E),
                0xB4 => self.or(Register::H),
                0xB5 => self.or(Register::L),
                0xB6 => self.or_hl(),
                0xB7 => self.or(Register::A),
                0xB8 => self.cp(Register::B),
                0xB9 => self.cp(Register::C),
                0xBA => self.cp(Register::D),
                0xBB => self.cp(Register::E),
                0xBC => self.cp(Register::H),
                0xBD => self.cp(Register::L),
                0xBE => self.cp_hl(),
                0xBF => self.cp(Register::A),
                //0xC0
                0xC0 => self.ret_cond(Flag::Z, false),
                0xC1 => self.pop(WideRegister::BC),
                0xC2 => self.jp_imm(Flag::Z, false),
                0xC4 => self.call_cond(Flag::Z, false),
                0xC8 => self.ret_cond(Flag::Z, true),
                0xC9 => self.ret(),
                0xC3 => self.jp(),
                0xC5 => self.push(WideRegister::BC),
                0xC6 => self.add_imm(),
                0xC7 => self.rst(0x00),
                0xCA => self.jp_imm(Flag::Z, true),
                0xCB => self.prefix_cb(),
                0xCC => self.call_cond(Flag::Z, true),
                0xCD => self.call(),
                0xCE => self.adc_a_imm(),
                0xCF => self.rst(0x08),
                //0xD0
                0xD0 => self.ret_cond(Flag::C, false),
                0xD1 => self.pop(WideRegister::DE),
                0xD2 => self.jp_imm(Flag::C, false),
                0xD4 => self.call_cond(Flag::C, false),
                0xD5 => self.push(WideRegister::DE),
                0xD6 => self.sub_imm(),
                0xD7 => self.rst(0x10),
                0xD8 => self.ret_cond(Flag::C, true),
                0xD9 => self.reti(),
                0xDA => self.jp_imm(Flag::C, true),
                0xDC => self.call_cond(Flag::C, true),
                0xDE => self.sbc_a_imm(),
                0xDF => self.rst(0x18),
                //0xE0
                0xE0 => self.ld_io_n(),
                0xE1 => self.pop(WideRegister::HL),
                0xE2 => self.ld_io_c(),
                0xE5 => self.push(WideRegister::HL),
                0xE6 => self.and_imm(),
                0xE7 => self.rst(0x20),
                0xE8 => self.add_sp(),
                0xE9 => self.jp_hl(),
                0xEA => self.ld_nn_a(),
                0xEE => self.xor_imm(),
                0xEF => self.rst(0x28),
                //0xF0
                0xF0 => self.rd_io_n(),
                0xF1 => self.pop(WideRegister::AF),
                0xF2 => self.rd_io_c(),
                0xF3 => self.di(),
                0xF5 => self.push(WideRegister::AF),
                0xF6 => self.or_imm(),
                0xF7 => self.rst(0x30),
                0xF8 => self.ld_hl_sp_dd(),
                0xF9 => self.ld_sp_hl(),
                0xFA => self.ld_a_nn(),
                0xFB => self.ei(),
                0xFE => self.cp_imm(),
                0xFF => self.rst(0x38),
                _ => {
                    self.log_state(opcode);
                    todo!("opcode 0x{:02X?} hasn't been implemented yet!", opcode);
                }
            };
        } else {
            return 0;
        }
    }

    fn log_state(&mut self, opcode: u8) {
        println!(
            "OP: 0x{:02X?} | AF={:04X?}, BC={:04X?}, DE={:04X?}, HL={:04X?} | SP={:04X?}, PC={:04X?} | flags: {}{}{}{} | ie: {:02X?} if: {:02X?} ime: {} | clock: {:16?} | TIMA: {:02X?}",
            opcode, self.AF, self.BC, self.DE, self.HL, self.SP, self.PC,
            if self.get_flag(Flag::Z) {'z'} else {'-'},
            if self.get_flag(Flag::N) {'n'} else {'-'},
            if self.get_flag(Flag::H) {'h'} else {'-'},
            if self.get_flag(Flag::C) {'c'} else {'-'},
            self.read(0xFFFF), self.read(0xFF0F), if self.ime {1} else {0},
            self.clock_cycles,
            self.read(0xFF05)
        );
    }

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
            Register::F => (self.AF & 0x00FF) as u8,
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
            Register::F => self.AF = (self.AF & 0xFF00) | value as u16,
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

    fn ld_imm(&mut self, reg: Register) -> u8 {
        let val = self.fetch_byte();

        self.set_register(&reg, val);

        return 8;
    }

    fn ld_imm_wide(&mut self, wide_reg: WideRegister) -> u8 {
        let val = self.fetch_word();

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

    fn xor_imm(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte();

        let val = a ^ n;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 8;
    }

    fn xor_hl(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.read(self.HL);

        let val = a ^ r;

        self.set_flag(Flag::Z, val == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        self.set_register(&Register::A, val);

        return 8;
    }

    fn jp(&mut self) -> u8 {
        let address = self.fetch_word();

        self.PC = address;
        return 16;
    }

    fn ldd(&mut self, wide_reg: WideRegister, reg: Register) -> u8 {
        //(HL)=A, HL=HL-1

        let address = self.get_wide_register(&wide_reg);
        self.write(address, self.get_register(&reg));

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

    fn inc_hl(&mut self) -> u8 {
        let hl = self.read(self.HL);

        let value = hl + 1;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((hl & 0xf) + (1 & 0xf)) & 0x10 == 0x10);

        self.write(self.HL, value);

        return 12;
    }

    fn dec_hl(&mut self) -> u8 {
        let hl = self.read(self.HL);

        let value = hl - 1;

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((hl & 0xf) - (1 & 0xf)) & 0x10 == 0x10);

        self.write(self.HL, value);

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

    fn jr(&mut self) -> u8 {
        let offset = self.fetch_byte() as i8;

        self.PC += offset as u16;

        return 12;
    }

    fn jr_cond(&mut self, flag: Flag, jump_if_true: bool) -> u8 {
        let offset = self.fetch_byte() as i8;

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

    fn ld_io_n(&mut self) -> u8 {
        //ld (FF00+n),A
        //write to io-port n (memory FF00+n)

        let offset = self.fetch_byte();

        let value = self.get_register(&Register::A);

        self.write(0xFF00 + (offset as u16), value);

        return 12;
    }

    fn ld_io_c(&mut self) -> u8 {
        //ld (FF00+C),A
        //write to io-port C (memory FF00+C)

        let a = self.get_register(&Register::A);
        let c = self.get_register(&Register::C);

        self.write(0xFF00 + c as u16, a);

        return 8;
    }

    fn rd_io_n(&mut self) -> u8 {
        //ld A,(FF00+n)
        //read from io-port n (memory FF00+n)

        let offset = self.fetch_byte();
        let value = self.read(0xFF00 + offset as u16);

        self.set_register(&Register::A, value);

        return 12;
    }

    fn rd_io_c(&mut self) -> u8 {
        //ld A,(FF00+c)
        //read from io-port n (memory FF00+n)

        let offset = self.get_register(&Register::C);
        let value = self.read(0xFF00 + offset as u16);

        self.set_register(&Register::A, value);

        return 12;
    }

    fn cp_imm(&mut self) -> u8 {
        //cp n
        //compare A-n
        //z1hc
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte();

        let result = a - n;

        self.set_flag(Flag::Z, result == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, half_carry(a, n));
        self.set_flag(Flag::C, result > a);

        return 8;
    }

    fn ld_imm_ind(&mut self, reg: WideRegister) -> u8 {
        let value = self.fetch_byte();

        self.write(self.get_wide_register(&reg), value);

        return 12;
    }

    fn ld_nn_a(&mut self) -> u8 {
        //ld (nn),A
        //(nn)=A
        let address = self.fetch_word();
        let value = self.get_register(&Register::A);

        self.write(address, value);

        return 16;
    }

    fn ld_a_nn(&mut self) -> u8 {
        //A=(nn)

        let address = self.fetch_word();
        let value = self.read(address);

        self.set_register(&Register::A, value);

        return 16;
    }

    fn ldi(&mut self, register: Register, wide_register: WideRegister) -> u8 {
        //ldi A,(HL)
        //A=(HL), HL=HL+1

        let hl = self.get_wide_register(&wide_register);
        let value = self.read(hl);
        self.set_register(&register, value);

        self.set_wide_register(&wide_register, hl + 1);

        return 8;
    }

    fn call(&mut self) -> u8 {
        //call to nn, SP=SP-2, (SP)=PC, PC=nn
        let address = self.fetch_word();

        self.SP -= 2;
        self.write(self.SP, (self.PC & 0x00FF) as u8);
        self.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);
        self.PC = address;

        return 24;
    }

    fn call_cond(&mut self, flag: Flag, jump_if_true: bool) -> u8 {
        let address = self.fetch_word();

        if self.get_flag(flag) == jump_if_true {
            self.SP -= 2;
            self.write(self.SP, (self.PC & 0x00FF) as u8);
            self.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);
            self.PC = address;
            return 24;
        } else {
            return 12;
        }
    }

    fn rst(&mut self, address: u16) -> u8 {
        self.SP -= 2;
        self.write(self.SP, (self.PC & 0x00FF) as u8);
        self.write(self.SP + 1, ((self.PC & 0xFF00) >> 8) as u8);

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

    fn or_hl(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.read(self.HL);

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

    fn cp_hl(&mut self) -> u8 {
        //compare A-(HL)
        let a = self.get_register(&Register::A);
        let r = self.read(self.HL);
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

    fn and_hl(&mut self) -> u8 {
        //A=A & r
        let a = self.get_register(&Register::A);
        let r = self.read(self.HL);
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

    fn add_a_hl(&mut self) -> u8 {
        //A=A+(HL)

        let a = self.get_register(&Register::A);
        let hl = self.read(self.HL);

        let value = a + hl;

        self.set_register(&Register::A, value);

        //Z 0 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (hl & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value < a);

        return 8;
    }

    fn ret(&mut self) -> u8 {
        //return, PC=(SP), SP=SP+2

        let pc_lo = self.read(self.SP) as u16;
        let pc_hi = self.read(self.SP + 1) as u16;

        self.PC = (pc_hi << 8) | pc_lo;
        self.SP += 2;

        return 16;
    }

    fn reti(&mut self) -> u8 {
        //return and enable interrupts
        self.ime = true;

        let pc_lo = self.read(self.SP) as u16;
        let pc_hi = self.read(self.SP + 1) as u16;

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

    fn and_imm(&mut self) -> u8 {
        //A=A & n
        let n = self.fetch_byte();

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

    fn or_imm(&mut self) -> u8 {
        //A=A | n
        let n = self.fetch_byte();

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

    fn prefix_cb(&mut self) -> u8 {
        let n = self.fetch_byte();

        return match n {
            //0x00
            0x00 => self.rlc(Register::B),
            0x01 => self.rlc(Register::C),
            0x02 => self.rlc(Register::D),
            0x03 => self.rlc(Register::E),
            0x04 => self.rlc(Register::H),
            0x05 => self.rlc(Register::L),
            0x06 => self.rlc_hl(),
            0x07 => self.rlc(Register::A),
            0x08 => self.rrc(Register::B),
            0x09 => self.rrc(Register::C),
            0x0A => self.rrc(Register::D),
            0x0B => self.rrc(Register::E),
            0x0C => self.rrc(Register::H),
            0x0D => self.rrc(Register::L),
            0x0E => self.rrc_hl(),
            0x0F => self.rrc(Register::A),
            //0x10
            0x10 => self.rl(Register::B),
            0x11 => self.rl(Register::C),
            0x12 => self.rl(Register::D),
            0x13 => self.rl(Register::E),
            0x14 => self.rl(Register::H),
            0x15 => self.rl(Register::L),
            0x16 => self.rl_hl(),
            0x17 => self.rl(Register::A),
            0x18 => self.rr(Register::B),
            0x19 => self.rr(Register::C),
            0x1A => self.rr(Register::D),
            0x1B => self.rr(Register::E),
            0x1C => self.rr(Register::H),
            0x1D => self.rr(Register::L),
            0x1E => self.rr_hl(),
            0x1F => self.rr(Register::A),
            //0x20
            0x20 => self.sla(Register::B),
            0x21 => self.sla(Register::C),
            0x22 => self.sla(Register::D),
            0x23 => self.sla(Register::E),
            0x24 => self.sla(Register::H),
            0x25 => self.sla(Register::L),
            0x26 => self.sla_hl(),
            0x27 => self.sla(Register::A),
            0x28 => self.sra(Register::B),
            0x29 => self.sra(Register::C),
            0x2A => self.sra(Register::D),
            0x2B => self.sra(Register::E),
            0x2C => self.sra(Register::H),
            0x2D => self.sra(Register::L),
            0x2E => self.sra_hl(),
            0x2F => self.sra(Register::A),
            //0x30
            0x30 => self.swap(Register::B),
            0x31 => self.swap(Register::C),
            0x32 => self.swap(Register::D),
            0x33 => self.swap(Register::E),
            0x34 => self.swap(Register::H),
            0x35 => self.swap(Register::L),
            0x36 => self.swap_hl(),
            0x37 => self.swap(Register::A),
            0x38 => self.srl(Register::B),
            0x39 => self.srl(Register::C),
            0x3A => self.srl(Register::D),
            0x3B => self.srl(Register::E),
            0x3C => self.srl(Register::H),
            0x3D => self.srl(Register::L),
            0x3E => self.srl_hl(),
            0x3F => self.srl(Register::A),
            //0x40
            0x40 => self.bit(0, Register::B),
            0x41 => self.bit(0, Register::C),
            0x42 => self.bit(0, Register::D),
            0x43 => self.bit(0, Register::E),
            0x44 => self.bit(0, Register::H),
            0x45 => self.bit(0, Register::L),
            0x46 => self.bit_hl(0),
            0x47 => self.bit(0, Register::A),
            0x48 => self.bit(1, Register::B),
            0x49 => self.bit(1, Register::C),
            0x4A => self.bit(1, Register::D),
            0x4B => self.bit(1, Register::E),
            0x4C => self.bit(1, Register::H),
            0x4D => self.bit(1, Register::L),
            0x4E => self.bit_hl(1),
            0x4F => self.bit(1, Register::A),
            //0x50
            0x50 => self.bit(2, Register::B),
            0x51 => self.bit(2, Register::C),
            0x52 => self.bit(2, Register::D),
            0x53 => self.bit(2, Register::E),
            0x54 => self.bit(2, Register::H),
            0x55 => self.bit(2, Register::L),
            0x56 => self.bit_hl(2),
            0x57 => self.bit(2, Register::A),
            0x58 => self.bit(3, Register::B),
            0x59 => self.bit(3, Register::C),
            0x5A => self.bit(3, Register::D),
            0x5B => self.bit(3, Register::E),
            0x5C => self.bit(3, Register::H),
            0x5D => self.bit(3, Register::L),
            0x5E => self.bit_hl(3),
            0x5F => self.bit(3, Register::A),
            //0x60
            0x60 => self.bit(4, Register::B),
            0x61 => self.bit(4, Register::C),
            0x62 => self.bit(4, Register::D),
            0x63 => self.bit(4, Register::E),
            0x64 => self.bit(4, Register::H),
            0x65 => self.bit(4, Register::L),
            0x66 => self.bit_hl(4),
            0x67 => self.bit(4, Register::A),
            0x68 => self.bit(5, Register::B),
            0x69 => self.bit(5, Register::C),
            0x6A => self.bit(5, Register::D),
            0x6B => self.bit(5, Register::E),
            0x6C => self.bit(5, Register::H),
            0x6D => self.bit(5, Register::L),
            0x6E => self.bit_hl(5),
            0x6F => self.bit(5, Register::A),
            //0x70
            0x70 => self.bit(6, Register::B),
            0x71 => self.bit(6, Register::C),
            0x72 => self.bit(6, Register::D),
            0x73 => self.bit(6, Register::E),
            0x74 => self.bit(6, Register::H),
            0x75 => self.bit(6, Register::L),
            0x76 => self.bit_hl(6),
            0x77 => self.bit(6, Register::A),
            0x78 => self.bit(7, Register::B),
            0x79 => self.bit(7, Register::C),
            0x7A => self.bit(7, Register::D),
            0x7B => self.bit(7, Register::E),
            0x7C => self.bit(7, Register::H),
            0x7D => self.bit(7, Register::L),
            0x7E => self.bit_hl(7),
            0x7F => self.bit(7, Register::A),
            //0x80
            0x80 => self.res(0, Register::B),
            0x81 => self.res(0, Register::C),
            0x82 => self.res(0, Register::D),
            0x83 => self.res(0, Register::E),
            0x84 => self.res(0, Register::H),
            0x85 => self.res(0, Register::L),
            0x86 => self.res_hl(0),
            0x87 => self.res(0, Register::A),
            0x88 => self.res(1, Register::B),
            0x89 => self.res(1, Register::C),
            0x8A => self.res(1, Register::D),
            0x8B => self.res(1, Register::E),
            0x8C => self.res(1, Register::H),
            0x8D => self.res(1, Register::L),
            0x8E => self.res_hl(1),
            0x8F => self.res(1, Register::A),
            //0x90
            0x90 => self.res(2, Register::B),
            0x91 => self.res(2, Register::C),
            0x92 => self.res(2, Register::D),
            0x93 => self.res(2, Register::E),
            0x94 => self.res(2, Register::H),
            0x95 => self.res(2, Register::L),
            0x96 => self.res_hl(2),
            0x97 => self.res(2, Register::A),
            0x98 => self.res(3, Register::B),
            0x99 => self.res(3, Register::C),
            0x9A => self.res(3, Register::D),
            0x9B => self.res(3, Register::E),
            0x9C => self.res(3, Register::H),
            0x9D => self.res(3, Register::L),
            0x9E => self.res_hl(3),
            0x9F => self.res(3, Register::A),
            //0xA0
            0xA0 => self.res(4, Register::B),
            0xA1 => self.res(4, Register::C),
            0xA2 => self.res(4, Register::D),
            0xA3 => self.res(4, Register::E),
            0xA4 => self.res(4, Register::H),
            0xA5 => self.res(4, Register::L),
            0xA6 => self.res_hl(4),
            0xA7 => self.res(4, Register::A),
            0xA8 => self.res(5, Register::B),
            0xA9 => self.res(5, Register::C),
            0xAA => self.res(5, Register::D),
            0xAB => self.res(5, Register::E),
            0xAC => self.res(5, Register::H),
            0xAD => self.res(5, Register::L),
            0xAE => self.res_hl(5),
            0xAF => self.res(5, Register::A),
            //0xB0
            0xB0 => self.res(6, Register::B),
            0xB1 => self.res(6, Register::C),
            0xB2 => self.res(6, Register::D),
            0xB3 => self.res(6, Register::E),
            0xB4 => self.res(6, Register::H),
            0xB5 => self.res(6, Register::L),
            0xB6 => self.res_hl(6),
            0xB7 => self.res(6, Register::A),
            0xB8 => self.res(7, Register::B),
            0xB9 => self.res(7, Register::C),
            0xBA => self.res(7, Register::D),
            0xBB => self.res(7, Register::E),
            0xBC => self.res(7, Register::H),
            0xBD => self.res(7, Register::L),
            0xBE => self.res_hl(7),
            0xBF => self.res(7, Register::A),
            //0xC0
            0xC0 => self.set(0, Register::B),
            0xC1 => self.set(0, Register::C),
            0xC2 => self.set(0, Register::D),
            0xC3 => self.set(0, Register::E),
            0xC4 => self.set(0, Register::H),
            0xC5 => self.set(0, Register::L),
            0xC6 => self.set_hl(0),
            0xC7 => self.set(0, Register::A),
            0xC8 => self.set(1, Register::B),
            0xC9 => self.set(1, Register::C),
            0xCA => self.set(1, Register::D),
            0xCB => self.set(1, Register::E),
            0xCC => self.set(1, Register::H),
            0xCD => self.set(1, Register::L),
            0xCE => self.set_hl(1),
            0xCF => self.set(1, Register::A),
            //0xD0
            0xD0 => self.set(2, Register::B),
            0xD1 => self.set(2, Register::C),
            0xD2 => self.set(2, Register::D),
            0xD3 => self.set(2, Register::E),
            0xD4 => self.set(2, Register::H),
            0xD5 => self.set(2, Register::L),
            0xD6 => self.set_hl(2),
            0xD7 => self.set(2, Register::A),
            0xD8 => self.set(3, Register::B),
            0xD9 => self.set(3, Register::C),
            0xDA => self.set(3, Register::D),
            0xDB => self.set(3, Register::E),
            0xDC => self.set(3, Register::H),
            0xDD => self.set(3, Register::L),
            0xDE => self.set_hl(3),
            0xDF => self.set(3, Register::A),
            //0xE0
            0xE0 => self.set(4, Register::B),
            0xE1 => self.set(4, Register::C),
            0xE2 => self.set(4, Register::D),
            0xE3 => self.set(4, Register::E),
            0xE4 => self.set(4, Register::H),
            0xE5 => self.set(4, Register::L),
            0xE6 => self.set_hl(4),
            0xE7 => self.set(4, Register::A),
            0xE8 => self.set(5, Register::B),
            0xE9 => self.set(5, Register::C),
            0xEA => self.set(5, Register::D),
            0xEB => self.set(5, Register::E),
            0xEC => self.set(5, Register::H),
            0xED => self.set(5, Register::L),
            0xEE => self.set_hl(5),
            0xEF => self.set(5, Register::A),
            //0xF0
            0xF0 => self.set(6, Register::B),
            0xF1 => self.set(6, Register::C),
            0xF2 => self.set(6, Register::D),
            0xF3 => self.set(6, Register::E),
            0xF4 => self.set(6, Register::H),
            0xF5 => self.set(6, Register::L),
            0xF6 => self.set_hl(6),
            0xF7 => self.set(6, Register::A),
            0xF8 => self.set(7, Register::B),
            0xF9 => self.set(7, Register::C),
            0xFA => self.set(7, Register::D),
            0xFB => self.set(7, Register::E),
            0xFC => self.set(7, Register::H),
            0xFD => self.set(7, Register::L),
            0xFE => self.set_hl(7),
            0xFF => self.set(7, Register::A),
            _ => {
                self.log_state(n);
                todo!("PREFIX CB 0x{:02X?} hasn't been implemented yet!", n);
            }
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

    fn swap_hl(&mut self) -> u8 {
        //exchange low/hi-nibble

        let r = self.read(self.HL);

        let hi = r & 0b11110000;
        let lo = r & 0b00001111;

        let value = (lo << 4) | (hi >> 4);

        self.write(self.HL, value);

        //z000
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, false);

        return 16;
    }

    fn ld_hl_sp_dd(&mut self) -> u8 {
        //HL = SP +/- dd ; dd is 8-bit signed number
        let nn = self.fetch_byte() as i8 as u16;

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

    fn pop(&mut self, wide_register: WideRegister) -> u8 {
        //pop rr
        //rr=(SP) SP=SP+2 ; rr may be BC,DE,HL,AF
        let lo = self.read(self.SP) as u16;
        let hi = self.read(self.SP + 1) as u16;
        self.SP += 2;

        let value = (hi << 8) | lo;

        self.set_wide_register(&wide_register, value);

        return 12;
    }

    fn push(&mut self, wide_register: WideRegister) -> u8 {
        //SP=SP-2 (SP)=rr ; rr may be BC,DE,HL,AF

        let value = self.get_wide_register(&wide_register);

        self.SP -= 2;
        self.write(self.SP, (value & 0x00FF) as u8);
        self.write(self.SP + 1, ((value & 0xFF00) >> 8) as u8);

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

    fn ld_hl(&mut self, register: Register) -> u8 {
        //r=(HL)

        let value = self.read(self.HL);
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

    fn res_hl(&mut self, bit: u8) -> u8 {
        let mut a = self.read(self.HL);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        mask = !mask;

        a &= mask;

        self.write(self.HL, a);

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

    fn set_hl(&mut self, bit: u8) -> u8 {
        let mut a = self.read(self.HL);

        let mut mask: u8 = 0b0000_0001;

        mask = mask << bit;
        a |= mask;

        self.write(self.HL, a);
        return 16;
    }

    fn ld_ind_wide(&mut self, wide_register: WideRegister) -> u8 {
        let value = self.get_register(&Register::A);
        let address = self.get_wide_register(&wide_register);

        self.write(address, value);

        return 8;
    }

    fn ld_a_ind(&mut self, wide_register: WideRegister) -> u8 {
        //A=(DE)
        let value = self.read(self.get_wide_register(&wide_register));
        self.set_register(&Register::A, value);

        return 8;
    }

    fn ldi_hl_a(&mut self) -> u8 {
        //(HL)=A, HL=HL+1

        self.write(self.HL, self.get_register(&Register::A));
        self.HL += 1;

        return 8;
    }

    fn ldd_a_hl(&mut self) -> u8 {
        //A=(HL), HL=HL-1

        let hl = self.read(self.HL);
        self.set_register(&Register::A, hl);
        self.HL -= 1;

        return 8;
    }

    fn jp_imm(&mut self, flag: Flag, jump_if_true: bool) -> u8 {
        let address = self.fetch_word();

        if self.get_flag(flag) == jump_if_true {
            self.PC = address;
            return 16;
        } else {
            return 12;
        }
    }

    fn ret_cond(&mut self, flag: Flag, return_if_true: bool) -> u8 {
        if self.get_flag(flag) == return_if_true {
            let pc_lo = self.read(self.SP) as u16;
            let pc_hi = self.read(self.SP + 1) as u16;

            self.PC = (pc_hi << 8) | pc_lo;
            self.SP += 2;
            return 20;
        } else {
            return 8;
        }
    }

    fn ld_ind(&mut self, register: Register) -> u8 {
        //(HL)=r

        self.write(self.HL, self.get_register(&register));

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

    fn srl_hl(&mut self) -> u8 {
        //shift right logical (b7=0)
        let r = self.read(self.HL);

        let result: u8 = r >> 1;

        self.write(self.HL, result);

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

    fn sla_hl(&mut self) -> u8 {
        //shift left arithmetic (b0=0)
        let r = self.read(self.HL);
        let msb = r & 0b1000_0000;

        let result: u8 = r << 1;

        self.write(self.HL, result);

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

    fn sra_hl(&mut self) -> u8 {
        //shift right arithmetic (b0=0)
        let r = self.read(self.HL);
        let msb = r & 0b1000_0000;
        let lsb = r & 0b0000_0001;

        let result: u8 = (r >> 1) | msb;

        self.write(self.HL, result);

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

    fn rlc_hl(&mut self) -> u8 {
        let r = self.read(self.HL);

        let msb = r & 0b1000_0000;

        let res = (r << 1) | (r >> 7);

        self.write(self.HL, res);

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

    fn rrc_hl(&mut self) -> u8 {
        let r = self.read(self.HL);

        let lsb = r & 0b0000_0001;

        let res = (r >> 1) | (r << 7);

        self.write(self.HL, res);

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

    fn rr_hl(&mut self) -> u8 {
        let r = self.read(self.HL);
        let mut carry = self.get_flag(Flag::C);

        let lsb = r & 1;

        let mut res = r >> 1;
        if carry {
            res |= 0b1000_0000;
        }

        self.write(self.HL, res);
        carry = lsb > 0;

        self.set_flag(Flag::Z, res == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, false);
        self.set_flag(Flag::C, carry);

        return 16;
    }

    fn rl_hl(&mut self) -> u8 {
        let r = self.read(self.HL);
        let mut carry = self.get_flag(Flag::C);

        let msb = r & 0b1000_0000;

        let mut res = r << 1;
        if carry {
            res |= 0b0000_0001;
        }

        self.write(self.HL, res);
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

    fn stop(&mut self) -> u8 {
        self.fetch_byte();

        return 8;
        //low power standby mode???
    }

    fn add_imm(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte();

        let value = a + n;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (n & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value < a);

        return 8;
    }

    fn sub_imm(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let n = self.fetch_byte();

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

    fn adc_a_imm(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.fetch_byte();
        let cy = self.get_flag(Flag::C) as u8;
        let value = a + r + cy;

        self.set_register(&Register::A, value);

        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, ((a & 0xf) + (r & 0xf) + (cy & 0xf)) > 0xF);
        self.set_flag(Flag::C, a as u32 + r as u32 + cy as u32 > 255);

        return 8;
    }

    fn sbc_a_imm(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.fetch_byte();
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

    fn sub_hl(&mut self) -> u8 {
        //A=A-(HL)
        let a = self.get_register(&Register::A);

        let hl = self.read(self.HL);

        let value = a - hl;

        self.set_register(&Register::A, value);

        //Z 1 H C
        self.set_flag(Flag::Z, value == 0);
        self.set_flag(Flag::N, true);
        self.set_flag(Flag::H, ((a & 0xf) - (hl & 0xf)) & 0x10 == 0x10);
        self.set_flag(Flag::C, value > a);

        return 8;
    }

    fn adc_hl(&mut self) -> u8 {
        let a = self.get_register(&Register::A);
        let r = self.read(self.HL);
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

    fn sbc_hl(&mut self) -> u8 {
        //A=A-(HL)-cy
        let a = self.get_register(&Register::A);
        let hl = self.read(self.HL);
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

    fn ld_sp(&mut self) -> u8 {
        //(nn)=SP
        let address = self.fetch_word();
        self.write(address, (self.SP & 0x00FF) as u8);
        self.write(address + 1, ((self.SP & 0xFF00) >> 8) as u8);

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

    fn add_sp(&mut self) -> u8 {
        //SP = SP +/- dd ; dd is 8-bit signed number

        let dd = self.fetch_byte() as i8;

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

    fn bit_hl(&mut self, bit: u8) -> u8 {
        let r = self.read(self.HL);

        self.set_flag(Flag::Z, (r & (1 << bit)) == 0);
        self.set_flag(Flag::N, false);
        self.set_flag(Flag::H, true);

        return 16;
    }

    //TODO: work out a way to get this into the PPU and out of the CPU - via common MMU (do before APU)
    //TODO: only render what's in the 160x144 viewport because this is S L O W
    //      also optimise this bad boy because it's called 256 times per FRAME
    fn scanline(&mut self) {
        let last_scanline = self.read(0xFF44);

        let scanline = if last_scanline == 153 {
            0
        } else {
            last_scanline + 1
        };
        self.write(0xFF44, scanline);

        if scanline == 144 {
            self.set_interrupt_flag(Interrupt::VBlank, true);
            //update stat register
            self.ram[0xFF41 - RAM_START as usize] |= 1;
            if self.enable_logging {
                println!("sent VBlank interrupt");
            }
        }

        let ly = self.read(0xFF44);
        let lyc = self.read(0xFF45);
        if ly == lyc {
            //set stat register flag
            self.set_interrupt_flag(Interrupt::LCD_STAT, true);
            self.ram[0xFF41 - RAM_START as usize] = 0b01000000;
        } else {
            self.ram[0xFF41 - RAM_START as usize] = 0b00000000;
        }

        let lcdc = self.read(0xFF40);
        let addressing_mode = if (get_bit(lcdc, 4)) == 0 {
            AddressingMode::Mode8800
        } else {
            AddressingMode::Mode8000
        };

        //background
        let row = scanline / 0x08;
        let address_offset = row as u16 * 0x20;
        let mut tile_id = address_offset;
        let y = scanline % 8;

        let bg_tile_data_area_start = match get_bit(lcdc, 3) > 0 {
            false => 0x9800 + address_offset,
            true => 0x9C00 + address_offset,
        };
        let bg_tile_data_area_end = bg_tile_data_area_start + 0x20;

        let mut scroll_x = self.read(0xFF43);

        //So each scanline is a row of pixels not tiles
        //for a given scanline, only retrieve one row's tiles and only render one row of that tile

        for address in bg_tile_data_area_start..bg_tile_data_area_end {
            let tile_idx = self.read(address);

            //The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX,
            //which are only read at the beginning of the scanline (for the initial shifting of pixels).
            let scroll_y = self.read(0xFF42);
            scroll_x = (scroll_x & 0b0000_0111) | (self.read(0xFF43) & 0b1111_1000);

            let tile = self.get_tile_row(tile_idx, y, &addressing_mode);

            let tile_x = ((tile_id % 0x20) * 0x08) as u8;
            let tile_y = ((tile_id / 0x20) * 0x08) as u8;

            let pixel_y = tile_y + y - scroll_y;

            for x in 0..8 {
                let pixel = tile[x as usize];

                let pixel_x = tile_x + x - scroll_x;

                let color = match pixel {
                    0b00 => self.screen.palette.black,
                    0b01 => self.screen.palette.dark,
                    0b10 => self.screen.palette.light,
                    0b11 => self.screen.palette.white,
                    _ => panic!("Unexpected pixel value {pixel}"),
                };

                let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                self.screen.pixels[offset] = color;
            }
            tile_id += 1
        }

        //window
        let display_window = get_bit(lcdc, 5) > 0 && get_bit(lcdc, 0) > 0;
        if display_window {
            let window_tile_data_area = match get_bit(lcdc, 6) > 0 {
                true => 0x9C00..=0x9FFF,
                false => 0x9800..=0x9BFF,
            };

            let wy = self.read(0xFF4A);
            let wx = self.read(0xFF4B) - 7;

            for address in window_tile_data_area {
                let tile_idx = self.read(address);

                let tile = self.get_tile_row(tile_idx, y, &addressing_mode);

                let tile_x = ((tile_id % 0x20) * 0x08) as u8;
                let tile_y = ((tile_id / 0x20) * 0x08) as u8;

                for x in 0..8 {
                    let y = scanline % 8;
                    let pixel = tile[x as usize];

                    let pixel_y = tile_y + y + wy;
                    let pixel_x = tile_x + x + wx;

                    let color = match pixel {
                        0b00 => self.screen.palette.black,
                        0b01 => self.screen.palette.dark,
                        0b10 => self.screen.palette.light,
                        0b11 => self.screen.palette.white,
                        _ => panic!("Unexpected pixel value {pixel}"),
                    };
                    let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                    self.screen.pixels[offset] = color;
                }
                tile_id += 1
            }
        }

        //sprites
        let obj_enable = get_bit(lcdc, 1) > 0;
        if obj_enable {
            self.draw_sprites(lcdc, y);
        }
    }

    fn get_frame(&self) -> [u32; 256 * 256] {
        return self.screen.pixels;
    }

    fn draw_sprites(
        //what a dog's dinner of arguments
        &mut self,
        lcdc: u8,
        mut y: u8,
    ) {
        let oam_address = 0xFE00;
        for sprite_idx in 0..=40 {
            let y_pos = self.read(oam_address + (4 * sprite_idx) + 0);
            let x_pos = self.read(oam_address + (4 * sprite_idx) + 1);
            let t_idx = self.read(oam_address + (4 * sprite_idx) + 2);
            let attrs = self.read(oam_address + (4 * sprite_idx) + 3);

            let mode_16x8 = (lcdc & 0b0000_0100) > 0;
            // if mode_16x8 {
            //     panic!("16x8 mode is not yet implemented");
            // }

            let tile = self.get_tile_row(t_idx, y, &AddressingMode::Mode8000);

            let y_flip = (attrs & 0b0100_0000) > 0;
            if y_flip {
                y = 7 - y;
            }
            let x_flip = (attrs & 0b0010_0000) > 0;

            //todo: there is probably a much smarter and more efficient way to do this using sdl, I just do not know how yet
            for x in 0..8 {
                let pixel_x = (x_pos + x) as i32 - 8; //X = Object’s horizontal position on the screen + 8
                let pixel_y = (y_pos + y) as i32 - 16; //Y = Object’s vertical position on the screen + 16

                if pixel_x < 0 || pixel_y < 0 {
                    continue; //pixel is off the screen, no need to draw
                }

                let tile_x = if x_flip { 7 - x } else { x };

                let pixel = tile[tile_x as usize];

                if pixel == 0b00 {
                    continue; //pixel is transparent, no need to draw
                }

                let color = match pixel {
                    0b01 => self.screen.palette.dark,
                    0b10 => self.screen.palette.light,
                    0b11 => self.screen.palette.white,
                    _ => panic!("Unexpected pixel value {pixel}"),
                };
                let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                self.screen.pixels[offset] = color;
            }
        }
    }

    fn get_tile_row(&self, tile_idx: u8, y: u8, addressing_mode: &AddressingMode) -> [u8; 8] {
        let root_address = match addressing_mode {
            AddressingMode::Mode8000 => 0x8000 + (tile_idx as u16 * 0x10),
            AddressingMode::Mode8800 => 0x9000 + (tile_idx as i8 as u16 * 0x10),
        };

        let mut tile = [0; 8];

        let mut pixel_x = 0;
        let byte = (y as u16) * 2;
        let lo = self.read(root_address + byte);
        let hi = self.read(root_address + byte + 1);

        for bit in (0..8).rev() {
            let pixel = (get_bit(hi, bit) << 1) | get_bit(lo, bit);

            tile[pixel_x] = pixel;

            pixel_x += 1;
        }

        return tile;
    }
}

extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use std::time::SystemTime;

fn save_state(cpu: &CPU) {}

fn load_state() -> CPU {
    return CPU {
        enable_logging: todo!(),
        joypad: todo!(),
        screen: todo!(),
        halted: todo!(),
        ime: todo!(),
        remaining_cycles: todo!(),
        clock_cycles: todo!(),
        AF: todo!(),
        BC: todo!(),
        DE: todo!(),
        HL: todo!(),
        SP: todo!(),
        PC: todo!(),
        rom: todo!(),
        ram: todo!(),
    };
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let bytes = std::fs::read(file).unwrap();

    let cart = Cart::load(bytes);
    let name = cart.name;

    println!("loaded cart into memory");

    let sdl_context = sdl2::init()?;
    let video_subsystem = sdl_context.video()?;

    let scale = 4;

    let width = 160 * scale;
    let height = 144 * scale;

    let window = video_subsystem
        .window("rgb", width, height)
        .position_centered()
        .opengl()
        .build()
        .map_err(|e| e.to_string())?;

    let mut canvas = window.into_canvas().build().map_err(|e| e.to_string())?;

    let texture_creator = canvas.texture_creator();

    let mut texture = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGB24, 256, 256)
        .map_err(|e| e.to_string())?;

    let mut event_pump = sdl_context.event_pump()?;

    //TODO: since I treat the CPU as the gameboy, maybe make a gameboy struct that own a CPU? then the api will make more sense

    // let palette = Palette {
    //     black: 0xfff4b8FF,
    //     dark: 0xff8b40FF,
    //     light: 0xa22fc9FF,
    //     white: 0x290143FF,
    // };
    // let palette = Palette {
    //     black: 0x1a2129ff,
    //     dark: 0x312137FF,
    //     light: 0x943054FF,
    //     white: 0xf5a1a1FF,
    // };
    let palette: Palette = Palette {
        black: 0xE0F8D0FF,
        dark: 0x88c070FF,
        light: 0x346856FF,
        white: 0x081820FF,
    };
    let mut cpu = CPU {
        enable_logging: false,
        joypad: Joypad::new(),
        screen: Screen::new(palette),
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
        rom: cart.rom,
        ram: [0; 0x8000],
    };
    cpu.ram[0xFF00 - RAM_START as usize] = 0x0F; //joypad
    cpu.ram[0xFF40 - RAM_START as usize] = 0x91; //LCDC
    cpu.ram[0xFFFF - RAM_START as usize] = 1; //ie

    const TARGET_FPS: f64 = 59.7;
    const NANOS_PER_FRAME: u128 = (1_000_000_000f64 / TARGET_FPS) as u128;
    const TICKS_PER_SCANLINE: u64 = 456;
    const SCANLINES_PER_FRAME: u64 = 153;

    let mut frame_count = 0;
    let mut limit = true;

    let spin_sleeper = spin_sleep::SpinSleeper::new(100_000)
        .with_spin_strategy(spin_sleep::SpinStrategy::YieldThread);

    loop {
        let mut scanline: u64 = 0;

        //clear stat register VBlank
        cpu.ram[0xFF41 - RAM_START as usize] &= !1;

        //Timer stuff
        let mut ticks = 0;

        let now = SystemTime::now();

        while scanline <= SCANLINES_PER_FRAME {
            for _ in 0..=TICKS_PER_SCANLINE {
                cpu.tick(ticks);
                ticks += 1;
            }

            cpu.scanline(); //TODO: This is a huge performance hit, optimise this call
            scanline += 1;
        }
        cpu.set_interrupt_flag(Interrupt::VBlank, false);
        frame_count += 1;

        let frame = cpu.get_frame();
        texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
            for y in 0..256 {
                for x in 0..256 {
                    let offset = y * pitch + x * 3;
                    let pixel = frame[y * 256 + x];
                    buffer[offset + 0] = ((pixel & 0xFF000000) >> 24) as u8;
                    buffer[offset + 1] = ((pixel & 0x00FF0000) >> 16) as u8;
                    buffer[offset + 2] = ((pixel & 0x0000FF00) >> 8) as u8;
                }
            }
        })?;

        canvas.clear();
        canvas.copy(&texture, Some(Rect::new(0, 0, 160, 144)), None)?;
        canvas.present();

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit { .. } => return Ok(()),
                Event::KeyDown {
                    keycode: Some(Keycode::Space),
                    ..
                } => limit = false,
                Event::KeyDown {
                    keycode: Some(Keycode::F1),
                    ..
                } => cpu.enable_logging = !cpu.enable_logging,

                //TODO: Match based on a key mapping, not hard coded keys
                //Key pressed
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Right);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Left),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Left);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Up),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Up);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Down);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::X),
                    ..
                } => {
                    cpu.press_button(JoypadButton::A);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Z),
                    ..
                } => {
                    cpu.press_button(JoypadButton::B);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::LShift),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Select);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    cpu.press_button(JoypadButton::Start);
                }
                //Key released
                Event::KeyUp {
                    keycode: Some(Keycode::Space),
                    ..
                } => limit = true,
                Event::KeyUp {
                    keycode: Some(Keycode::Right),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Right);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Left),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Left);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Up),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Up);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Down);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::X),
                    ..
                } => {
                    cpu.release_button(JoypadButton::A);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Z),
                    ..
                } => {
                    cpu.release_button(JoypadButton::B);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::LShift),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Select);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    cpu.release_button(JoypadButton::Start);
                }
                _ => {}
            }
        }

        match now.elapsed() {
            Ok(elapsed) => {
                if elapsed.as_nanos() < NANOS_PER_FRAME {
                    let remainder = (NANOS_PER_FRAME - elapsed.as_nanos()) as u64;

                    let before = SystemTime::now();
                    if limit {
                        spin_sleeper.sleep_ns(remainder);
                    }

                    let actual = before.elapsed().unwrap().as_nanos() as u64;

                    let percent = 100.0f64 * NANOS_PER_FRAME as f64
                        / (elapsed.as_nanos() as u64 + actual) as f64;
                    let window = canvas.window_mut();
                    window
                        .set_title(
                            format!("{} | {:3.0}% | frame {frame_count}", name, percent).as_str(),
                        )
                        .map_err(|e| e.to_string())?;
                }
            }
            Err(e) => {
                // an error occurred!
                println!("Error: {e:?}");
            }
        }
        //exit(9);
    }

    Ok(())
}

//Unit Tests - why does rust expect them to be mingled with the business code?
#[cfg(test)]
mod test {
    use serde::{Deserialize, Serialize};
    use serde_json::Result;

    use crate::{Cart, Flag, Joypad, Register, CPU, RAM_START};

    #[derive(Serialize, Deserialize, Debug)]
    #[serde(transparent)]
    struct Hex {
        #[serde(with = "hex::serde")]
        hex: Vec<u8>,
    }

    #[derive(Debug, Deserialize)]
    struct CpuState {
        a: String,
        b: String,
        c: String,
        d: String,
        e: String,
        f: String,
        h: String,
        l: String,
        pc: String,
        sp: String,
    }

    #[derive(Debug, Deserialize)]
    struct SystemState {
        cpu: CpuState,
        ram: Vec<Vec<String>>,
    }

    #[derive(Debug, Deserialize)]
    struct TestCase {
        name: String,
        initial: SystemState,
        r#final: SystemState,
        cycles: Vec<Option<Vec<String>>>,
    }

    #[test]
    fn cpu_0x00() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/00.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x01() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/01.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x02() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/02.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x03() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/03.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x04() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/04.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x05() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/05.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x06() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/06.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x07() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/07.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x08() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/08.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x09() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/09.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x0F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/0F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x10() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/10.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x11() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/11.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x12() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/12.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x13() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/13.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x14() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/14.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x15() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/15.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x16() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/16.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x17() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/17.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x18() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/18.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x19() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/19.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x1F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/1F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x20() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/20.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x21() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/21.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x22() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/22.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x23() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/23.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x24() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/24.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x25() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/25.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x26() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/26.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x27() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/27.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x28() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/28.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x29() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/29.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x2F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/2F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x30() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/30.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x31() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/31.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x32() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/32.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x33() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/33.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x34() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/34.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x35() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/35.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x36() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/36.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x37() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/37.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x38() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/38.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x39() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/39.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x3F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/3F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x40() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/40.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x41() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/41.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x42() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/42.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x43() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/43.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x44() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/44.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x45() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/45.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x46() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/46.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x47() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/47.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x48() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/48.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x49() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/49.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x4F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/4F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x50() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/50.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x51() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/51.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x52() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/52.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x53() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/53.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x54() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/54.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x55() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/55.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x56() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/56.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x57() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/57.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x58() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/58.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x59() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/59.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x5F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/5F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x60() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/60.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x61() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/61.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x62() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/62.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x63() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/63.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x64() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/64.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x65() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/65.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x66() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/66.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x67() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/67.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x68() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/68.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x69() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/69.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x6F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/6F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x70() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/70.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x71() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/71.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x72() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/72.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x73() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/73.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x74() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/74.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x75() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/75.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x76() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/76.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x77() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/77.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x78() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/78.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x79() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/79.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x7F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/7F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x80() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/80.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x81() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/81.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x82() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/82.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x83() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/83.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x84() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/84.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x85() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/85.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x86() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/86.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x87() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/87.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x88() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/88.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x89() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/89.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x8F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/8F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x90() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/90.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x91() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/91.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x92() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/92.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x93() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/93.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x94() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/94.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x95() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/95.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x96() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/96.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x97() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/97.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x98() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/98.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x99() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/99.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9A() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9A.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9B() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9B.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9C() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9C.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9D() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9D.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9E() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9E.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0x9F() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/9F.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA3() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A3.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA4() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A4.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xA9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/A9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAB() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AB.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAC() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AC.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAD() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AD.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xAF() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/AF.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB3() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B3.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB4() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B4.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xB9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/B9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBB() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BB.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBC() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BC.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBD() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BD.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xBF() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/BF.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC3() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C3.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC4() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C4.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xC9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/C9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCB() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CB.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCC() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CC.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCD() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CD.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xCF() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/CF.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD4() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D4.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xD9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/D9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xDA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/DA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xDC() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/DC.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xDE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/DE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xDF() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/DF.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xE9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/E9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xEA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/EA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xEE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/EE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xEF() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/EF.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF0() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F0.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF1() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F1.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF2() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F2.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF3() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F3.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF5() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F5.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF6() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F6.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF7() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F7.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF8() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F8.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xF9() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/F9.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xFA() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/FA.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xFB() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/FB.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    #[test]
    fn cpu_0xFE() -> Result<()> {
        let data =
            std::fs::read_to_string("resources/gameboy-test-data-master/cpu_tests/v1/FE.json")
                .expect("could not read json");
        let test_cases: Vec<TestCase> = serde_json::from_str(data.as_str())?;

        run_test_cases(test_cases);
        Ok(())
    }

    fn run_test_cases(test_cases: Vec<TestCase>) {
        for test_case in test_cases {
            println!("Running test case: {}", test_case.name);

            let initial = test_case.initial;

            let mut cpu = CPU {
                enable_logging: false,
                joypad: Joypad::new(),
                halted: false,
                ime: false,
                AF: 0x0000,
                BC: 0x0000,
                DE: 0x0000,
                HL: 0x0000,
                SP: 0x0000,
                PC: 0x0000,
                clock_cycles: 0,
                remaining_cycles: 0,
                rom: vec![0; 0x8000],
                ram: [0; 0x8000],
            };

            //CPU
            cpu.set_register(&Register::A, to_hex_u8(&initial.cpu.a));
            cpu.set_register(&Register::B, to_hex_u8(&initial.cpu.b));
            cpu.set_register(&Register::C, to_hex_u8(&initial.cpu.c));
            cpu.set_register(&Register::D, to_hex_u8(&initial.cpu.d));
            cpu.set_register(&Register::E, to_hex_u8(&initial.cpu.e));
            cpu.set_register(&Register::F, to_hex_u8(&initial.cpu.f));
            cpu.set_register(&Register::H, to_hex_u8(&initial.cpu.h));
            cpu.set_register(&Register::L, to_hex_u8(&initial.cpu.l));

            cpu.PC = to_hex_u16(&initial.cpu.pc);
            cpu.SP = to_hex_u16(&initial.cpu.sp);

            //Memory
            for ram in initial.ram {
                let address = to_hex_u16(&ram[0]);
                let value = to_hex_u8(&ram[1]);

                if address < RAM_START {
                    cpu.rom[address as usize] = value;
                } else {
                    cpu.ram[(address - RAM_START) as usize] = value;
                }
            }

            //run cycles
            let expected_clock_cycles =
                4 * test_case.cycles.iter().filter(|&el| el.is_some()).count() as u64;

            let mut clock_cycles = 0;
            while clock_cycles < expected_clock_cycles {
                clock_cycles += cpu.next_op() as u64;
            }

            //compare final state
            let expected_state = test_case.r#final;
            compare_register(&cpu, &Register::A, to_hex_u8(&expected_state.cpu.a));
            compare_register(&cpu, &Register::B, to_hex_u8(&expected_state.cpu.b));
            compare_register(&cpu, &Register::C, to_hex_u8(&expected_state.cpu.c));
            compare_register(&cpu, &Register::D, to_hex_u8(&expected_state.cpu.d));
            compare_register(&cpu, &Register::E, to_hex_u8(&expected_state.cpu.e));
            compare_flags(&cpu, to_hex_u8(&expected_state.cpu.f));
            compare_register(&cpu, &Register::H, to_hex_u8(&expected_state.cpu.h));
            compare_register(&cpu, &Register::L, to_hex_u8(&expected_state.cpu.l));

            assert_eq!(
                to_hex_u16(&expected_state.cpu.pc),
                cpu.PC,
                "PC mismatch, expected 0x{:04X} but got 0x{:04X}",
                to_hex_u16(&expected_state.cpu.pc),
                cpu.PC
            );
            assert_eq!(
                to_hex_u16(&expected_state.cpu.sp),
                cpu.SP,
                "SP mismatch, expected 0x{:04X} but got 0x{:04X}",
                to_hex_u16(&expected_state.cpu.sp),
                cpu.SP
            );

            for ram in expected_state.ram {
                let address = to_hex_u16(&ram[0]);
                let expected = to_hex_u8(&ram[1]);

                let actual = cpu.read(address);

                assert_eq!(
                    expected, actual,
                    "ram mismatch, expected 0x{:04X} at 0x{:04X} but got 0x{:04X}",
                    expected, address, actual
                );
            }
        }
    }

    fn compare_flags(cpu: &CPU, expected: u8) {
        let actual = cpu.get_register(&Register::F);
        assert_eq!(
            expected,
            actual,
            "Flags mismatch, expected {}{}{}{} but got {}{}{}{} ({:08b}|{:08b})",
            if (expected & 0b1000_0000) > 0 {
                'z'
            } else {
                '-'
            },
            if (expected & 0b0100_0000) > 0 {
                'n'
            } else {
                '-'
            },
            if (expected & 0b0010_0000) > 0 {
                'h'
            } else {
                '-'
            },
            if (expected & 0b0001_0000) > 0 {
                'c'
            } else {
                '-'
            },
            if cpu.get_flag(Flag::Z) { 'z' } else { '-' },
            if cpu.get_flag(Flag::N) { 'n' } else { '-' },
            if cpu.get_flag(Flag::H) { 'h' } else { '-' },
            if cpu.get_flag(Flag::C) { 'c' } else { '-' },
            expected,
            actual
        );
    }

    fn compare_register(cpu: &CPU, register: &Register, expected: u8) {
        let actual = cpu.get_register(register);
        assert_eq!(
            expected, actual,
            "Register {:?} mismatch, expected 0x{:02X} but got 0x{:02X}",
            register, expected, actual
        );
    }

    fn to_hex_u8(prefixed_hex_string: &String) -> u8 {
        let without_prefix = prefixed_hex_string.trim_start_matches("0x");
        return u8::from_str_radix(without_prefix, 16).unwrap();
    }

    fn to_hex_u16(prefixed_hex_string: &String) -> u16 {
        let without_prefix = prefixed_hex_string.trim_start_matches("0x");
        return u16::from_str_radix(without_prefix, 16).unwrap();
    }
}
