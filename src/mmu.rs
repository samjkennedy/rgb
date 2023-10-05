use crate::{
    cpu::Interrupt,
    joypad::{ButtonMode, Joypad, JoypadButton},
};

pub struct MMU {
    joypad: Joypad,
    pub rom: Vec<u8>,
    pub ram: [u8; 0x8000], //TODO: maybe split this into multiple arrays named by memory region?
}

pub const RAM_START: u16 = 0x8000;

pub fn get_bit(byte: u8, bit: u8) -> u8 {
    return if byte & (1 << bit) > 0 { 1 } else { 0 };
}

pub fn is_bit(byte: u8, bit: u8) -> bool {
    return byte & (1 << bit) > 0;
}

impl MMU {
    pub fn new(rom: Vec<u8>, joypad: Joypad) -> MMU {
        let mut ram: [u8; 0x8000] = [0; 0x8000];

        ram[0xFF00 - RAM_START as usize] = 0x0F; //joypad
        ram[0xFF40 - RAM_START as usize] = 0x91; //LCDC
        ram[0xFFFF - RAM_START as usize] = 1; //ie

        return MMU {
            joypad: joypad,
            rom: rom,
            ram: ram,
        };
    }

    pub fn read(&self, address: u16) -> u8 {
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

    pub fn write(&mut self, address: u16, value: u8) {
        if address < RAM_START {
            return;
        }
        let ram_address = (address - RAM_START) as usize;
        match address {
            0xE000..=0xFDFF => {
                //mirror ram, do not write
            }
            0xFEA0..=0xFEFF => {
                //invalid memory region
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

    pub fn press_button(&mut self, button: JoypadButton) {
        let joypad_state = self.read(0xFF00);

        let button_ord = button as u8;
        self.joypad.state |= 1 << button_ord;

        //should only fire an interrupt if the input is selected by bits 5/6
        let select = joypad_state ^ 0b0011_0000;
        let select_bit = ((button_ord / 4) + 1) << 4;

        if (select & select_bit) > 0 {
            self.set_interrupt_flag(Interrupt::Joypad, true)
        }
    }

    pub fn release_button(&mut self, button: JoypadButton) {
        let button_ord = button as u8;
        self.joypad.state &= !(1 << button_ord);
    }

    pub fn interrupt_enabled(&self, interrupt: Interrupt) -> bool {
        let ie = self.read(0xFFFF);
        let interrupt_mask = 1 << interrupt as u8;

        return (ie & interrupt_mask) > 0;
    }

    pub fn set_interrupt_flag(&mut self, interrupt: Interrupt, set: bool) {
        let interrupt_mask = 1 << interrupt as u8;
        let if_interrupt_flags = self.read(0xFF0F);

        if set {
            self.ram[0xFF0F - RAM_START as usize] = if_interrupt_flags | interrupt_mask;
        } else {
            self.ram[0xFF0F - RAM_START as usize] = if_interrupt_flags & !interrupt_mask;
        }
    }

    pub fn get_interrupt_flag(&self, interrupt: Interrupt) -> bool {
        let interrupt_mask = 1 << interrupt as u8;
        return (self.read(0xFF0F) & interrupt_mask) > 0;
    }

    fn dma_transfer(&mut self, lower_byte: u8) {
        //TODO: this may need to be smarter with timings and memory access restrictions
        //better to implement as a subroutine tbh and jump to it?
        for offset in 0x00..=0x9F {
            let source_address = lower_byte as u16 * 0x100 + offset;
            let dest_address = 0xFE00 + offset;

            self.write(dest_address, self.read(source_address));
        }
    }
}
