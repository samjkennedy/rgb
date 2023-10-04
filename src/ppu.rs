use crate::{
    cpu::Interrupt,
    mmu::{get_bit, MMU, RAM_START},
};

pub struct Palette {
    pub black: u32,
    pub dark: u32,
    pub light: u32,
    pub white: u32,
}

#[derive(Debug)]
enum AddressingMode {
    Mode8000,
    Mode8800,
}

pub struct PPU {
    palette: Palette, //TODO: should allow hot swapping of palettes
    pixels: [u32; 256 * 256],
}

impl PPU {
    pub fn new(palette: Palette) -> PPU {
        return PPU {
            palette: palette,
            pixels: [0; 256 * 256],
        };
    }
    //TODO: only render what's in the 160x144 viewport because this is S L O W
    //      also optimise this bad boy because it's called 256 times per FRAME
    pub fn scanline(&mut self, mmu: &mut MMU) {
        let last_scanline = mmu.read(0xFF44);

        let scanline = if last_scanline == 153 {
            0
        } else {
            last_scanline + 1
        };
        mmu.write(0xFF44, scanline);

        if scanline == 144 {
            mmu.set_interrupt_flag(Interrupt::VBlank, true);
            //update stat register
            mmu.ram[0xFF41 - RAM_START as usize] |= 1;
        }

        let ly = mmu.read(0xFF44);
        let lyc = mmu.read(0xFF45);
        if ly == lyc {
            //set stat register flag
            mmu.set_interrupt_flag(Interrupt::LCD_STAT, true);
            mmu.ram[0xFF41 - RAM_START as usize] = 0b01000000;
        } else {
            mmu.ram[0xFF41 - RAM_START as usize] = 0b00000000;
        }

        let lcdc = mmu.read(0xFF40);
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

        let mut scroll_x = mmu.read(0xFF43);

        //So each scanline is a row of pixels not tiles
        //for a given scanline, only retrieve one row's tiles and only render one row of that tile

        for address in bg_tile_data_area_start..bg_tile_data_area_end {
            let tile_idx = mmu.read(address);

            //The scroll registers are re-read on each tile fetch, except for the low 3 bits of SCX,
            //which are only read at the beginning of the scanline (for the initial shifting of pixels).
            let scroll_y = mmu.read(0xFF42);
            scroll_x = (scroll_x & 0b0000_0111) | (mmu.read(0xFF43) & 0b1111_1000);

            let tile = self.get_tile_row(tile_idx, y, &addressing_mode, mmu);

            let tile_x = ((tile_id % 0x20) * 0x08) as u8;
            let tile_y = ((tile_id / 0x20) * 0x08) as u8;

            let pixel_y = tile_y + y - scroll_y;

            for x in 0..8 {
                let pixel = tile[x as usize];

                let pixel_x = tile_x + x - scroll_x;

                let color = match pixel {
                    0b00 => self.palette.black,
                    0b01 => self.palette.dark,
                    0b10 => self.palette.light,
                    0b11 => self.palette.white,
                    _ => panic!("Unexpected pixel value {pixel}"),
                };

                let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                self.pixels[offset] = color;
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

            let wy = mmu.read(0xFF4A);
            let wx = mmu.read(0xFF4B) - 7;

            for address in window_tile_data_area {
                let tile_idx = mmu.read(address);

                let tile = self.get_tile_row(tile_idx, y, &addressing_mode, mmu);

                let tile_x = ((tile_id % 0x20) * 0x08) as u8;
                let tile_y = ((tile_id / 0x20) * 0x08) as u8;

                for x in 0..8 {
                    let y = scanline % 8;
                    let pixel = tile[x as usize];

                    let pixel_y = tile_y + y + wy;
                    let pixel_x = tile_x + x + wx;

                    let color = match pixel {
                        0b00 => self.palette.black,
                        0b01 => self.palette.dark,
                        0b10 => self.palette.light,
                        0b11 => self.palette.white,
                        _ => panic!("Unexpected pixel value {pixel}"),
                    };
                    let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                    self.pixels[offset] = color;
                }
                tile_id += 1
            }
        }

        //sprites
        let obj_enable = get_bit(lcdc, 1) > 0;
        if obj_enable {
            self.draw_sprites(lcdc, y, mmu);
        }
    }

    pub fn get_frame(&self) -> &[u32; 256 * 256] {
        return &self.pixels;
    }

    fn draw_sprites(&mut self, lcdc: u8, mut y: u8, mmu: &MMU) {
        let oam_address = 0xFE00;
        for sprite_idx in 0..=40 {
            let y_pos = mmu.read(oam_address + (4 * sprite_idx) + 0);
            let x_pos = mmu.read(oam_address + (4 * sprite_idx) + 1);
            let t_idx = mmu.read(oam_address + (4 * sprite_idx) + 2);
            let attrs = mmu.read(oam_address + (4 * sprite_idx) + 3);

            let mode_16x8 = (lcdc & 0b0000_0100) > 0;
            if mode_16x8 {
                panic!("16x8 mode is not yet implemented");
            }

            let tile = self.get_tile_row(t_idx, y, &AddressingMode::Mode8000, mmu);

            let y_flip = (attrs & 0b0100_0000) > 0;
            if y_flip {
                y = 7 - y;
            }
            let x_flip = (attrs & 0b0010_0000) > 0;

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
                    0b01 => self.palette.dark,
                    0b10 => self.palette.light,
                    0b11 => self.palette.white,
                    _ => panic!("Unexpected pixel value {pixel}"),
                };
                let offset = (pixel_y as usize * 256) + (pixel_x as usize);
                self.pixels[offset] = color;
            }
        }
    }

    fn get_tile_row(
        &self,
        tile_idx: u8,
        y: u8,
        addressing_mode: &AddressingMode,
        mmu: &MMU,
    ) -> [u8; 8] {
        let root_address = match addressing_mode {
            AddressingMode::Mode8000 => 0x8000 + (tile_idx as u16 * 0x10),
            AddressingMode::Mode8800 => 0x9000 + (tile_idx as i8 as u16 * 0x10),
        };

        let mut tile = [0; 8];

        let mut pixel_x = 0;
        let byte = (y as u16) * 2;
        let lo = mmu.read(root_address + byte);
        let hi = mmu.read(root_address + byte + 1);

        for bit in (0..8).rev() {
            let pixel = (get_bit(hi, bit) << 1) | get_bit(lo, bit);

            tile[pixel_x] = pixel;

            pixel_x += 1;
        }

        return tile;
    }
}
