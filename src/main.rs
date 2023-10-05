#![allow(arithmetic_overflow)]
use std::collections::HashMap;
use std::env;
use std::time::SystemTime;

mod cart;
mod cpu;
mod joypad;
mod mmu;
mod ppu;

extern crate sdl2;

use cpu::CPU;
use joypad::Joypad;

//sdl
use sdl2::controller::{Axis, Button};
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;

use crate::cart::Cart;
use crate::cpu::Interrupt;
use crate::joypad::JoypadButton;
use crate::mmu::{MMU, RAM_START};
use crate::ppu::{Palette, PPU};

struct ControlsMap {
    keyboard_mapping: HashMap<Keycode, JoypadButton>,
    controller_mapping: HashMap<Button, JoypadButton>,
}

impl ControlsMap {
    //TODO: eventually read these from a settings file
    fn new() -> ControlsMap {
        let mut keyboard_mapping = HashMap::new();

        keyboard_mapping.insert(Keycode::Right, JoypadButton::Right);
        keyboard_mapping.insert(Keycode::Left, JoypadButton::Left);
        keyboard_mapping.insert(Keycode::Up, JoypadButton::Up);
        keyboard_mapping.insert(Keycode::Down, JoypadButton::Down);

        keyboard_mapping.insert(Keycode::X, JoypadButton::A);
        keyboard_mapping.insert(Keycode::Z, JoypadButton::B);
        keyboard_mapping.insert(Keycode::LShift, JoypadButton::Select);
        keyboard_mapping.insert(Keycode::Return, JoypadButton::Start);

        let mut controller_mapping = HashMap::new();

        //TODO: stick configuration?

        controller_mapping.insert(Button::DPadRight, JoypadButton::Right);
        controller_mapping.insert(Button::DPadLeft, JoypadButton::Left);
        controller_mapping.insert(Button::DPadUp, JoypadButton::Up);
        controller_mapping.insert(Button::DPadDown, JoypadButton::Down);

        controller_mapping.insert(Button::A, JoypadButton::A);
        controller_mapping.insert(Button::B, JoypadButton::B);
        controller_mapping.insert(Button::Start, JoypadButton::Start);
        controller_mapping.insert(Button::Back, JoypadButton::Select);

        return ControlsMap {
            keyboard_mapping,
            controller_mapping,
        };
    }

    fn get_keyboard_button(&self, keycode: Keycode) -> Option<&JoypadButton> {
        return self.keyboard_mapping.get(&keycode);
    }

    fn get_controller_button(&self, button: Button) -> Option<&JoypadButton> {
        return self.controller_mapping.get(&button);
    }
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let file = &args[1];
    let bytes = std::fs::read(file).unwrap();

    let cart = Cart::load(bytes);
    let name = cart.name;

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

    let joypad = Joypad::new();

    //TODO: handle when no controllers detected
    let controls_map = ControlsMap::new();

    let game_controller_subsystem = sdl_context.game_controller()?;

    let available = game_controller_subsystem
        .num_joysticks()
        .map_err(|e| format!("can't enumerate joysticks: {}", e))?;

    // Iterate over all available joysticks and look for game controllers.
    let controller = (0..available).find_map(|id| {
        if !game_controller_subsystem.is_game_controller(id) {
            println!("{} is not a game controller", id);
            return None;
        }

        match game_controller_subsystem.open(id) {
            Ok(c) => {
                // We managed to find and open a game controller,
                // exit the loop
                Some(c)
            }
            Err(e) => {
                println!("failed: {:?}", e);
                None
            }
        }
    });

    let mut cpu = CPU::new();
    let mut mmu = MMU::new(cart.rom, joypad);
    let mut ppu = PPU::new(palette);

    const TARGET_FPS: f64 = 59.7;
    const NANOS_PER_FRAME: u128 = (1_000_000_000f64 / TARGET_FPS) as u128;
    const TICKS_PER_SCANLINE: u64 = 456;
    const SCANLINES_PER_FRAME: u64 = 153;

    let mut frame_count = 0;
    let mut limit = true;

    //let mut drift = 0; //how much we're off each frame

    let spin_sleeper = spin_sleep::SpinSleeper::new(100_000)
        .with_spin_strategy(spin_sleep::SpinStrategy::YieldThread);

    loop {
        let mut scanline: u64 = 0;

        //clear stat register VBlank
        mmu.ram[0xFF41 - RAM_START as usize] &= !1;

        //Timer stuff
        let mut ticks = 0;

        let now = SystemTime::now();

        while scanline <= SCANLINES_PER_FRAME {
            for _ in 0..=TICKS_PER_SCANLINE {
                cpu.tick(&mut mmu, ticks);
                ticks += 1;
            }
            ppu.scanline(&mut mmu);
            scanline += 1;
        }
        mmu.set_interrupt_flag(Interrupt::VBlank, false);
        frame_count += 1;

        texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
            let frame = ppu.get_frame();
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
                Event::KeyUp {
                    keycode: Some(Keycode::Space),
                    ..
                } => limit = true,

                Event::KeyDown { keycode, .. } => match keycode {
                    Some(code) => match controls_map.get_keyboard_button(code) {
                        Some(joypad_button) => mmu.press_button(joypad_button),
                        None => {}
                    },
                    None => {}
                },
                Event::KeyUp { keycode, .. } => match keycode {
                    Some(code) => match controls_map.get_keyboard_button(code) {
                        Some(joypad_button) => mmu.release_button(joypad_button),
                        None => {}
                    },
                    None => {}
                },

                //TODO: This isn't configurable
                Event::ControllerAxisMotion { axis, value, .. } => {
                    let dead_zone = 10_000; //TODO: configurable

                    if value > dead_zone || value < -dead_zone {
                        match axis {
                            Axis::LeftX => {
                                if value > 0 {
                                    mmu.press_button(&JoypadButton::Right);
                                } else {
                                    mmu.press_button(&JoypadButton::Left);
                                }
                            }
                            Axis::LeftY => {
                                if value > 0 {
                                    mmu.press_button(&JoypadButton::Down);
                                } else {
                                    mmu.press_button(&JoypadButton::Up);
                                }
                            }
                            _ => {}
                        }
                    } else {
                        mmu.release_button(&JoypadButton::Right);
                        mmu.release_button(&JoypadButton::Left);
                        mmu.release_button(&JoypadButton::Up);
                        mmu.release_button(&JoypadButton::Down);
                    }
                }

                Event::ControllerButtonDown { button, .. } => {
                    match controls_map.get_controller_button(button) {
                        Some(joypad_button) => mmu.press_button(joypad_button),
                        None => {}
                    }
                }

                Event::ControllerButtonUp { button, .. } => {
                    match controls_map.get_controller_button(button) {
                        Some(joypad_button) => mmu.release_button(joypad_button),
                        None => {}
                    }
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

                    //drift = actual - remainder; //Needs more thought

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
}
