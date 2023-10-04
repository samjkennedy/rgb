#![allow(arithmetic_overflow)]
use std::env;

mod cart;
mod cpu;
mod joypad;
mod mmu;
mod ppu;

extern crate sdl2;

use cpu::CPU;
use joypad::Joypad;

//sdl
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use std::time::SystemTime;

use crate::cart::Cart;
use crate::cpu::Interrupt;
use crate::joypad::JoypadButton;
use crate::mmu::{MMU, RAM_START};
use crate::ppu::{Palette, PPU};

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

            ppu.scanline(&mut mmu); //TODO: This is a huge performance hit, optimise this call
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

                //TODO: Match based on a key mapping, not hard coded keys
                //Key pressed
                Event::KeyDown {
                    keycode: Some(Keycode::Right),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Right);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Left),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Left);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Up),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Up);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Down);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::X),
                    ..
                } => {
                    mmu.press_button(JoypadButton::A);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Z),
                    ..
                } => {
                    mmu.press_button(JoypadButton::B);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::LShift),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Select);
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    mmu.press_button(JoypadButton::Start);
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
                    mmu.release_button(JoypadButton::Right);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Left),
                    ..
                } => {
                    mmu.release_button(JoypadButton::Left);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Up),
                    ..
                } => {
                    mmu.release_button(JoypadButton::Up);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    mmu.release_button(JoypadButton::Down);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::X),
                    ..
                } => {
                    mmu.release_button(JoypadButton::A);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Z),
                    ..
                } => {
                    mmu.release_button(JoypadButton::B);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::LShift),
                    ..
                } => {
                    mmu.release_button(JoypadButton::Select);
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    mmu.release_button(JoypadButton::Start);
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
