# RGB
![image](tetris.png)
## Rusty GameBoy

A gameboy emulator written in rust. Currently can only run Tetris and Dr. Mario.

## Goals

- An emulator capable of playing _most_ gameboy games
- QoL features like debugging info, controller support and rebinding, rewinding, etc
- Good performance and cross platform support

## Build

`cargo build --release`

## Run

`cargo run -r <ROM_FILE>`

## Missing features

- Audio
- Proper rendering
- Debugger
- Remapping controls + ~~controller support~~
- Save states