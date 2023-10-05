pub struct Joypad {
    pub state: u8,
}
pub enum ButtonMode {
    Action,
    Direction,
}

#[derive(Copy, Clone)]
pub enum JoypadButton {
    Right,
    Left,
    Up,
    Down,
    A,
    B,
    Select,
    Start,
}

impl Joypad {
    pub fn new() -> Joypad {
        return Joypad { state: 0 };
    }

    pub fn read(&self, mode: ButtonMode) -> u8 {
        return match mode {
            ButtonMode::Action => !((self.state & 0xF0) >> 4),
            ButtonMode::Direction => !(self.state & 0x0F),
        };
    }
}
