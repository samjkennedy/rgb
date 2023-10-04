#[derive(Clone)]
pub struct Cart {
    pub name: String,
    pub rom: Vec<u8>,
}

impl Cart {
    pub fn load(bytes: Vec<u8>) -> Cart {
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
