#[cfg(test)]
mod test {
    use serde::{Deserialize, Serialize};
    use serde_json::Result;

    use crate::{mmu::MMU, Flag, Joypad, Register, CPU, RAM_START};

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

            let mut mmu = MMU::new(vec![0; 0x8000], Joypad::new());
            let mut cpu = CPU::new();

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
                    mmu.rom[address as usize] = value;
                } else {
                    mmu.ram[(address - RAM_START) as usize] = value;
                }
            }

            //run cycles
            let expected_clock_cycles =
                4 * test_case.cycles.iter().filter(|&el| el.is_some()).count() as u64;

            let mut clock_cycles = 0;
            while clock_cycles < expected_clock_cycles {
                clock_cycles += cpu.next_op(&mut mmu) as u64;
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

                let actual = mmu.read(address);

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
