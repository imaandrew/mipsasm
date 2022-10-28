use mipsasm::Mipsasm;

pub fn asm(inst: &str) -> Vec<u32> {
    Mipsasm::new().base(0x80000000).assemble(inst).unwrap()
}

pub fn disasm(inst: &[u32]) -> String {
    let x = Mipsasm::new().base(0x80000000).debug().disassemble(inst);
    x.first().unwrap().clone()
}

#[macro_export]
macro_rules! test {
    ($name:ident, $inst:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let inst = asm($inst);
            assert_eq!(inst, vec![$expected]);
            assert_eq!(disasm(&inst), $inst);
        }
    };
}

#[macro_export]
macro_rules! lbl_test {
    ($name:ident, $inst:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let inst = asm($inst);
            assert_eq!(inst, vec![$expected]);
            let re = Regex::new(r"\w+:\n").unwrap();
            if let Some(x) = re.find_iter($inst).last() {
                let y = re.replace($inst, "");
                assert_eq!(
                    disasm(&inst),
                    y.replace(&x.as_str().replace(":\n", ""), "-0x1")
                );
                return;
            }

            assert_eq!(disasm(&inst), $inst);
        }
    };
}
