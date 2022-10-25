use mipsasm::Mipsasm;

pub fn asm(inst: &str) -> Vec<u32> {
    Mipsasm::new().base(0x80000000).assemble(inst).unwrap()
}

pub fn disasm(inst: &[u32]) -> String {
    let x = Mipsasm::new().base(0x80000000).debug().disassemble(inst);
    x.first().unwrap().clone()
}
