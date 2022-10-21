pub fn asm(inst: &str) -> Vec<u32> {
    let x = mipsasm::parser::scan(inst, 0x80000000, None).unwrap();
    mipsasm::assembler::assemble(x)
}

pub fn disasm(inst: &[u32]) -> String {
    let x = mipsasm::disassembler::disassemble(inst.to_vec());
    format!("{:?}", x.first().unwrap())
}
