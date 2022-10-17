use crate::ast;

type I = ast::ITypeOp;
type J = ast::JTypeOp;
type R = ast::RTypeOp;

#[rustfmt::skip]
pub fn assemble(insts: Vec<ast::Instruction>) -> Vec<u32> {
    let mut bytes = vec![];
    for inst in insts {
        let i = match inst {
            ast::Instruction::Immediate { op, rs, rt, imm } => match op {
                I::Addi => 0b001000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Addiu => 0b001001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Andi => 0b001100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::B => 0b000100 << 26 | imm.as_u32(),
                I::Bal => 0b000001 << 26 | 0b10001 << 16 | imm.as_u32(),
                I::Bc0f => 0b010000 << 26 | 0b01000 << 21 | imm.as_u32(),
                I::Bc0fl => 0b010000 << 26 | 0b01000 << 21 | 0b00010 << 16 | imm.as_u32(),
                I::Bc0t => 0b010000 << 26 | 0b01000 << 21 | 0b00001 << 16 | imm.as_u32(),
                I::Bc0tl => 0b010000 << 26 | 0b01000 << 21 | 0b00011 << 16 | imm.as_u32(),
                I::Bc1f => 0b010001 << 26 | 0b01000 << 21 | imm.as_u32(),
                I::Bc1fl => 0b010001 << 26 | 0b01000 << 21 | 0b00010 << 16 | imm.as_u32(),
                I::Bc1t => 0b010001 << 26 | 0b01000 << 21 | 0b00001 << 16 | imm.as_u32(),
                I::Bc1tl => 0b010001 << 26 | 0b01000 << 21 | 0b00011 << 16 | imm.as_u32(),
                I::Beq => 0b000100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Beql => 0b010100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Beqz => 0b000100 << 26 | rs.as_num() << 21 | imm.as_u32(),
                I::Beqzl => 0b010100 << 26 | rs.as_num() << 21 | imm.as_u32(),
                I::Bge => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101010);
                    0b000100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgel => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101010);
                    0b010100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgeu => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101011);
                    0b000100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgeul => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101011);
                    0b010100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgez => 0b000001 << 26 | rs.as_num() << 21 | 0b00001 << 16 | imm.as_u32(),
                I::Bgezal => 0b000001 << 26 | rs.as_num() << 21 | 0b10001 << 16 | imm.as_u32(),
                I::Bgezall => 0b000001 << 26 | rs.as_num() << 21 | 0b10011 << 16 | imm.as_u32(),
                I::Bgezl => 0b000001 << 26 | rs.as_num() << 21 | 0b00011 << 16 | imm.as_u32(),
                I::Bgt => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101010);
                    0b000101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgtl => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101010);
                    0b010101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgtu => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101011);
                    0b000101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgtul => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101011);
                    0b010101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bgtz => 0b000111 << 26 | rs.as_num() << 21 | imm.as_u32(),
                I::Bgtzl => 0b010111 << 26 | rs.as_num() << 21 | imm.as_u32(),
                I::Ble => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101010);
                    0b000100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Blel => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101010);
                    0b010100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bleu => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101011);
                    0b000100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bleul => {
                    bytes.push(0b000001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101011);
                    0b010100 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Blez => 0b000110 << 26 | rs.as_num() << 21| imm.as_u32(),
                I::Blezl => 0b010110 << 26 | rs.as_num() << 21| imm.as_u32(),
                I::Blt => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101010);
                    0b000101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bltl => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101010);
                    0b010101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bltu => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101011);
                    0b000101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bltul => {
                    bytes.push(0b000001 << 21 | rs.as_num() << 16 | rt.as_num() << 11 | 0b101011);
                    0b010101 << 26 | 0b000001 << 21 | imm.as_u32()
                }
                I::Bltz => 0b000001 << 26 | rs.as_num() << 21 | imm.as_u32(),
                I::Bltzal => 0b000001 << 26 | rs.as_num() << 21 | 0b10000 << 16 | imm.as_u32(),
                I::Bltzall => 0b000001 << 26 | rs.as_num() << 21 | 0b10010 << 16 | imm.as_u32(),
                I::Bltzl => 0b000001 << 26 | rs.as_num() << 21 | 0b00010 << 16 | imm.as_u32(),
                I::Bne => 0b000101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Bnel => 0b010101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Bnez => 0b000101 << 26 | rs.as_num() << 21| imm.as_u32(),
                I::Bnezl => 0b010101 << 26 | rs.as_num() << 21| imm.as_u32(),
                I::Cache => 0b101111 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Daddi => 0b011000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Daddiu => 0b011001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Dli => {
                    bytes.push(0b001111 << 26 | rt.as_num() << 16 | (imm.as_u64() >> 48) as u32);
                    bytes.push(0b001101 << 26 | rt.as_num() << 21 | rt.as_num() << 16 | ((imm.as_u64() >> 32) & 0xFFFF) as u32);
                    bytes.push(rt.as_num() << 16 | rt.as_num() << 11 | 16 << 6 | 0b111000);
                    bytes.push(0b001101 << 26 | rt.as_num() << 21 | rt.as_num() << 16 | ((imm.as_u64() >> 16) & 0xFFFF) as u32);
                    bytes.push(rt.as_num() << 16 | rt.as_num() << 11 | 16 << 6 | 0b111000);
                    0b001101 << 26 | rt.as_num() << 21 | rt.as_num() << 16 | (imm.as_u64() & 0xFFFF) as u32

                }
                I::Dsubi => 0b011000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32().wrapping_neg(),
                I::Dsubiu => 0b001001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32().wrapping_neg(),
                I::Lb => 0b100000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lbu => 0b100100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Ld => 0b110111 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Ldc1 => 0b110101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Ldl => 0b011010 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Ldr => 0b011011 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lh => 0b100001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lhu => 0b100101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Li => {
                    bytes.push(0b001111 << 26 | rt.as_num() << 16 | imm.as_u32() >> 16);
                    0b001101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32() & 0xFFFF
                }
                I::Ll => 0b110000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lld => 0b110100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lli => 0b001101 << 26 | rt.as_num() << 16 | imm.as_u32(),
                I::Lui => 0b001111 << 26 | rt.as_num() << 16 | imm.as_u32(),
                I::Lw => 0b100011 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lwc1 => 0b110001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lwl => 0b100010 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lwr => 0b100110 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Lwu => 0b100111 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Ori => 0b001101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sb => 0b101000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sc => 0b111000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Scd => 0b111100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sd => 0b111111 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sdc1 => 0b111101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sdl => 0b101100 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sdr => 0b101101 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sh => 0b101001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Slti => 0b001010 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Sltiu => 0b001011 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Subi => 0b001000 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32().wrapping_neg(),
                I::Subiu => 0b001001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32().wrapping_neg(),
                I::Sw => 0b101011 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Swc1 => 0b111001 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Swl => 0b101010 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Swr => 0b101110 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
                I::Teqi => 0b000001 << 26 | rs.as_num() << 21 | 0b01100 << 16 | imm.as_u32(),
                I::Tgei => 0b000001 << 26 | rs.as_num() << 21 | 0b01000 << 16 | imm.as_u32(),
                I::Tgeiu => 0b000001 << 26 | rs.as_num() << 21 | 0b01001 << 16 | imm.as_u32(),
                I::Tlti => 0b000001 << 26 | rs.as_num() << 21 | 0b01010 << 16 | imm.as_u32(),
                I::Tltiu => 0b000001 << 26 | rs.as_num() << 21 | 0b01011 << 16 | imm.as_u32(),
                I::Tnei => 0b000001 << 26 | rs.as_num() << 21 | 0b01110 << 16 | imm.as_u32(),
                I::Xori => 0b001110 << 26 | rs.as_num() << 21 | rt.as_num() << 16 | imm.as_u32(),
            }
            ast::Instruction::Jump { op, target } => match op {
                J::J => 0b000010 << 26 | (target.as_u32() & 0x3FFFFFF) >> 2,
                J::Jal => 0b000011 << 26 | (target.as_u32() & 0x3FFFFFF) >> 2,
            }
            ast::Instruction::Register { op, rs, rt, rd, sa } => match op {
                R::Abs => {
                    bytes.push(0b000001 << 16 | rt.as_num() << 11 | 31 << 6 | 0b000011);
                    bytes.push(rd.as_num() << 21 | rt.as_num() << 16 | 0b000001 << 11 | 0b100110);
                    rd.as_num() << 21 | rd.as_num() << 16 | 0b000001 << 11 | 0b101110
                }
                R::AbsS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000101,
                R::AbsD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000101,
                R::Add => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100000,
                R::Addu => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100001,
                R::AddS => 0b010001 << 26 | 0b10000 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6,
                R::AddD => 0b010001 << 26 | 0b10001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6,
                R::And => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100100,
                R::Break => 0b001101,
                R::Clear => rd.as_num() << 11 | 0b100001,
                R::Cs => 0b010001 << 26 | 0b10000 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b0000011 << 4 | sa as u32,
                R::Cd => 0b010001 << 26 | 0b10001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b0000011 << 4 | sa as u32,
                R::CeilLS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001010,
                R::CeilLD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001010,
                R::CeilWS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001110,
                R::CeilWD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001110,
                R::Cfc0 => 0b010000 << 26 | 0b00010 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Cfc1 => 0b010001 << 26 | 0b00010 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Ctc0 => 0b010000 << 26 | 0b00110 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Ctc1 => 0b010001 << 26 | 0b00110 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::CvtDS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100001,
                R::CvtDW => 0b010001 << 26 | 0b10100 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100001,
                R::CvtDL => 0b010001 << 26 | 0b10101 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100001,
                R::CvtLS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100101,
                R::CvtLD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100101,
                R::CvtSD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100000,
                R::CvtSW => 0b010001 << 26 | 0b10100 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100000,
                R::CvtSL => 0b010001 << 26 | 0b10101 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100000,
                R::CvtWS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100100,
                R::CvtWD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b100100,
                R::Dabs => {
                    bytes.push(0b000001 << 16 | rt.as_num() << 11 | 31 << 6 | 0b000011);
                    bytes.push(rd.as_num() << 21 | rt.as_num() << 16 | 0b000001 << 11 | 0b100110);
                    rd.as_num() << 21 | rd.as_num() << 16 | 0b000001 << 11 | 0b100011
                }
                R::Dadd => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101100,
                R::Daddu => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101101,
                R::Ddiv => {
                    if rd.as_num() == 0 {
                        rs.as_num() << 21 | rt.as_num() << 16 | 0b011110
                    } else {
                        bytes.push(rt.as_num() << 21 | 0b110100);
                        bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011110);
                        bytes.push(0b011001 << 26 | 0b000001 << 16 | 0xFFFF);
                        bytes.push(0b000101 << 26 | 0b000001 << 21 | rt.as_num() << 16 | 4);
                        bytes.push(0b011001 << 26 | 0b000001 << 16 | 1);
                        bytes.push(0b000001 << 16 | 0b000001 << 11 | 31 << 6 | 0b111100);
                        bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b110100);
                        rd.as_num() << 11 | 0b010010

                    }
                },
                R::Ddivu => {
                    if rd.as_num() == 0 {
                        rs.as_num() << 21 | rt.as_num() << 16 | 0b011111
                    } else {
                        bytes.push(rt.as_num() << 21 | 0b110100);
                        bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011111);
                        rd.as_num() << 11 | 0b010010
                    }
                },
                R::Div => if rd.as_num() == 0 {
                    rs.as_num() << 21 | rt.as_num() << 16 | 0b011010
                } else {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011010);
                    bytes.push(0b001111 << 26 | 0b000001 << 16 | 0xFFFF);
                    bytes.push(0b001101 << 26 | 0b000001 << 21 | 0b000001 << 16 | 0xFFFF);
                    bytes.push(0b000101 << 26 | 0b000001 << 21 | rt.as_num() << 16 | 3);
                    bytes.push(0b001111 << 26 | 0b000001 << 16 | 0x8000);
                    bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b110100);
                    rd.as_num() << 11 | 0b010010
                }
                R::Divu => if rd.as_num() == 0 {
                    rs.as_num() << 21 | rt.as_num() << 16 | 0b011011
                } else {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011011);
                    rd.as_num() << 11 | 0b010010
                }
                R::DivS => 0b010001 << 26 | 0b10000 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000011,
                R::DivD => 0b010001 << 26 | 0b10001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000011,
                R::Dmfc0 => 0b010000 << 26 | 0b00001 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Dmfc1 => 0b010001 << 26 | 0b00001 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Dmove => rs.as_num() << 21 | rd.as_num() << 11 | 0b101101,
                R::Dmtc0 => 0b010000 << 26 | 0b00101 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Dmtc1 => 0b010001 << 26 | 0b00101 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Dmul => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011100);
                    rd.as_num() << 11 | 0b010010
                }
                R::Dmulo => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011100);
                    bytes.push(rd.as_num() << 11 | 0b010010);
                    bytes.push(rd.as_num() << 16 | rd.as_num() << 11 | 31 << 6 | 0b111111);
                    bytes.push(0b000001 << 11 | 0b010000);
                    bytes.push(rd.as_num() << 21 | 0b000001 << 16 | 0b110110);
                    rd.as_num() << 11 | 0b010010
                }
                R::Dmulou => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011101);
                    bytes.push(0b000001 << 11 | 0b010000);
                    bytes.push(rd.as_num() << 11 | 0b010010);
                    rs.as_num() << 21 | 0b110110
                }
                R::Dmult => rs.as_num() << 21 | rt.as_num() << 16 | 0b011100,
                R::Dmulu => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011101);
                    rd.as_num() << 11 | 0b010010
                }
                R::Dmultu => rs.as_num() << 21 | rt.as_num() << 16 | 0b011101,
                R::Dneg => rs.as_num() << 21 | rd.as_num() << 11 | 0b101110,
                R::Dnegu => rs.as_num() << 21 | rd.as_num() << 11 | 0b101111,
                R::Drem => {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011110);
                    bytes.push(0b011001 << 26 | 0b000001 << 16 | 0xFFFF);
                    bytes.push(0b000101 << 26 | 0b000001 << 21 | rt.as_num() << 16 | 4);
                    bytes.push(0b011001 << 26 | 0b000001 << 16 | 1);
                    bytes.push(0b000001 << 16 | 0b000001 << 11 | 31 << 6 | 0b111100);
                    bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b110100);
                    rd.as_num() << 11 | 0b010000
                }
                R::Dremu => {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011111);
                    rd.as_num() << 11 | 0b010000
                }
                R::Drol => {
                    bytes.push(rt.as_num() << 16 | 0b000001 << 11 | 0b101111);
                    bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b000001 << 11 | 0b010110);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b010100);
                    rd.as_num() << 21 | 0b000001 << 16 | rd.as_num() << 11 | 0b100101
                }
                R::Dror => {
                    bytes.push(rt.as_num() << 16 | 0b000001 << 11 | 0b101111);
                    bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b000001 << 11 | 0b010100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b010110);
                    rd.as_num() << 21 | 0b000001 << 16 | rd.as_num() << 11 | 0b100101
                }
                R::Dsll => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111000,
                R::Dsll32 => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111100,
                R::Dsllv => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b010100,
                R::Dsra => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111011,
                R::Dsra32 => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111111,
                R::Dsrav => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b010111,
                R::Dsrl => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111010,
                R::Dsrl32 => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b111110,
                R::Dsrlv => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b010110,
                R::Dsub => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101110,
                R::Dsubu => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101111,
                R::Eret => 0b010000 << 26 | 0b00001 << 25 | 0b011000,
                R::FloorLS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001011,
                R::FloorLD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001011,
                R::FloorWS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001111,
                R::FloorWD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001111,
                R::Jalr => rs.as_num() << 21 | rd.as_num() << 11 | 0b001001,
                R::Jr => rs.as_num() << 21 | 0b001000,
                R::Mfc0 => 0b010000 << 26 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Mfc1 => 0b010001 << 26 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Mfhi => rd.as_num() << 11 | 0b010000,
                R::Mflo => rd.as_num() << 11 | 0b010010,
                R::Move => rs.as_num() << 21 | rd.as_num() << 11 | 0b100001,
                R::MovS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000110,
                R::MovD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000110,
                R::Mtc0 => 0b010000 << 26 | 0b00100 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Mtc1 => 0b010001 << 26 | 0b00100 << 21 | rt.as_num() << 16 | rd.as_num() << 11,
                R::Mthi => rs.as_num() << 21 | 0b010001,
                R::Mtlo => rs.as_num() << 21 | 0b010011,
                R::Mul => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011000);
                    rd.as_num() << 11 | 0b010010
                }
                R::Mulu => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011001);
                    rd.as_num() << 11 | 0b010010
                }
                R::MulS => 0b010001 << 26 | 0b10000 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000010,
                R::MulD => 0b010001 << 26 | 0b10001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000010,
                R::Mulo => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011000);
                    bytes.push(rd.as_num() << 11 | 0b010010);
                    bytes.push(rd.as_num() << 16 | rd.as_num() << 11 | 31 << 6 | 0b000011);
                    bytes.push(0b000001 << 11 | 0b010000);
                    bytes.push(rd.as_num() << 21 | 0b110110);
                    rd.as_num() << 11 | 0b010010
                }
                R::Mulou => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011001);
                    bytes.push(0b000001 << 11 | 0b010000);
                    bytes.push(rd.as_num() << 11 | 0b010010);
                    0b000001 << 21 | 0b110110
                }
                R::Mult => rs.as_num() << 21 | rt.as_num() << 16 | 0b011000,
                R::Multu => rs.as_num() << 21 | rt.as_num() << 16 | 0b011001,
                R::Neg => rs.as_num() << 21 | rd.as_num() << 11 | 0b100010,
                R::Negu => rs.as_num() << 21 | rd.as_num() << 11 | 0b100011,
                R::NegS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000111,
                R::NegD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000111,
                R::Nop => 0,
                R::Nor => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100111,
                R::Not => rs.as_num() << 21 | rd.as_num() << 11 | 0b100111,
                R::Or => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100101,
                R::Rem => {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011010);
                    bytes.push(0b001111 << 26 | 0b000001 << 16 | 0xFFFF);
                    bytes.push(0b001101 << 26 | 0b000001 << 21 | 0b000001 << 16 | 0xFFFF);
                    bytes.push(0b000101 << 26 | 0b000001 << 21 | rt.as_num() << 16 | 3);
                    bytes.push(0b001111 << 26 | 0b000001 << 16 | 0x8000);
                    bytes.push(rs.as_num() << 21 | 0b000001 << 16 | 0b110100);
                    rd.as_num() << 11 | 0b010000
                }
                R::Remu => {
                    bytes.push(rt.as_num() << 21 | 0b110100);
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | 0b011011);
                    rd.as_num() << 11 | 0b010000
                }
                R::RoundLS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001000,
                R::RoundLD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001000,
                R::RoundWS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001100,
                R::RoundWD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001100,
                R::Seq => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100110);
                    0b001011 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::Sge => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101010);
                    0b001110 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::Sgeu => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rs.as_num() << 11 | 0b101011);
                    0b001110 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::Sgt => rt.as_num() << 21 | rs.as_num() << 16 | rd.as_num() << 11 | 0b101010,
                R::Sgtu => rt.as_num() << 21 | rs.as_num() << 16 | rd.as_num() << 11 | 0b101011,
                R::Sle => {
                    bytes.push(rt.as_num() << 21 | rs.as_num() << 16 | rd.as_num() << 11 | 0b101010);
                    0b001110 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::Sleu => {
                    bytes.push(rt.as_num() << 21 | rs.as_num() << 16 | rd.as_num() << 11 | 0b101011);
                    0b001110 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::Sll => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6,
                R::Sllv => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b000100,
                R::Slt => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101010,
                R::Sltu => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b101011,
                R::Sne => {
                    bytes.push(rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100110);
                    0b001011 << 26 | rd.as_num() << 21 | rd.as_num() << 16 | 1
                }
                R::SqrtS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000100,
                R::SqrtD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000100,
                R::Sra => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b000011,
                R::Srav => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b000111,
                R::Srl => rt.as_num() << 16 | rd.as_num() << 11 | (sa as u32) << 6 | 0b000010,
                R::Srlv => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b000110,
                R::Sub => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100010,
                R::Subu => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100011,
                R::SubS => 0b010001 << 26 | 0b10000 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000001,
                R::SubD => 0b010001 << 26 | 0b10001 << 21 | rt.as_num() << 16 | rs.as_num() << 11 | rd.as_num() << 6 | 0b000001,
                R::Sync => 0b001111,
                R::Syscall => 0b001100,
                R::Teq => rs.as_num() << 21 | rt.as_num() << 16 | 0b110100,
                R::Tge => rs.as_num() << 21 | rt.as_num() << 16 | 0b110000,
                R::Tgeu => rs.as_num() << 21 | rt.as_num() << 16 | 0b110001,
                R::Tlbp => 0b010000 << 26 | 0b00001 << 25 | 0b001000,
                R::Tlbr => 0b010000 << 26 | 0b00001 << 25 | 0b000001,
                R::Tlbwi => 0b010000 << 26 | 0b00001 << 25 | 0b000010,
                R::Tlbwr => 0b010000 << 26 | 0b00001 << 25 | 0b000110,
                R::Tlt => rs.as_num() << 21 | rt.as_num() << 16 | 0b110010,
                R::Tltu => rs.as_num() << 21 | rt.as_num() << 16 | 0b110011,
                R::Tne => rs.as_num() << 21 | rt.as_num() << 16 | 0b110110,
                R::TruncLS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001001,
                R::TruncLD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001001,
                R::TruncWS => 0b010001 << 26 | 0b10000 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001101,
                R::TruncWD => 0b010001 << 26 | 0b10001 << 21 | rs.as_num() << 11 | rd.as_num() << 6 | 0b001101,
                R::Xor => rs.as_num() << 21 | rt.as_num() << 16 | rd.as_num() << 11 | 0b100110,
            }
        };
        bytes.push(i);
    }
    bytes
}
