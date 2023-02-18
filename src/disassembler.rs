use crate::ast;

macro_rules! inst {
    (Imm, $op:ident, $rs:expr, $rt:expr, $imm:expr, $bytes:expr) => {
        ast::Instruction::Immediate {
            op: ast::ITypeOp::$op,
            rs: ast::Register::try_from($rs).unwrap(),
            rt: ast::Register::try_from($rt).unwrap(),
            imm: ast::Immediate::Short($imm as u16),
            bytes: $bytes,
        }
    };
    (Jump, $op:ident, $target:expr, $bytes:expr) => {
        ast::Instruction::Jump {
            op: ast::JTypeOp::$op,
            target: ast::Target::Address($target),
            bytes: $bytes,
        }
    };
    (Reg, $op:ident, $rs:expr, $rt:expr, $rd:expr, $bytes:expr) => {
        ast::Instruction::Register {
            op: ast::RTypeOp::$op,
            rs: ast::Register::try_from($rs).unwrap(),
            rt: ast::Register::try_from($rt).unwrap(),
            rd: ast::Register::try_from($rd).unwrap(),
            sa: 0,
            bytes: $bytes,
        }
    };
    (Reg, $op:ident, $rs:expr, $rt:expr, $rd:expr, $sa:expr, $bytes:expr) => {
        ast::Instruction::Register {
            op: ast::RTypeOp::$op,
            rs: ast::Register::try_from($rs).unwrap(),
            rt: ast::Register::try_from($rt).unwrap(),
            rd: ast::Register::try_from($rd).unwrap(),
            sa: $sa,
            bytes: $bytes,
        }
    };
    (Bytes, $imm:expr) => {
        ast::Instruction::Bytes {
            bytes: ast::Immediate::Int($imm),
        }
    };
}

pub fn disassemble(bytes: Vec<u32>, base_addr: u32) -> Vec<ast::Instruction> {
    let mut insts = vec![];

    for inst in bytes {
        let op = inst >> 26;
        let rs = (inst >> 21) & 0x1F;
        let rt = (inst >> 16) & 0x1F;
        let rd = (inst >> 11) & 0x1F;
        let sa = (inst >> 6) & 0x1F;
        let code = (inst >> 6) & 0xFFFFF;
        let funct = inst & 0x3F;
        let imm = inst & 0xFFFF;
        let target = ((inst & 0x3FFFFFF) << 2) | (base_addr & !0xFFFFFFF);

        let i = match op {
            0 => match funct {
                0 => inst!(Reg, Sll, 0, rt, rd, sa, inst),
                2 => inst!(Reg, Srl, 0, rt, rd, sa, inst),
                3 => inst!(Reg, Sra, 0, rt, rd, sa, inst),
                4 => inst!(Reg, Sllv, rs, rt, rd, inst),
                6 => inst!(Reg, Srlv, rs, rt, rd, inst),
                7 => inst!(Reg, Srav, rs, rt, rd, inst),
                8 => inst!(Reg, Jr, rs, 0, 0, inst),
                9 => inst!(Reg, Jalr, rs, 0, rd, inst),
                12 => inst!(Reg, Syscall, 0, 0, 0, code, inst),
                13 => inst!(Reg, Break, 0, 0, 0, code, inst),
                15 => inst!(Reg, Sync, 0, 0, 0, code, inst),
                16 => inst!(Reg, Mfhi, 0, 0, rd, inst),
                17 => inst!(Reg, Mthi, rs, 0, 0, inst),
                18 => inst!(Reg, Mflo, 0, 0, rd, inst),
                19 => inst!(Reg, Mtlo, rs, 0, 0, inst),
                20 => inst!(Reg, Dsllv, rs, rt, rd, inst),
                22 => inst!(Reg, Dsrlv, rs, rt, rd, inst),
                23 => inst!(Reg, Dsrav, rs, rt, rd, inst),
                24 => inst!(Reg, Mult, rs, rt, 0, inst),
                25 => inst!(Reg, Multu, rs, rt, 0, inst),
                26 => inst!(Reg, Div, rs, rt, 0, inst),
                27 => inst!(Reg, Divu, rs, rt, 0, inst),
                28 => inst!(Reg, Dmult, rs, rt, 0, inst),
                29 => inst!(Reg, Dmultu, rs, rt, 0, inst),
                30 => inst!(Reg, Ddiv, rs, rt, 0, inst),
                31 => inst!(Reg, Ddivu, rs, rt, 0, inst),
                32 => inst!(Reg, Add, rs, rt, rd, inst),
                33 => inst!(Reg, Addu, rs, rt, rd, inst),
                34 => inst!(Reg, Sub, rs, rt, rd, inst),
                35 => inst!(Reg, Subu, rs, rt, rd, inst),
                36 => inst!(Reg, And, rs, rt, rd, inst),
                37 => inst!(Reg, Or, rs, rt, rd, inst),
                38 => inst!(Reg, Xor, rs, rt, rd, inst),
                39 => inst!(Reg, Nor, rs, rt, rd, inst),
                42 => inst!(Reg, Slt, rs, rt, rd, inst),
                43 => inst!(Reg, Sltu, rs, rt, rd, inst),
                44 => inst!(Reg, Dadd, rs, rt, rd, inst),
                45 => inst!(Reg, Daddu, rs, rt, rd, inst),
                46 => inst!(Reg, Dsub, rs, rt, rd, inst),
                47 => inst!(Reg, Dsubu, rs, rt, rd, inst),
                48 => inst!(Reg, Tge, rs, rt, 0, inst),
                49 => inst!(Reg, Tgeu, rs, rt, 0, inst),
                50 => inst!(Reg, Tlt, rs, rt, 0, inst),
                51 => inst!(Reg, Tltu, rs, rt, 0, inst),
                52 => inst!(Reg, Teq, rs, rt, 0, inst),
                54 => inst!(Reg, Tne, rs, rt, 0, inst),
                56 => inst!(Reg, Dsll, 0, rt, rd, sa, inst),
                58 => inst!(Reg, Dsrl, 0, rt, rd, sa, inst),
                59 => inst!(Reg, Dsra, 0, rt, rd, sa, inst),
                60 => inst!(Reg, Dsll32, 0, rt, rd, sa, inst),
                62 => inst!(Reg, Dsrl32, 0, rt, rd, sa, inst),
                63 => inst!(Reg, Dsra32, 0, rt, rd, sa, inst),
                _ => inst!(Bytes, inst),
            },
            1 => match rt {
                0 => inst!(Imm, Bltz, rs, 0, imm, inst),
                1 => inst!(Imm, Bgez, rs, 0, imm, inst),
                2 => inst!(Imm, Bltzl, rs, 0, imm, inst),
                3 => inst!(Imm, Bgezl, rs, 0, imm, inst),
                8 => inst!(Imm, Tgei, rs, 0, imm, inst),
                9 => inst!(Imm, Tgeiu, rs, 0, imm, inst),
                10 => inst!(Imm, Tlti, rs, 0, imm, inst),
                11 => inst!(Imm, Tltiu, rs, 0, imm, inst),
                12 => inst!(Imm, Teqi, rs, 0, imm, inst),
                14 => inst!(Imm, Tnei, rs, 0, imm, inst),
                16 => inst!(Imm, Bltzal, rs, 0, imm, inst),
                17 => inst!(Imm, Bgezal, rs, 0, imm, inst),
                18 => inst!(Imm, Bltzall, rs, 0, imm, inst),
                19 => inst!(Imm, Bgezall, rs, 0, imm, inst),
                _ => inst!(Bytes, inst),
            },
            2 => inst!(Jump, J, target, inst),
            3 => inst!(Jump, Jal, target, inst),
            4 => inst!(Imm, Beq, rs, rt, imm, inst),
            5 => inst!(Imm, Bne, rs, rt, imm, inst),
            6 => inst!(Imm, Blez, rs, rt, imm, inst),
            7 => inst!(Imm, Bgtz, rs, rt, imm, inst),
            8 => inst!(Imm, Addi, rs, rt, imm, inst),
            9 => inst!(Imm, Addiu, rs, rt, imm, inst),
            10 => inst!(Imm, Slti, rs, rt, imm, inst),
            11 => inst!(Imm, Sltiu, rs, rt, imm, inst),
            12 => inst!(Imm, Andi, rs, rt, imm, inst),
            13 => inst!(Imm, Ori, rs, rt, imm, inst),
            14 => inst!(Imm, Xori, rs, rt, imm, inst),
            15 => inst!(Imm, Lui, rs, rt, imm, inst),
            16 => match (rs, rt) {
                (0, _) => inst!(Reg, Mfc0, 0, rt, rd, inst),
                (1, _) => inst!(Reg, Dmfc0, 0, rt, rd, inst),
                (2, _) => inst!(Reg, Cfc0, 0, rt, rd, inst),
                (4, _) => inst!(Reg, Mtc0, 0, rt, rd, inst),
                (5, _) => inst!(Reg, Dmtc0, 0, rt, rd, inst),
                (6, _) => inst!(Reg, Ctc0, 0, rt, rd, inst),
                (8, 0) => inst!(Imm, Bc0f, 0, 0, imm, inst),
                (8, 1) => inst!(Imm, Bc0t, 0, 0, imm, inst),
                (8, 2) => inst!(Imm, Bc0fl, 0, 0, imm, inst),
                (8, 3) => inst!(Imm, Bc0tl, 0, 0, imm, inst),
                (_, _) => match funct {
                    1 => inst!(Reg, Tlbr, 0, 0, 0, inst),
                    2 => inst!(Reg, Tlbwi, 0, 0, 0, inst),
                    6 => inst!(Reg, Tlbwr, 0, 0, 0, inst),
                    8 => inst!(Reg, Tlbp, 0, 0, 0, inst),
                    24 => inst!(Reg, Eret, 0, 0, 0, inst),
                    _ => inst!(Bytes, inst),
                },
            },
            17 => match (rs, rt) {
                (0, _) => inst!(Reg, Mfc1, 0, rt, rd, inst),
                (1, _) => inst!(Reg, Dmfc1, 0, rt, rd, inst),
                (2, _) => inst!(Reg, Cfc1, 0, rt, rd, inst),
                (4, _) => inst!(Reg, Mtc1, 0, rt, rd, inst),
                (5, _) => inst!(Reg, Dmtc1, 0, rt, rd, inst),
                (6, _) => inst!(Reg, Ctc1, 0, rt, rd, inst),
                (8, 0) => inst!(Imm, Bc1f, 0, 0, imm, inst),
                (8, 1) => inst!(Imm, Bc1t, 0, 0, imm, inst),
                (8, 2) => inst!(Imm, Bc1fl, 0, 0, imm, inst),
                (8, 3) => inst!(Imm, Bc1tl, 0, 0, imm, inst),
                (16, _) => match funct {
                    0 => inst!(Reg, AddS, rd, rt, sa, inst),
                    1 => inst!(Reg, SubS, rd, rt, sa, inst),
                    2 => inst!(Reg, MulS, rd, rt, sa, inst),
                    3 => inst!(Reg, DivS, rd, rt, sa, inst),
                    4 => inst!(Reg, SqrtS, rd, 0, sa, inst),
                    5 => inst!(Reg, AbsS, rd, 0, sa, inst),
                    6 => inst!(Reg, MovS, rd, 0, sa, inst),
                    7 => inst!(Reg, NegS, rd, 0, sa, inst),
                    8 => inst!(Reg, RoundLS, rd, 0, sa, inst),
                    9 => inst!(Reg, TruncLS, rd, 0, sa, inst),
                    10 => inst!(Reg, CeilLS, rd, 0, sa, inst),
                    11 => inst!(Reg, FloorLS, rd, 0, sa, inst),
                    12 => inst!(Reg, RoundWS, rd, 0, sa, inst),
                    13 => inst!(Reg, TruncWS, rd, 0, sa, inst),
                    14 => inst!(Reg, CeilWS, rd, 0, sa, inst),
                    15 => inst!(Reg, FloorWS, rd, 0, sa, inst),
                    33 => inst!(Reg, CvtDS, rd, 0, sa, inst),
                    36 => inst!(Reg, CvtWS, rd, 0, sa, inst),
                    37 => inst!(Reg, CvtLS, rd, 0, sa, inst),
                    48..=63 => inst!(Reg, Cs, rd, rt, 0, funct & 0xF, inst),
                    _ => inst!(Bytes, inst),
                },
                (17, _) => match funct {
                    0 => inst!(Reg, AddD, rd, rt, sa, inst),
                    1 => inst!(Reg, SubD, rd, rt, sa, inst),
                    2 => inst!(Reg, MulD, rd, rt, sa, inst),
                    3 => inst!(Reg, DivD, rd, rt, sa, inst),
                    4 => inst!(Reg, SqrtD, rd, 0, sa, inst),
                    5 => inst!(Reg, AbsD, rd, 0, sa, inst),
                    6 => inst!(Reg, MovD, rd, 0, sa, inst),
                    7 => inst!(Reg, NegD, rd, 0, sa, inst),
                    8 => inst!(Reg, RoundLD, rd, 0, sa, inst),
                    9 => inst!(Reg, TruncLD, rd, 0, sa, inst),
                    10 => inst!(Reg, CeilLD, rd, 0, sa, inst),
                    11 => inst!(Reg, FloorLD, rd, 0, sa, inst),
                    12 => inst!(Reg, RoundWD, rd, 0, sa, inst),
                    13 => inst!(Reg, TruncWD, rd, 0, sa, inst),
                    14 => inst!(Reg, CeilWD, rd, 0, sa, inst),
                    15 => inst!(Reg, FloorWD, rd, 0, sa, inst),
                    32 => inst!(Reg, CvtSD, rd, 0, sa, inst),
                    36 => inst!(Reg, CvtWD, rd, 0, sa, inst),
                    37 => inst!(Reg, CvtLD, rd, 0, sa, inst),
                    48..=63 => inst!(Reg, Cd, rd, rt, 0, funct & 0xF, inst),
                    _ => inst!(Bytes, inst),
                },
                (20, _) => match funct {
                    32 => inst!(Reg, CvtSW, rd, 0, sa, inst),
                    33 => inst!(Reg, CvtDW, rd, 0, sa, inst),
                    _ => inst!(Bytes, inst),
                },
                (21, _) => match funct {
                    32 => inst!(Reg, CvtSL, rd, 0, sa, inst),
                    33 => inst!(Reg, CvtDL, rd, 0, sa, inst),
                    _ => inst!(Bytes, inst),
                },
                _ => inst!(Bytes, inst),
            },
            20 => inst!(Imm, Beql, rs, rt, imm, inst),
            21 => inst!(Imm, Bnel, rs, rt, imm, inst),
            22 => inst!(Imm, Blezl, rs, rt, imm, inst),
            23 => inst!(Imm, Bgtzl, rs, rt, imm, inst),
            24 => inst!(Imm, Daddi, rs, rt, imm, inst),
            25 => inst!(Imm, Daddiu, rs, rt, imm, inst),
            26 => inst!(Imm, Ldl, rs, rt, imm, inst),
            27 => inst!(Imm, Ldr, rs, rt, imm, inst),
            32 => inst!(Imm, Lb, rs, rt, imm, inst),
            33 => inst!(Imm, Lh, rs, rt, imm, inst),
            34 => inst!(Imm, Lwl, rs, rt, imm, inst),
            35 => inst!(Imm, Lw, rs, rt, imm, inst),
            36 => inst!(Imm, Lbu, rs, rt, imm, inst),
            37 => inst!(Imm, Lhu, rs, rt, imm, inst),
            38 => inst!(Imm, Lwr, rs, rt, imm, inst),
            39 => inst!(Imm, Lwu, rs, rt, imm, inst),
            40 => inst!(Imm, Sb, rs, rt, imm, inst),
            41 => inst!(Imm, Sh, rs, rt, imm, inst),
            42 => inst!(Imm, Swl, rs, rt, imm, inst),
            43 => inst!(Imm, Sw, rs, rt, imm, inst),
            44 => inst!(Imm, Sdl, rs, rt, imm, inst),
            45 => inst!(Imm, Sdr, rs, rt, imm, inst),
            46 => inst!(Imm, Swr, rs, rt, imm, inst),
            47 => inst!(Imm, Cache, rs, rt, imm, inst),
            48 => inst!(Imm, Ll, rs, rt, imm, inst),
            49 => inst!(Imm, Lwc1, rs, rt, imm, inst),
            52 => inst!(Imm, Lld, rs, rt, imm, inst),
            53 => inst!(Imm, Ldc1, rs, rt, imm, inst),
            55 => inst!(Imm, Ld, rs, rt, imm, inst),
            56 => inst!(Imm, Sc, rs, rt, imm, inst),
            57 => inst!(Imm, Swc1, rs, rt, imm, inst),
            60 => inst!(Imm, Scd, rs, rt, imm, inst),
            61 => inst!(Imm, Sdc1, rs, rt, imm, inst),
            63 => inst!(Imm, Sd, rs, rt, imm, inst),
            _ => inst!(Bytes, inst),
        };

        insts.push(i);
    }
    insts
}
