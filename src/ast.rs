use std::convert::TryFrom;
use std::str::FromStr;
use strum_macros::EnumString;

#[derive(Debug)]
pub enum Token {
    Label(Target),
    Instruction(Instruction),
}

#[derive(Debug)]
pub enum Target {
    Label(String),
    Address(u32),
}

#[derive(Debug)]
pub enum Immediate {
    Signed(i16),
    Unsigned(u16),
}

#[derive(Debug)]
pub enum Instruction {
    Immediate {
        op: ITypeOp,
        rs: Register,
        rt: Register,
        imm: Immediate,
    },
    Jump {
        op: JTypeOp,
        target: Target,
    },
    Register {
        op: RTypeOp,
        rs: Register,
        rt: Register,
        rd: Register,
        sa: i16,
        funct: RTypeOp,
    },
    Regimm {
        op: ITypeOp,
        rs: Register,
        sub: ITypeOp,
        imm: Immediate,
    },
    Copz {
        op: ITypeOp,
        rs: CopRs,
        rt: CopRt,
        imm: Immediate,
    },
}

#[derive(Debug)]
pub enum Register {
    Zero,
    At,
    V0,
    V1,
    A0,
    A1,
    A2,
    A3,
    T0,
    T1,
    T2,
    T3,
    T4,
    T5,
    T6,
    T7,
    S0,
    S1,
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    T8,
    T9,
    K0,
    K1,
    Gp,
    Sp,
    Fp,
    Ra,
}

impl Register {
    pub fn null() -> Self {
        Register::Zero
    }
}

impl TryFrom<i32> for Register {
    type Error = ();

    fn try_from(reg: i32) -> Result<Self, Self::Error> {
        match reg {
            0 => Ok(Register::Zero),
            1 => Ok(Register::At),
            2 => Ok(Register::V0),
            3 => Ok(Register::V1),
            4 => Ok(Register::A0),
            5 => Ok(Register::A1),
            6 => Ok(Register::A2),
            7 => Ok(Register::A3),
            8 => Ok(Register::T0),
            9 => Ok(Register::T1),
            10 => Ok(Register::T2),
            11 => Ok(Register::T3),
            12 => Ok(Register::T4),
            13 => Ok(Register::T5),
            14 => Ok(Register::T6),
            15 => Ok(Register::T7),
            16 => Ok(Register::S0),
            17 => Ok(Register::S1),
            18 => Ok(Register::S2),
            19 => Ok(Register::S3),
            20 => Ok(Register::S4),
            21 => Ok(Register::S5),
            22 => Ok(Register::S6),
            23 => Ok(Register::S7),
            24 => Ok(Register::T8),
            25 => Ok(Register::T9),
            26 => Ok(Register::K0),
            27 => Ok(Register::K1),
            28 => Ok(Register::Gp),
            29 => Ok(Register::Sp),
            30 => Ok(Register::Fp),
            31 => Ok(Register::Ra),
            _ => Err(()),
        }
    }
}

impl FromStr for Register {
    type Err = ();

    fn from_str(reg: &str) -> Result<Self, Self::Err> {
        match reg.to_lowercase().as_str() {
            "zero" | "r0" => Ok(Register::Zero),
            "at" => Ok(Register::At),
            "v0" => Ok(Register::V0),
            "v1" => Ok(Register::V1),
            "a0" => Ok(Register::A0),
            "a1" => Ok(Register::A1),
            "a2" => Ok(Register::A2),
            "a3" => Ok(Register::A3),
            "t0" => Ok(Register::T0),
            "t1" => Ok(Register::T1),
            "t2" => Ok(Register::T2),
            "t3" => Ok(Register::T3),
            "t4" => Ok(Register::T4),
            "t5" => Ok(Register::T5),
            "t6" => Ok(Register::T6),
            "t7" => Ok(Register::T7),
            "s0" => Ok(Register::S0),
            "s1" => Ok(Register::S1),
            "s2" => Ok(Register::S2),
            "s3" => Ok(Register::S3),
            "s4" => Ok(Register::S4),
            "s5" => Ok(Register::S5),
            "s6" => Ok(Register::S6),
            "s7" => Ok(Register::S7),
            "t8" => Ok(Register::T8),
            "t9" => Ok(Register::T9),
            "k0" => Ok(Register::K0),
            "k1" => Ok(Register::K1),
            "gp" => Ok(Register::Gp),
            "sp" => Ok(Register::Sp),
            "fp" => Ok(Register::Fp),
            "ra" => Ok(Register::Ra),
            _ => Err(()),
        }
    }
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ITypeOp {
    Addi,
    Addiu,
    Andi,
    Bczf,
    Bczfl,
    Bczt,
    Bcztl,
    Beq,
    Beql,
    Bgez,
    Bgezal,
    Bgezall,
    Bgezl,
    Bgtz,
    Bgtzl,
    Blez,
    Blezl,
    Bltz,
    Bltzal,
    Bltzall,
    Bltzl,
    Bne,
    Bnel,
    Cache,
    Daddi,
    Daddiu,
    Lb,
    Lbu,
    Ld,
    Ldcz,
    Ldl,
    Ldr,
    Lh,
    Lhu,
    Ll,
    Lld,
    Lui,
    Lw,
    Lwcz,
    Lwl,
    Lwr,
    Lwu,
    Ori,
    Regimm,
    Sb,
    Sc,
    Scd,
    Sd,
    Sdcz,
    Sdl,
    Sdr,
    Sh,
    Slti,
    Sltiu,
    Sw,
    Swcz,
    Swl,
    Swr,
    Teqi,
    Tgei,
    Tgeiu,
    Tlti,
    Tltiu,
    Tnei,
    Xori,
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum JTypeOp {
    J,
    Jal,
}

#[derive(Debug, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum RTypeOp {
    Add,
    Addu,
    And,
    Break,
    Cfcz,
    Ctcz,
    Dadd,
    Daddu,
    Ddiv,
    Ddivu,
    Div,
    Divu,
    Dmfc0,
    Dmtc0,
    Dmult,
    Dmultu,
    Dsll,
    Dsllv,
    Dsll32,
    Dsra,
    Dsrav,
    Dsra32,
    Dsrl,
    Dsrlv,
    Dsrl32,
    Dsub,
    Dsubu,
    Eret,
    Jalr,
    Jr,
    Mfc0,
    Mfcz,
    Mfhi,
    Mflo,
    Mtc0,
    Mtcz,
    Mthi,
    Mtlo,
    Mult,
    Multu,
    Nor,
    Or,
    Sll,
    Sllv,
    Slt,
    Sltu,
    Special,
    Sra,
    Srav,
    Srl,
    Srlv,
    Sub,
    Subu,
    Sync,
    Syscall,
    Teq,
    Tge,
    Tgeu,
    Tlbp,
    Tlbr,
    Tlbwi,
    Tlbwr,
    Tlt,
    Tltu,
    Tne,
    Xor,
}

#[derive(Debug)]
pub enum CopRs {
    Mf,
    Dmf,
    Cf,
    Mt,
    Dmt,
    Ct,
    Bc,
}

#[derive(Debug)]
pub enum CopRt {
    Bcf,
    Bct,
    Bcfl,
    Bctl,
}
