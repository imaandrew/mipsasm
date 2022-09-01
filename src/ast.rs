use std::convert::{From, TryFrom};
use std::str::FromStr;
use strum_macros::EnumString;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RegParseError {
    #[error("invalid register `{0}`")]
    RegParseError(String),
}

#[derive(Debug)]
pub enum Target {
    Function(String),
    Label(String),
    Address(u32),
}

impl Target {
    pub fn as_u32(&self) -> u32 {
        match self {
            Target::Function(name) => {
                panic!("{}", name)
            }
            Target::Label(name) => {
                panic!("{}", name)
            }
            Target::Address(addr) => *addr,
        }
    }
}

#[derive(Debug)]
pub enum Immediate {
    Int(u16),
    Label(String),
}

impl Immediate {
    pub fn as_u32(&self) -> u32 {
        match self {
            Immediate::Int(i) => *i as u32,
            Immediate::Label(lbl) => panic!("{}", lbl),
        }
    }
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
        sa: u16,
    },
}

#[derive(Clone, Copy, Debug)]
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

impl TryFrom<u32> for Register {
    type Error = RegParseError;

    fn try_from(reg: u32) -> Result<Self, Self::Error> {
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
            e => Err(RegParseError::RegParseError(e.to_string())),
        }
    }
}

impl FromStr for Register {
    type Err = RegParseError;

    fn from_str(reg: &str) -> Result<Self, Self::Err> {
        match reg.trim_start_matches('$').to_lowercase().as_str() {
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
            e => Err(RegParseError::RegParseError(e.to_string())),
        }
    }
}

impl From<FloatRegister> for Register {
    fn from(reg: FloatRegister) -> Self {
        Register::try_from(reg as u32).unwrap()
    }
}

impl Register {
    pub fn as_num(&self) -> u32 {
        *self as u32
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FloatRegister {
    Fv0,
    Fv0f,
    Fv1,
    Fv1f,
    Ft0,
    Ft0f,
    Ft1,
    Ft1f,
    Ft2,
    Ft2f,
    Ft3,
    Ft3f,
    Fa0,
    Fa0f,
    Fa1,
    Fa1f,
    Ft4,
    Ft4f,
    Ft5,
    Ft5f,
    Fs0,
    Fs0f,
    Fs1,
    Fs1f,
    Fs2,
    Fs2f,
    Fs3,
    Fs3f,
    Fs4,
    Fs4f,
    Fs5,
    Fs5f,
}

impl TryFrom<i32> for FloatRegister {
    type Error = RegParseError;

    fn try_from(reg: i32) -> Result<Self, Self::Error> {
        match reg {
            0 => Ok(FloatRegister::Fv0),
            1 => Ok(FloatRegister::Fv0f),
            2 => Ok(FloatRegister::Fv1),
            3 => Ok(FloatRegister::Fv1f),
            4 => Ok(FloatRegister::Ft0),
            5 => Ok(FloatRegister::Ft0f),
            6 => Ok(FloatRegister::Ft1),
            7 => Ok(FloatRegister::Ft1f),
            8 => Ok(FloatRegister::Ft2),
            9 => Ok(FloatRegister::Ft2f),
            10 => Ok(FloatRegister::Ft3),
            11 => Ok(FloatRegister::Ft3f),
            12 => Ok(FloatRegister::Fa0),
            13 => Ok(FloatRegister::Fa0f),
            14 => Ok(FloatRegister::Fa1),
            15 => Ok(FloatRegister::Fa1f),
            16 => Ok(FloatRegister::Ft4),
            17 => Ok(FloatRegister::Ft4f),
            18 => Ok(FloatRegister::Ft5),
            19 => Ok(FloatRegister::Ft5f),
            20 => Ok(FloatRegister::Fs0),
            21 => Ok(FloatRegister::Fs0f),
            22 => Ok(FloatRegister::Fs1),
            23 => Ok(FloatRegister::Fs1f),
            24 => Ok(FloatRegister::Fs2),
            25 => Ok(FloatRegister::Fs2f),
            26 => Ok(FloatRegister::Fs3),
            27 => Ok(FloatRegister::Fs3f),
            28 => Ok(FloatRegister::Fs4),
            29 => Ok(FloatRegister::Fs4f),
            30 => Ok(FloatRegister::Fs5),
            31 => Ok(FloatRegister::Fs5f),
            e => Err(RegParseError::RegParseError(e.to_string())),
        }
    }
}

impl FromStr for FloatRegister {
    type Err = RegParseError;

    fn from_str(reg: &str) -> Result<Self, Self::Err> {
        match reg.trim_start_matches('$').to_lowercase().as_str() {
            "f0" | "fv0" => Ok(FloatRegister::Fv0),
            "f1" | "fv0f" => Ok(FloatRegister::Fv0f),
            "f2" | "fv1" => Ok(FloatRegister::Fv1),
            "f3" | "fv1f" => Ok(FloatRegister::Fv1f),
            "f4" | "ft0" => Ok(FloatRegister::Ft0),
            "f5" | "ft0f" => Ok(FloatRegister::Ft0f),
            "f6" | "ft1" => Ok(FloatRegister::Ft1),
            "f7" | "ft1f" => Ok(FloatRegister::Ft1f),
            "f8" | "ft2" => Ok(FloatRegister::Ft2),
            "f9" | "ft2f" => Ok(FloatRegister::Ft2f),
            "f10" | "ft3" => Ok(FloatRegister::Ft3),
            "f11" | "ft3f" => Ok(FloatRegister::Ft3f),
            "f12" | "fa0" => Ok(FloatRegister::Fa0),
            "f13" | "fa0f" => Ok(FloatRegister::Fa0f),
            "f14" | "fa1" => Ok(FloatRegister::Fa1),
            "f15" | "fa1f" => Ok(FloatRegister::Fa1f),
            "f16" | "ft4" => Ok(FloatRegister::Ft4),
            "f17" | "ft4f" => Ok(FloatRegister::Ft4f),
            "f18" | "ft5" => Ok(FloatRegister::Ft5),
            "f19" | "ft5f" => Ok(FloatRegister::Ft5f),
            "f20" | "fs0" => Ok(FloatRegister::Fs0),
            "f21" | "fs0f" => Ok(FloatRegister::Fs0f),
            "f22" | "fs1" => Ok(FloatRegister::Fs1),
            "f23" | "fs1f" => Ok(FloatRegister::Fs1f),
            "f24" | "fs2" => Ok(FloatRegister::Fs2),
            "f25" | "fs2f" => Ok(FloatRegister::Fs2f),
            "f26" | "fs3" => Ok(FloatRegister::Fs3),
            "f27" | "fs3f" => Ok(FloatRegister::Fs3f),
            "f28" | "fs4" => Ok(FloatRegister::Fs4),
            "f29" | "fs4f" => Ok(FloatRegister::Fs4f),
            "f30" | "fs5" => Ok(FloatRegister::Fs5),
            "f31" | "fs5f" => Ok(FloatRegister::Fs5f),
            e => Err(RegParseError::RegParseError(e.to_string())),
        }
    }
}

#[derive(Clone, Copy, Debug, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum ITypeOp {
    Addi,
    Addiu,
    Andi,
    Bc0f,
    Bc0fl,
    Bc0t,
    Bc0tl,
    Bc1f,
    Bc1fl,
    Bc1t,
    Bc1tl,
    Beq,
    Beql,
    Beqz,
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
    Bnez,
    Bnel,
    Cache,
    Daddi,
    Daddiu,
    Lb,
    Lbu,
    Ld,
    Ldc1,
    Ldc2,
    Ldl,
    Ldr,
    Lh,
    Lhu,
    Ll,
    Lld,
    Lui,
    Lw,
    Lwc1,
    Lwc2,
    Lwl,
    Lwr,
    Lwu,
    Ori,
    Sb,
    Sc,
    Scd,
    Sd,
    Sdc1,
    Sdc2,
    Sdl,
    Sdr,
    Sh,
    Slti,
    Sltiu,
    Sw,
    Swc1,
    Swc2,
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

#[derive(Clone, Copy, Debug, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum JTypeOp {
    J,
    Jal,
}

#[derive(Clone, Copy, Debug, EnumString)]
#[strum(ascii_case_insensitive)]
pub enum RTypeOp {
    AbsS,
    AbsD,
    Add,
    Addu,
    AddS,
    AddD,
    And,
    Break,
    Cs,
    Cd,
    CeilLS,
    CeilLD,
    CeilWS,
    CeilWD,
    Cfc1,
    Ctc1,
    CvtDS,
    CvtDW,
    CvtDL,
    CvtLS,
    CvtLD,
    CvtSD,
    CvtSW,
    CvtSL,
    CvtWS,
    CvtWD,
    Dadd,
    Daddu,
    Ddiv,
    Ddivu,
    Div,
    Divu,
    DivS,
    DivD,
    Dmfc0,
    Dmfc1,
    Dmtc0,
    Dmtc1,
    Dmult,
    Dmultu,
    Dsll,
    Dsll32,
    Dsllv,
    Dsra,
    Dsra32,
    Dsrav,
    Dsrl,
    Dsrl32,
    Dsrlv,
    Dsub,
    Dsubu,
    Eret,
    FloorLS,
    FloorLD,
    FloorWS,
    FloorWD,
    Jalr,
    Jr,
    Mfc0,
    Mfc1,
    Mfhi,
    Mflo,
    MovS,
    MovD,
    Mtc0,
    Mtc1,
    Mthi,
    Mtlo,
    MulS,
    MulD,
    Mult,
    Multu,
    NegS,
    NegD,
    Negu,
    Nor,
    Or,
    RoundLS,
    RoundLD,
    RoundWS,
    RoundWD,
    Sll,
    Sllv,
    Slt,
    Sltu,
    SqrtS,
    SqrtD,
    Sra,
    Srav,
    Srl,
    Srlv,
    Sub,
    Subu,
    SubS,
    SubD,
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
    TruncLS,
    TruncLD,
    TruncWS,
    TruncWD,
    Xor,
}
