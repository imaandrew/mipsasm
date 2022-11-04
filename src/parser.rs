use crate::ast;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::Write;
use std::{cmp, fmt, mem};

// Regex to match anything after a comment
static COMMENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(/{2}|;).*").unwrap());

macro_rules! error {
    ($self:ident, MultipleLabelDefinition, $label:expr, $first:expr) => {
        ParserError::MultipleLabelDefinition {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            label: $label.to_string(),
            first: Line {
                num: $first + 1,
                content: $self.input.get($first).unwrap().to_string(),
            },
        }
    };
    ($self:ident, InvalidLabel, $label:expr) => {
        ParserError::InvalidLabel {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            label: $label.to_string(),
        }
    };
    ($self:ident, InvalidInstruction) => {
        ParserError::InvalidInstruction {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
        }
    };
    ($self:ident, InvalidOperandCount, $ops:expr, $expected:expr, $found:expr) => {
        ParserError::InvalidOperandCount {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            ops: $ops.to_string(),
            expected: $expected,
            found: $found,
        }
    };
    ($self:ident, InvalidOpcode, $opcode:expr) => {
        ParserError::InvalidOpcode {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            opcode: $opcode.to_string(),
        }
    };
    ($self:ident, InvalidRegister, $register:expr) => {
        ParserError::InvalidRegister {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            register: $register.to_string(),
        }
    };
    ($self:ident, InvalidTargetAddress, $target:expr) => {
        ParserError::InvalidTargetAddress {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            address: $target.to_string(),
        }
    };
    ($self:ident, InvalidImmediate, $immediate:expr) => {
        ParserError::InvalidImmediate {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            immediate: $immediate.to_string(),
        }
    };
    ($self:ident, InvalidFloatCond, $cond:expr) => {
        ParserError::InvalidFloatCond {
            line: Line {
                num: $self.line_num,
                content: $self.input.get($self.line_num - 1).unwrap().to_string(),
            },
            cond: $cond.to_string(),
        }
    };
    ($self:ident, BranchOutOfBounds, $line:expr, $target:expr, $bounds:expr) => {
        ParserError::BranchOutOfBounds {
            line: Line {
                num: $line,
                content: $self.input.get($line - 1).unwrap().to_string(),
            },
            branch: $target,
            bounds: $bounds,
        }
    };
}

#[derive(Debug)]
pub struct Line {
    num: usize,
    content: String,
}

#[derive(Debug)]
pub enum ParserError {
    // #[error("label `{0}` defined multiple times")]
    MultipleLabelDefinition {
        line: Line,
        label: String,
        first: Line,
    },
    //#[error("label `{0}` must start with a letter")]
    InvalidLabel {
        line: Line,
        label: String,
    },
    //#[error("invalid instruction `{0}`")]
    InvalidInstruction {
        line: Line,
    },
    //#[error("invalid number of operands `{line}`\n Expected {expected} operands, found {found}")]
    InvalidOperandCount {
        line: Line,
        expected: usize,
        found: usize,
        ops: String,
    },
    //#[error("invalid opcode `{0}`")]
    InvalidOpcode {
        line: Line,
        opcode: String,
    },
    //#[error("invalid register `{0}`")]
    InvalidRegister {
        line: Line,
        register: String,
    },
    //#[error("invalid target address `{0}`")]
    InvalidTargetAddress {
        line: Line,
        address: String,
    },
    //#[error("invalid immediate `{0}`")]
    InvalidImmediate {
        line: Line,
        immediate: String,
    },
    //#[error("invalid float compare condition `{0}`")]
    InvalidFloatCond {
        line: Line,
        cond: String,
    },
    //#[error("branch `{0}` out of bounds")]
    BranchOutOfBounds {
        line: Line,
        branch: String,
        bounds: (u32, u32),
    },
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::MultipleLabelDefinition {
                line: Line { num, content },
                label,
                first:
                    Line {
                        num: first_num,
                        content: first_content,
                    },
            } => {
                let margin = cmp::max(num.to_string().len(), first_num.to_string().len());
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: label `{}` defined multiple times",
                    label
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *first_num,
                        first_content,
                        margin,
                        true,
                        "first defined here",
                        true,
                        label
                    )
                )?;
                writeln!(f, "\x1b[94m...\x1b[0m")?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "redefined here", false, label)
                )
            }
            Self::InvalidLabel {
                line: Line { num, content },
                label,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: label `{}` must start with a letter",
                    label
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "defined here", true, label)
                )
            }
            Self::InvalidInstruction {
                line: Line { num, content },
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: invalid instruction `{}`",
                    content.trim()
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, content.trim())
                )
            }
            Self::InvalidOperandCount {
                line: Line { num, content },
                ops,
                expected,
                found,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: invalid number of operands `{}`",
                    content
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *num,
                        content,
                        margin,
                        false,
                        format!("expected {} operands, found {}", expected, found).as_str(),
                        true,
                        ops
                    )
                )
            }
            Self::InvalidOpcode {
                line: Line { num, content },
                opcode,
            } => {
                let margin = num.to_string().len();
                writeln!(f, "\x1b[91merror\x1b[0m: invalid opcode `{}`", opcode)?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, opcode)
                )
            }
            Self::InvalidRegister {
                line: Line { num, content },
                register,
            } => {
                let margin = num.to_string().len();
                writeln!(f, "\x1b[91merror\x1b[0m: invalid register `{}`", register)?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, register)
                )
            }
            Self::InvalidTargetAddress {
                line: Line { num, content },
                address,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: invalid target address `{}`",
                    address
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, address)
                )
            }
            Self::InvalidImmediate {
                line: Line { num, content },
                immediate,
            } => {
                let margin = num.to_string().len();
                writeln!(f, "\x1b[91merror\x1b[0m: invalid immediate `{}`", immediate)?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, immediate)
                )
            }
            Self::InvalidFloatCond {
                line: Line { num, content },
                cond,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: invalid float compare condition `{}`",
                    cond
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, cond)
                )
            }
            Self::BranchOutOfBounds {
                line: Line { num, content },
                branch,
                bounds,
            } => {
                let margin = num.to_string().len();
                writeln!(f, "\x1b[91merror\x1b[0m: branch `{}` out of bounds", branch)?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *num,
                        content,
                        margin,
                        false,
                        &format!(
                            "should be between 0x{:08x?} and 0x{:08x?}",
                            bounds.0, bounds.1
                        ),
                        true,
                        branch
                    )
                )
            }
        }
    }
}

fn fmt_line(
    num: usize,
    content: &str,
    margin: usize,
    err_underline: bool,
    msg: &str,
    first_space: bool,
    underline: &str,
) -> String {
    let mut s = String::new();
    let underline_start = content
        .find(underline)
        .unwrap_or_else(|| panic!("{}", content));
    if first_space {
        writeln!(s, "\x1b[94m{:>margin$} |\x1b[0m", "").unwrap();
    }
    writeln!(s, "\x1b[94m{:>margin$} |\x1b[0m {}", num, content).unwrap();
    if err_underline {
        writeln!(
            s,
            "\x1b[94m{:>margin$} | {: <start$}{:-<len$} {msg}\x1b[0m",
            "",
            "",
            "",
            len = underline.len(),
            start = underline_start
        )
        .unwrap();
    } else {
        writeln!(
            s,
            "\x1b[94m{:>margin$} | {: <start$}\x1b[91m{:^<len$} {msg}\x1b[0m",
            "",
            "",
            "",
            len = underline.len(),
            start = underline_start
        )
        .unwrap();
    }
    write!(s, "\x1b[94m{:>margin$} |\x1b[0m", "").unwrap();
    s
}

pub struct Parser<'a> {
    input: Vec<&'a str>,
    insts: Vec<(usize, ast::Instruction)>,
    labels: HashMap<String, usize>,
    base_addr: u32,
    syms: &'a HashMap<&'a str, u32>,
    line_num: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, base_addr: u32, syms: &'a HashMap<&'a str, u32>) -> Parser<'a> {
        Parser {
            input: input.lines().collect(),
            insts: vec![],
            labels: HashMap::new(),
            base_addr,
            syms,
            line_num: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Instruction>, ParserError> {
        for i in 0..self.input.len() {
            self.line_num += 1;
            let l = COMMENT_RE.replace_all(self.input.get(i).unwrap().trim(), "");
            self.scan_line(&l)?;
        }
        self.adjust_labels()?;
        Ok(mem::take(&mut self.insts)
            .into_iter()
            .map(|(_, i)| i)
            .collect())
    }

    fn scan_line(&mut self, line: &str) -> Result<(), ParserError> {
        if line.ends_with(':') {
            self.labels
                .insert(self.parse_label(line)?, self.insts.len());
        } else if !line.is_empty() {
            self.insts.push((self.line_num, self.parse_inst(line)?));
        }

        Ok(())
    }

    fn parse_label(&self, line: &str) -> Result<String, ParserError> {
        let label = line.trim_end_matches(':').to_string();
        if label.chars().next().unwrap().is_numeric() {
            return Err(error!(self, InvalidLabel, label));
        }
        if self.labels.contains_key(&label) {
            return Err(error!(
                self,
                MultipleLabelDefinition,
                label,
                *self.labels.get(&label).unwrap()
            ));
        }
        Ok(label)
    }

    fn parse_inst(&self, line: &str) -> Result<ast::Instruction, ParserError> {
        let (op, arg) = match line.split_once(' ') {
            Some((op, arg)) => (op, arg),
            None => (line, ""),
        };

        if line.is_empty() {
            return Err(error!(self, InvalidInstruction));
        }

        let args = arg.split(',').collect::<Vec<&str>>();

        let offset_regex = Regex::new(r".+\s*\(").unwrap();
        let base_regex = Regex::new(r"\(.*?\)").unwrap();

        match op.to_lowercase().trim() {
            // -----------------------------------------------------------------
            // |    op     |  base   |   rt    |             offset            |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, offset(base)
            "cache" | "lb" | "lbu" | "ld" | "ldl" | "ldr" | "lh" | "lhu" | "ll" | "lld" | "lw"
            | "lwl" | "lwr" | "lwu" | "sb" | "sc" | "scd" | "sd" | "sdl" | "sdr" | "sh" | "sw"
            | "swl" | "swr" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = if op.to_lowercase().trim() == "cache" {
                    ast::Register::try_from(
                        self.parse_immediate::<u16>(args.first().unwrap())?.as_u32(),
                    )
                    .unwrap()
                } else {
                    args.first().unwrap().parse().unwrap()
                };
                let x = args.get(1).unwrap();
                let base = base_regex
                    .find_iter(x)
                    .last()
                    .unwrap()
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .unwrap();
                if let Some(x) = offset_regex.find(x) {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rs: base,
                        rt,
                        imm: self.parse_immediate::<i16>(&x.as_str()[..x.as_str().len() - 1])?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rs: base,
                        rt,
                        imm: self.parse_immediate::<i16>("0")?,
                    })
                }
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |          immediate            |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, rs, immediate
            "addi" | "addiu" | "andi" | "daddi" | "daddiu" | "ori" | "slti" | "sltiu" | "xori"
            | "dsubi" | "dsubiu" | "subi" | "subiu" => {
                if args.len() != 3 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rs = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(2).unwrap();
                if op == "andi" || op == "ori" || op == "xori" {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rt,
                        rs,
                        imm: self.parse_immediate::<u16>(imm)?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rt,
                        rs,
                        imm: self.parse_immediate::<i16>(imm)?,
                    })
                }
            }
            // -----------------------------------------------------------------
            // |    op     |  00000  |   rt    |           immediate           |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, immediate
            "lui" | "lli" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt,
                    rs: ast::Register::null(),
                    imm: self.parse_immediate::<u16>(imm)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |  00000  |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, offset
            "teqi" | "tgei" | "tgeiu" | "tlti" | "tltiu" | "tnei" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt: ast::Register::null(),
                    rs,
                    imm: self.parse_immediate::<i16>(imm)?,
                })
            }
            "bgez" | "bgezal" | "bgezall" | "bgezl" | "bltz" | "bltzal" | "bltzall" | "bltzl"
            | "beqz" | "bnez" | "beqzl" | "bnezl" | "bgtz" | "bgtzl" | "blez" | "blezl" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt: ast::Register::null(),
                    rs,
                    imm: self.parse_immediate::<u32>(imm)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, rt, offset
            "beq" | "beql" | "bne" | "bnel" | "bge" | "bgt" | "ble" | "blt" | "bgeu" | "bgtu"
            | "bleu" | "bltu" | "bgel" | "bgtl" | "blel" | "bltl" | "bgeul" | "bgtul" | "bleul"
            | "bltul" => {
                if args.len() != 3 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(2).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt,
                    rs,
                    imm: self.parse_immediate::<u32>(imm)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |                       target                      |
            // ------6-------------------------------26-------------------------
            //  Format:  op target
            "j" | "jal" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                let target = args.first().unwrap().trim();
                Ok(ast::Instruction::Jump {
                    op: op.parse().unwrap(),
                    target: self.parse_target(target)?,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |      0000 0000 0000 000     |  stype  |    op     |
            // ------6-------------------15-------------------5---------6-------
            //  Format:  op          (stype = 0 implied)
            "nop" | "sync" => Ok(ast::Instruction::Register {
                op: op.parse().unwrap(),
                rd: ast::Register::null(),
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                sa: 0,
            }),
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rs, rt
            "add" | "addu" | "and" | "dadd" | "daddu" | "dsub" | "dsubu" | "nor" | "or" | "slt"
            | "sltu" | "sub" | "subu" | "xor" | "dmul" | "dmulu" | "dmulo" | "dmulou" | "drem"
            | "dremu" | "drol" | "dror" | "mul" | "mulu" | "mulo" | "mulou" | "rem" | "remu"
            | "seq" | "sge" | "sgeu" | "sgt" | "sgtu" | "sle" | "sleu" | "sne" => {
                if args.len() != 3 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rs = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(2).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd,
                    rs,
                    rt,
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |  00000  |   rt    |    rd   |   sa    |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rt, sa
            "dsll" | "dsll32" | "dsra" | "dsra32" | "dsrl" | "dsrl32" | "sll" | "sra" | "srl" => {
                if args.len() != 3 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let sa = args.get(2).unwrap().trim();
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd,
                    rs: ast::Register::null(),
                    rt,
                    sa: if sa.ends_with('`') || !sa.contains("0x") {
                        sa.trim_end_matches('`').parse::<i32>().unwrap() as u32
                    } else {
                        let sa = sa.replace("0x", "");
                        i32::from_str_radix(&sa, 16).unwrap() as u32
                    },
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |    rd   |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rt, rs
            "dsllv" | "dsrav" | "dsrlv" | "sllv" | "srav" | "srlv" => {
                if args.len() != 3 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rs = args.get(2).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd,
                    rs,
                    rt,
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |                   code                |    op     |
            // ------6--------------------------20-----------------------6------
            //  Format:  op offset
            "break" | "syscall" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                let code = if args.first().unwrap().is_empty() {
                    ast::Immediate::Short(0)
                } else if !args.first().unwrap().is_empty() {
                    self.parse_immediate::<u16>(args.first().unwrap().trim())?
                } else {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                };

                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd: ast::Register::null(),
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    sa: code.as_u32(),
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |   0000 0000 00    |    op     |
            // ------6----------5---------5--------------10--------------6------
            //  Format:  op rs, rt
            "dmult" | "dmultu" | "mult" | "multu" | "teq" | "tge" | "tgeu" | "tlt" | "tltu"
            | "tne" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd: ast::Register::null(),
                    rs,
                    rt,
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |  00000  |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rs
            "jalr" => {
                if args.len() != 2 && args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                if args.len() == 1 {
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rd: ast::Register::Ra,
                        rs,
                        rt: ast::Register::null(),
                        sa: 0,
                    })
                } else {
                    let rd = args.get(1).unwrap().parse().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rd: rs,
                        rs: rd,
                        rt: ast::Register::null(),
                        sa: 0,
                    })
                }
            }
            "abs" | "dabs" | "dmove" | "dneg" | "dnegu" | "move" | "neg" | "negu" | "not" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rs = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd,
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            "b" | "bal" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }

                let imm = args.first().unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    imm: self.parse_immediate::<u32>(imm)?,
                })
            }
            "li" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt,
                    imm: self.parse_immediate::<i32>(imm)?,
                })
            }
            "ddiv" | "ddivu" | "div" | "divu" => {
                if args.len() != 3 && args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rs = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                if args.len() == 2 {
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rd,
                        rs,
                        rt: ast::Register::null(),
                        sa: 0,
                    })
                } else {
                    let rt = args.get(2).unwrap().parse().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rd,
                        rs,
                        rt,
                        sa: 0,
                    })
                }
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |     0000 0000 0000 000      |    op     |
            // ------6----------5------------------15--------------------6------
            //  Format:  op rs
            "jr" | "mthi" | "mtlo" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd: ast::Register::null(),
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   0000 0000 00    |   rd    |  00000  |    op     |
            // ------6---------------10-------------5---------5----------6------
            //  Format:  op rd
            "clear" | "mfhi" | "mflo" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                let rd = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rd,
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            "dli" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt,
                    imm: self.parse_immediate::<i64>(imm)?,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |    bc    |           offset             |
            // ------6----------5----------5------------------16----------------
            //  Format:  op offset
            "bc0f" | "bc1f" | "bc0fl" | "bc1fl" | "bc0t" | "bc1t" | "bc0tl" | "bc1tl" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                let offset = args.first().unwrap();
                Ok(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    imm: self.parse_immediate::<u32>(offset)?,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |   rt    |   rd    |    0000 0000 000    |
            // ------6----------5---------5---------5--------------11-----------
            //  Format:  op rt, rd
            "cfc0" | "ctc0" | "dmfc0" | "dmtc0" | "mfc0" | "mtc0" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rd = args.get(1).unwrap().parse::<ast::Cop0Register>().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt,
                    rd: rd.into(),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |   rt    |   fs    |    0000 0000 000    |
            // ------6----------5---------5---------5--------------11-----------
            //  Format:  op rt, fs
            "cfc1" | "ctc1" | "dmfc1" | "dmtc1" | "mfc1" | "mtc1" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rd = args.get(1).unwrap().parse::<ast::FloatRegister>().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(ast::Instruction::Register {
                    op: op.parse().unwrap(),
                    rs: ast::Register::null(),
                    rt,
                    rd: rd.into(),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |CO|      0000 0000 0000 0000 000       |    op     |
            // ------6------1-------------------19-----------------------6------
            //  Format:  op
            "eret" | "tlbp" | "tlbr" | "tlbwi" | "tlbwr" => Ok(ast::Instruction::Register {
                op: op.parse().unwrap(),
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                rd: ast::Register::null(),
                sa: 0,
            }),
            // -----------------------------------------------------------------
            // |    op     |   base  |   ft    |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op ft, offset(base)
            "ldc1" | "lwc1" | "sdc1" | "swc1" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let ft = args
                    .first()
                    .unwrap()
                    .parse::<ast::FloatRegister>()
                    .map_err(|ast::RegParseError::RegParseError(e)| {
                        error!(self, InvalidRegister, e)
                    })?;
                let x = args.get(1).unwrap();
                let base = base_regex
                    .find_iter(x)
                    .last()
                    .unwrap()
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .map_err(|ast::RegParseError::RegParseError(e)| {
                        error!(self, InvalidRegister, e)
                    })?;
                if let Some(x) = offset_regex.find(x) {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rs: base,
                        rt: ast::Register::from(ft),
                        imm: self.parse_immediate::<i16>(&x.as_str().replace('(', ""))?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rs: base,
                        rt: ast::Register::from(ft),
                        imm: self.parse_immediate::<i16>("0")?,
                    })
                }
            }
            _ => match &op.to_lowercase()[..op.len() - 2] {
                // -----------------------------------------------------------------
                // |   COP1    |   fmt   |   ft    |   fs    |   fd    |    op     |
                // ------6----------5---------5---------5---------5----------6------
                //  Format:  op.fmt fd, fs, ft
                "add" | "sub" | "mul" | "div" => {
                    if args.len() != 3 {
                        return Err(error!(self, InvalidOperandCount, arg, 3, args.len()));
                    }
                    let fd = args
                        .first()
                        .unwrap()
                        .parse::<ast::FloatRegister>()
                        .map_err(|ast::RegParseError::RegParseError(e)| {
                            error!(self, InvalidRegister, e)
                        })?;
                    let fs = args.get(1).unwrap().parse::<ast::FloatRegister>().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    let ft = args.get(2).unwrap().parse::<ast::FloatRegister>().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rs: ast::Register::from(fs),
                        rt: ast::Register::from(ft),
                        rd: ast::Register::from(fd),
                        sa: 0,
                    })
                }
                // -----------------------------------------------------------------
                // |   COP1    |   fmt   |  00000  |   fs    |   fd    |    op     |
                // ------6----------5---------5---------5---------5----------6------
                //  Format:  op.fmt fd, fs
                "abs" | "ceil.l" | "ceil.w" | "cvt.d" | "cvt.l" | "cvt.s" | "cvt.w" | "floor.l"
                | "floor.w" | "mov" | "neg" | "round.l" | "round.w" | "sqrt" | "trunc.l"
                | "trunc.w" => {
                    if args.len() != 2 {
                        return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                    }
                    let fd = args
                        .first()
                        .unwrap()
                        .parse::<ast::FloatRegister>()
                        .map_err(|ast::RegParseError::RegParseError(e)| {
                            error!(self, InvalidRegister, e)
                        })?;
                    let fs = args.get(1).unwrap().parse::<ast::FloatRegister>().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(ast::Instruction::Register {
                        op: op.parse().unwrap(),
                        rs: ast::Register::from(fs),
                        rt: ast::Register::null(),
                        rd: ast::Register::from(fd),
                        sa: 0,
                    })
                }
                e => {
                    // -----------------------------------------------------------------
                    // |   COP1    |   fmt   |   ft    |   fs    | 000 |00 |11 | cond  |
                    // ------6----------5---------5---------5-------3----2---2-----4----
                    //  Format:  C.cond.fmt fs, ft
                    if e.starts_with("c.") {
                        if args.len() != 2 {
                            return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                        }
                        let fs = args
                            .first()
                            .unwrap()
                            .parse::<ast::FloatRegister>()
                            .map_err(|ast::RegParseError::RegParseError(e)| {
                                error!(self, InvalidRegister, e)
                            })?;
                        let ft = args.get(1).unwrap().parse::<ast::FloatRegister>().map_err(
                            |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                        )?;
                        return Ok(ast::Instruction::Register {
                            op: format!("C.{}", op.chars().last().unwrap()).parse().unwrap(),
                            rs: ast::Register::from(fs),
                            rt: ast::Register::from(ft),
                            rd: ast::Register::null(),
                            sa: self.parse_float_cond(
                                op.split('.').collect::<Vec<&str>>().get(1).unwrap(),
                            )?,
                        });
                    }
                    Err(error!(self, InvalidOpcode, op))
                }
            },
        }
    }

    // Convert each label to an absolute immediate or address
    fn adjust_labels(&mut self) -> Result<(), ParserError> {
        for i in 0..self.insts.len() {
            if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::Label(lbl),
            } = &self.insts[i].1
            {
                let lbl_addr = self.labels.get(lbl.as_str()).unwrap();
                self.insts[i].1 = ast::Instruction::Immediate {
                    op: *op,
                    rs: *rs,
                    rt: *rt,
                    // Calculate the offset from the label to the current instruction
                    imm: ast::Immediate::Short((*lbl_addr as isize - (i + 1) as isize) as u16),
                };
            } else if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::Int(addr),
            } = &self.insts[i].1
            {
                if !op.to_string().starts_with('b') {
                    continue;
                }

                // Make sure the address is within the bounds of the program
                if *addr < self.base_addr || *addr > self.base_addr + self.insts.len() as u32 * 4 {
                    return Err(error!(
                        self,
                        BranchOutOfBounds,
                        self.insts[i].0,
                        self.input.get(self.insts[i].0 - 1).unwrap().to_string(),
                        (self.base_addr, self.base_addr + self.insts.len() as u32 * 4)
                    ));
                }

                let offset = *addr as isize - (i + 1) as isize;
                self.insts[i].1 = ast::Instruction::Immediate {
                    op: *op,
                    rs: *rs,
                    rt: *rt,
                    imm: ast::Immediate::Short(((offset / 4) as i16) as u16),
                };
            } else if let ast::Instruction::Jump {
                op,
                target: ast::Target::Label(lbl),
            } = &self.insts[i].1
            {
                let lbl_addr = self.labels.get(lbl.as_str()).unwrap();
                self.insts[i].1 = ast::Instruction::Jump {
                    op: *op,
                    target: ast::Target::Address(self.base_addr + *lbl_addr as u32 * 4),
                };
            }
        }

        Ok(())
    }

    fn parse_immediate<T>(&self, imm: &str) -> Result<ast::Immediate, ParserError>
    where
        T: num::PrimInt + std::str::FromStr,
    {
        let imm = imm.trim();

        if self.labels.contains_key(imm) {
            return Ok(ast::Immediate::Label(imm.to_string()));
        }

        let imm_regex = Regex::new(r"\(.*\)").unwrap();
        if let Some(x) = imm_regex.find(imm) {
            let x = self.parse_target(&x.as_str().replace(&['(', ')'][..], ""))?;
            match &imm[..3] {
                "%hi" => {
                    return Ok(ast::Immediate::new(
                        ((x.as_u32() + (x.as_u32() & 0x8000) * 2) >> 16) as u16,
                    ))
                }
                "%lo" => return Ok(ast::Immediate::new((x.as_u32() & 0xffff) as u16)),
                _ => todo!(),
            }
        }

        if imm.contains("0x") {
            let imm = imm.replace("0x", "");
            Ok(ast::Immediate::new::<T>(
                T::from_str_radix(&imm, 16).map_err(|_| error!(self, InvalidImmediate, imm))?,
            ))
        } else {
            Ok(ast::Immediate::new(
                imm.parse::<T>()
                    .map_err(|_| error!(self, InvalidImmediate, imm))?,
            ))
        }
    }

    fn parse_target(&self, target: &str) -> Result<ast::Target, ParserError> {
        if let Some(x) = self.syms.get(target) {
            return Ok(ast::Target::Address(*x));
        }

        if target.starts_with("0x") {
            let target = target.replace("0x", "");
            Ok(ast::Target::Address(
                u32::from_str_radix(&target, 16)
                    .map_err(|_| error!(self, InvalidTargetAddress, target))?,
            ))
        } else if target[0..1].parse::<u32>().is_ok() {
            Ok(ast::Target::Address(
                target
                    .parse::<u32>()
                    .map_err(|_| error!(self, InvalidTargetAddress, target))?,
            ))
        } else {
            Ok(ast::Target::Label(target.to_string()))
        }
    }

    fn parse_float_cond(&self, cond: &str) -> Result<u32, ParserError> {
        match cond.to_lowercase().as_str() {
            "f" => Ok(0),
            "un" => Ok(1),
            "eq" => Ok(2),
            "ueq" => Ok(3),
            "olt" => Ok(4),
            "ult" => Ok(5),
            "ole" => Ok(6),
            "ule" => Ok(7),
            "sf" => Ok(8),
            "ngle" => Ok(9),
            "seq" => Ok(10),
            "ngl" => Ok(11),
            "lt" => Ok(12),
            "nge" => Ok(13),
            "le" => Ok(14),
            "ngt" => Ok(15),
            _ => Err(error!(self, InvalidFloatCond, cond)),
        }
    }
}
