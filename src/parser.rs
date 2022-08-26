use crate::ast;
use regex::Regex;
use std::collections::HashMap;
use std::num::ParseIntError;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("label `{0}` defined multiple times")]
    MultipleLabelDefinition(String),
    #[error("invalid instruction `{0}`")]
    InvalidInstruction(String),
    #[error("invalid number of operands `{line}`\n Expected {expected} operands, found {found}")]
    InvalidOperandCount {
        line: String,
        expected: usize,
        found: usize,
    },
    #[error("invalid opcode `{0}`")]
    InvalidOpcode(#[from] strum::ParseError),
    #[error("invalid register `{0}`")]
    InvalidRegister(String),
    #[error("invalid target address `{0}`")]
    InvalidTargetAddress(String),
    //#[error("invalid immediate `{0}`")]
    //InvalidImmediate(#[from] ParseIntError),
    #[error("invalid coprocessor `{0}`")]
    InvalidCopNumber(String),
    #[error("invalid coprocessor sub-opcode `{0}`")]
    InvalidCopSubOpcode(String),
    #[error("invalid float compare condition `{0}`")]
    InvalidFloatCond(String),
}

pub fn scan(input: &str, base_addr: u32, syms: HashMap<String, u32>) -> Result<Vec<ast::Instruction>, ParserError> {
    let mut parser = Parser::new(input, base_addr, syms);
    parser.scan()?;
    parser.adjust_labels();
    Ok(parser.insts)
}

struct Parser<'a> {
    input: &'a str,
    insts: Vec<ast::Instruction>,
    labels: HashMap<&'a str, isize>,
    base_addr: u32,
    syms: HashMap<String, u32>,
}

impl<'a> Parser<'a> {
    fn new(input: &str, base_addr: u32, syms: HashMap<String, u32>) -> Parser {
        Parser {
            input,
            insts: vec![],
            labels: HashMap::new(),
            base_addr,
            syms,
        }
    }

    fn scan(&mut self) -> Result<(), ParserError> {
        for line in self.input.lines() {
            self.scan_line(line)?;
        }
        Ok(())
    }

    fn scan_line(&mut self, line: &'a str) -> Result<(), ParserError> {
        if line.starts_with('.') {
            self.labels
                .insert(self.parse_label(line)?, self.insts.len() as isize + 1);
        } else if !line.is_empty() {
            self.insts.push(self.parse_inst(line)?);
        }

        Ok(())
    }

    fn parse_label(&self, label: &'a str) -> Result<&'a str, ParserError> {
        if self.labels.contains_key(&label) {
            return Err(ParserError::MultipleLabelDefinition(label.to_string()));
        }
        Ok(label)
    }

    fn parse_inst(&self, inst: &'a str) -> Result<ast::Instruction, ParserError> {
        let mut line = inst.split_whitespace();
        let op = match line.next() {
            Some(x) => x,
            None => return Err(ParserError::InvalidInstruction(inst.to_string())),
        };
        let args = line.collect::<String>();
        let args = args.split(',').collect::<Vec<&str>>();

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
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rt = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .unwrap();
                let x = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?;
                let base = base_regex
                    .find_iter(x)
                    .last()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .unwrap();
                if let Some(x) = offset_regex.find(x) {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rs: base,
                        rt,
                        imm: self.parse_immediate(x.as_str()[.. x.as_str().len() - 1].trim(), true)?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rs: base,
                        rt,
                        imm: self.parse_immediate("0", true)?,
                    })
                }
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |          immediate            |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, rs, immediate
            "addi" | "addiu" | "andi" | "daddi" | "daddiu" | "ori" | "slti" | "sltiu" | "xori" => {
                if args.len() != 3 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 3,
                        found: args.len(),
                    });
                }
                let rt = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rs = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                if op == "andi" || op == "ori" || op == "xori" {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt,
                        rs,
                        imm: self.parse_immediate(imm, false)?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt,
                        rs,
                        imm: self.parse_immediate(imm, true)?,
                    })
                }
            }
            // -----------------------------------------------------------------
            // |    op     |  00000  |   rt    |           immediate           |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, immediate
            "lui" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rt = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt,
                    rs: ast::Register::null(),
                    imm: self.parse_immediate(imm, true)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |  00000  |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, offset
            "beqz" | "bgtz" | "bgtzl" | "blez" | "blezl" | "bnez" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt: ast::Register::null(),
                    rs,
                    imm: self.parse_immediate(imm, true)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, rt, offset
            "beq" | "beql" | "bne" | "bnel" => {
                if args.len() != 3 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 3,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rt = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt,
                    rs,
                    imm: self.parse_immediate(imm, true)?,
                })
            }
            // -----------------------------------------------------------------
            // |    op     |                       target                      |
            // ------6-------------------------------26-------------------------
            //  Format:  op target
            "j" | "jal" => {
                if args.len() != 1 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let target = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Jump {
                    op: op.parse()?,
                    target: self.parse_target(target)?,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |      0000 0000 0000 000     |  stype  |    op     |
            // ------6-------------------15-------------------5---------6-------
            //  Format:  op          (stype = 0 implied)
            "sync" => Ok(ast::Instruction::Register {
                op: op.parse()?,
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
            | "sltu" | "sub" | "subu" | "xor" => {
                if args.len() != 3 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 3,
                        found: args.len(),
                    });
                }
                let rd = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rs = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rt = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
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
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 3,
                        found: args.len(),
                    });
                }
                let rd = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rt = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let sa = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rd,
                    rs: ast::Register::null(),
                    rt,
                    sa: if sa.ends_with('`') || !sa.contains("0x") {
                        sa.trim_end_matches('`').parse::<i32>().unwrap() as u16
                    } else {
                        let sa = sa.replace("0x", "");
                        i32::from_str_radix(&sa, 16).unwrap() as u16
                    },
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |    rd   |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rt, rs
            "dsllv" | "dsrav" | "dsrlv" | "sllv" | "srav" | "srlv" => {
                if args.len() != 3 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 3,
                        found: args.len(),
                    });
                }
                let rd = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rt = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rs = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
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
            "break" | "syscall" => Ok(ast::Instruction::Register {
                op: op.parse()?,
                rd: ast::Register::null(),
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                sa: 0,
            }),
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |   0000 0000 00    |    op     |
            // ------6----------5---------5--------------10--------------6------
            //  Format:  op rs, rt
            "ddiv" | "ddivu" | "div" | "divu" | "dmult" | "dmultu" | "mult" | "multu" | "teq"
            | "tge" | "tgeu" | "tlt" | "tltu" | "tne" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rt = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rd: ast::Register::null(),
                    rs,
                    rt,
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |  00000  |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rs, rd
            "jalr" => {
                if args.len() != 2 && args.len() != 1 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rd = match args.get(1) {
                    Some(x) => x.trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?,
                    None => ast::Register::null(),
                };
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rd,
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |     0000 0000 0000 000      |    op     |
            // ------6----------5------------------15--------------------6------
            //  Format:  op rs
            "jr" | "mthi" | "mtlo" => {
                if args.len() != 1 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
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
            "mfhi" | "mflo" => {
                if args.len() != 1 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let rd = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rd,
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |  REGIMM   |   rs    |   op    |           immediate           |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, immediate
            "bgez" | "bgezal" | "bgezall" | "bgezl" | "bltz" | "bltzal" | "bltzall" | "bltzl"
            | "teqi" | "tgei" | "tgeiu" | "tlti" | "tltiu" | "tnei" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rs,
                    rt: ast::Register::null(),
                    imm: self.parse_immediate(imm, true)?,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |    bc    |           offset             |
            // |           |         |          |                              |
            // ------6----------5----------5------------------16----------------
            //  Format:  op offset
            "bc0f" | "bc1f" | "bc0fl" | "bc1fl" | "bc0t" | "bc1t" | "bc0tl" | "bc1tl" => {
                if args.len() != 1 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 1,
                        found: args.len(),
                    });
                }
                let offset = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    imm: self.parse_immediate(offset, true)?,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |   rt    |   rd    |    0000 0000 000    |
            // ------6----------5---------5---------5--------------11-----------
            //  Format:  op rt, rd
            "dmfc0" | "dmtc0" | "mfc0" | "mtc0" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rt = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rd = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rs: ast::Register::null(),
                    rt,
                    rd,
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |   rt    |   fs    |    0000 0000 000    |
            // ------6----------5---------5---------5--------------11-----------
            //  Format:  op rt, fs
            "cfc1" | "ctc1" | "dmfc1" | "dmtc1" | "mfc1" | "mtc1" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rt = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rd = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::FloatRegister>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rs: ast::Register::null(),
                    rt,
                    rd: ast::Register::from(rd),
                    sa: 0,
                })
            }
            // -----------------------------------------------------------------
            // |   COPz    |CO|      0000 0000 0000 0000 000       |    op     |
            // ------6------1-------------------19-----------------------6------
            //  Format:  op
            // TODO: Figure out what CO is
            "eret" | "tlbp" | "tlbr" | "tlbwi" | "tlbwr" => Ok(ast::Instruction::Register {
                op: op.parse()?,
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
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let ft = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::FloatRegister>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let x = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?;
                let base = base_regex
                    .find_iter(x)
                    .last()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                if let Some(x) = offset_regex.find(x) {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rs: base,
                        rt: ast::Register::from(ft),
                        imm: self.parse_immediate(x.as_str().replace('(', "").trim(), true)?,
                    })
                } else {
                    Ok(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rs: base,
                        rt: ast::Register::from(ft),
                        imm: self.parse_immediate("0", true)?,
                    })
                }
            }
            "nop" => Ok(ast::Instruction::Register {
                op: ast::RTypeOp::Sll,
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                rd: ast::Register::null(),
                sa: 0,
            }),
            "negu" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rd = args
                    .first()
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rs = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Instruction::Register {
                    op: op.parse()?,
                    rd,
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                })
            }
            _ => match &op.to_lowercase()[..op.len() - 2] {
                // -----------------------------------------------------------------
                // |   COP1    |   fmt   |   ft    |   fs    |   fd    |    op     |
                // ------6----------5---------5---------5---------5----------6------
                //  Format:  op.fmt fd, fs, ft
                "add" | "sub" | "mul" | "div" => {
                    if args.len() != 3 {
                        return Err(ParserError::InvalidOperandCount {
                            line: inst.to_string(),
                            expected: 3,
                            found: args.len(),
                        });
                    }
                    let fd = args
                        .first()
                        .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                        .trim()
                        .parse::<ast::FloatRegister>()
                        .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                    let fs = args
                        .get(1)
                        .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                        .trim()
                        .parse::<ast::FloatRegister>()
                        .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                    let ft = args
                        .get(2)
                        .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                        .trim()
                        .parse::<ast::FloatRegister>()
                        .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                    Ok(ast::Instruction::Register {
                        op: op.replace('.', "").parse()?,
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
                        return Err(ParserError::InvalidOperandCount {
                            line: inst.to_string(),
                            expected: 2,
                            found: args.len(),
                        });
                    }
                    let fd = args
                        .first()
                        .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                        .trim()
                        .parse::<ast::FloatRegister>()
                        .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                    let fs = args
                        .get(1)
                        .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                        .trim()
                        .parse::<ast::FloatRegister>()
                        .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                    Ok(ast::Instruction::Register {
                        op: op.replace('.', "").parse()?,
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
                            return Err(ParserError::InvalidOperandCount {
                                line: inst.to_string(),
                                expected: 2,
                                found: args.len(),
                            });
                        }
                        let fs = args
                            .first()
                            .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                            .trim()
                            .parse::<ast::FloatRegister>()
                            .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                        let ft = args
                            .get(1)
                            .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                            .trim()
                            .parse::<ast::FloatRegister>()
                            .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                        return Ok(ast::Instruction::Register {
                            op: format!("C{}", op.chars().last().unwrap()).parse()?,
                            rs: ast::Register::from(fs),
                            rt: ast::Register::from(ft),
                            rd: ast::Register::null(),
                            sa: parse_float_cond(
                                op.split('.').collect::<Vec<&str>>().get(1).unwrap(),
                            )?,
                        });
                    }
                    Err(ParserError::InvalidInstruction(inst.to_string()))
                }
            },
        }
    }

    fn adjust_labels(&mut self) {
        for i in 0..self.insts.len() {
            if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::Label(lbl),
            } = &self.insts[i]
            {
                let lbl_addr = self.labels.get(lbl.as_str()).unwrap();
                self.insts[i] = ast::Instruction::Immediate {
                    op: *op,
                    rs: *rs,
                    rt: *rt,
                    imm: ast::Immediate::Int((*lbl_addr - (i + 1) as isize) as u16 * 4),
                };
            } else if let ast::Instruction::Jump {
                op,
                target: ast::Target::Label(lbl),
            } = &self.insts[i]
            {
                let lbl_addr = self.labels.get(lbl.as_str()).unwrap();
                self.insts[i] = ast::Instruction::Jump {
                    op: *op,
                    target: ast::Target::Address(self.base_addr + *lbl_addr as u32 * 4),
                };
            }
        }
    }

    fn parse_immediate(&self, offset: &str, signed: bool) -> Result<ast::Immediate, ParserError> {
        println!("{}", offset);
        if offset.starts_with('.') {
            return Ok(ast::Immediate::Label(offset.to_string()));
        }
        let offset_regex = Regex::new(r"\(.*\)").unwrap();
        if let Some(x) = offset_regex.find(offset) {
            let x = self.parse_target(&x.as_str().replace(&['(', ')'][..], ""))?;
            match &offset[..3] {
                "%hi" => return Ok(ast::Immediate::Int((x.as_u32() >> 16) as u16)),
                "%lo" => return Ok(ast::Immediate::Int((x.as_u32() & 0xffff) as u16)),
                _ => todo!(),
            }
        }
        if signed {
            if offset.ends_with('`') || !offset.contains("0x") {
                Ok(ast::Immediate::Int(
                    offset.trim_end_matches('`').parse::<i32>().unwrap() as u16,
                ))
            } else {
                let offset = offset.replace("0x", "");
                Ok(ast::Immediate::Int(
                    i32::from_str_radix(&offset, 16).unwrap() as u16,
                ))
            }
        } else if offset.ends_with('`') || !offset.contains("0x") {
            Ok(ast::Immediate::Int(
                offset.trim_end_matches('`').parse::<u16>().unwrap(),
            ))
        } else {
            let offset = offset.replace("0x", "");
            Ok(ast::Immediate::Int(
                u32::from_str_radix(&offset, 16).unwrap() as u16,
            ))
        }
    }
    
    fn parse_target(&self, target: &str) -> Result<ast::Target, ParserError> {
        if let Some(x) = self.syms.get(target) {
            return Ok(ast::Target::Address(*x));
        }
        if target.starts_with("~Func:") {
            Ok(ast::Target::Function(target.replace("~Func:", "")))
        } else if target.starts_with('.') {
            Ok(ast::Target::Label(target.to_string()))
        } else if target.ends_with('`') {
            match target.trim_end_matches('`').parse::<u32>() {
                Ok(addr) => Ok(ast::Target::Address(addr)),
                Err(_) => Err(ParserError::InvalidTargetAddress(target.to_string())),
            }
        } else {
            let addr = target.replace("0x", "");
            match u32::from_str_radix(&addr, 16) {
                Ok(addr) => Ok(ast::Target::Address(addr)),
                Err(_) => Err(ParserError::InvalidTargetAddress(target.to_string())),
            }
        }
    }
}

fn parse_float_cond(cond: &str) -> Result<u16, ParserError> {
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
        _ => Err(ParserError::InvalidFloatCond(cond.to_string())),
    }
}
