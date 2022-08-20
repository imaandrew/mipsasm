use crate::ast;
use regex::Regex;
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
    #[error("invalid immediate `{0}`")]
    InvalidImmediate(#[from] ParseIntError),
}

pub fn parse(input: &str) -> Result<Vec<ast::Token>, ParserError> {
    let mut parser = Parser::new(input);
    parser.parse()?;
    Ok(parser.tokens)
}

struct Parser<'a> {
    input: &'a str,
    tokens: Vec<ast::Token>,
    labels: Vec<&'a str>,
}

impl<'a> Parser<'a> {
    fn new(input: &str) -> Parser {
        Parser {
            input,
            tokens: vec![],
            labels: vec![],
        }
    }

    fn parse(&mut self) -> Result<(), ParserError> {
        for line in self.input.lines() {
            self.parse_line(line)?;
        }
        Ok(())
    }

    fn parse_line(&mut self, line: &'a str) -> Result<(), ParserError> {
        if line.starts_with('.') {
            self.tokens.push(self.parse_label(line)?);
        } else if !line.is_empty() {
            self.tokens.push(self.parse_inst(line)?);
        }

        Ok(())
    }

    fn parse_label(&self, label: &'a str) -> Result<ast::Token, ParserError> {
        if self.labels.contains(&label) {
            return Err(ParserError::MultipleLabelDefinition(label.to_string()));
        }
        Ok(ast::Token::Label(ast::Immediate::Label(label.to_string())))
    }

    fn parse_inst(&self, inst: &'a str) -> Result<ast::Token, ParserError> {
        let mut line = inst.split_whitespace();
        let op = match line.next() {
            Some(x) => x,
            None => return Err(ParserError::InvalidInstruction(inst.to_string())),
        };
        let args = line.collect::<String>();
        let args = args.split(',').collect::<Vec<&str>>();

        let offset_regex = Regex::new(r".+?\s*\(").unwrap();
        let base_regex = Regex::new(r"\(.*\)").unwrap();

        match op.to_lowercase().as_str() {
            // -----------------------------------------------------------------
            // |    op     |  base   |   rt    |             offset            |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, offset(base)
            "cache" | "lb" | "lbu" | "ld" | "ldl" | "ldr" | "lh" | "lhu" | "ll" | "lld" | "lw"
            | "lwl" | "lwr" | "lwu" | "sb" | "sc" | "scd" | "sd" | "sdl" | "sdr" | "sh" | "sw"
            | "swl" | "swr" => {
                let op = op.parse()?;
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rt = args
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let x = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?;
                let base = base_regex
                    .find(x)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let offset = offset_regex
                    .find(x)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .as_str()
                    .replace('(', "");
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op,
                    rs: base,
                    rt,
                    imm: Self::parse_immediate(offset.trim(), true)?,
                }))
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
                    .get(0)
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
                let imm = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                if op == "andi" || op == "ori" || op == "xori" {
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt,
                        rs,
                        imm: Self::parse_immediate(imm, false)?,
                    }))
                } else {
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt,
                        rs,
                        imm: Self::parse_immediate(imm, true)?,
                    }))
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt,
                    rs: ast::Register::null(),
                    imm: Self::parse_immediate(imm, true)?,
                }))
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |  00000  |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, offset
            "bgtz" | "bgtzl" | "blez" | "blezl" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt: ast::Register::null(),
                    rs,
                    imm: Self::parse_immediate(imm, true)?,
                }))
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
                    .get(0)
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
                let imm = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt,
                    rs,
                    imm: Self::parse_immediate(imm, true)?,
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |      0000 0000 0000 000     |  stype  |    op     |
            // ------6-------------------15-------------------5---------6-------
            //  Format:  op          (stype = 0 implied)
            "sync" => Ok(ast::Token::Instruction(ast::Instruction::Register {
                op: ast::RTypeOp::Special,
                rd: ast::Register::null(),
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                sa: 0,
                funct: op.parse()?,
            })),
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
                    .get(0)
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
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse()?,
                }))
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
                    .get(0)
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
                    .trim()
                    .parse()?;
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs: ast::Register::null(),
                    rt,
                    sa,
                    funct: op.parse()?,
                }))
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
                    .get(0)
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
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse()?,
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |                   code                |    op     |
            // ------6--------------------------20-----------------------6------
            //  Format:  op offset
            "break" | "syscall" => Ok(ast::Token::Instruction(ast::Instruction::Register {
                op: ast::RTypeOp::Special,
                rd: ast::Register::null(),
                rs: ast::Register::null(),
                rt: ast::Register::null(),
                sa: 0,
                funct: op.parse()?,
            })),
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
                    .get(0)
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
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd: ast::Register::null(),
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse()?,
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |  00000  |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rs, rd
            "jalr" => {
                if args.len() != 2 {
                    return Err(ParserError::InvalidOperandCount {
                        line: inst.to_string(),
                        expected: 2,
                        found: args.len(),
                    });
                }
                let rs = args
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let rd = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse()?,
                }))
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd: ast::Register::null(),
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse()?,
                }))
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse()?,
                }))
            }
            _ => Err(ParserError::InvalidInstruction(inst.to_string())),
        }
    }

    fn parse_immediate(offset: &str, signed: bool) -> Result<ast::Immediate, ParserError> {
        let offset = offset.replace("0x", "");
        if signed {
            if offset.ends_with('`') {
                Ok(ast::Immediate::Signed(
                    offset.trim_end_matches('`').parse::<i16>()?,
                ))
            } else {
                Ok(ast::Immediate::Signed(i16::from_str_radix(&offset, 16)?))
            }
        } else if offset.ends_with('`') {
            Ok(ast::Immediate::Unsigned(
                offset.trim_end_matches('`').parse::<u16>()?,
            ))
        } else {
            Ok(ast::Immediate::Unsigned(u16::from_str_radix(&offset, 16)?))
        }
    }

    fn parse_target(target: &str) -> Result<ast::Target, ParserError> {
        if target.starts_with("~Func:") {
            Ok(ast::Target::Function(target.replace("~Func:", "")))
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
