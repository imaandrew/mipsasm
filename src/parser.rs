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
                    .parse::<ast::Register>()
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
                    rt: Box::new(rt),
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
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt: Box::new(rt),
                        rs,
                        imm: Self::parse_immediate(imm, false)?,
                    }))
                } else {
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse()?,
                        rt: Box::new(rt),
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
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt: Box::new(rt),
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
                    rt: Box::new(ast::Register::null()),
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
                    .parse::<ast::Register>()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(2)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse()?,
                    rt: Box::new(rt),
                    rs,
                    imm: Self::parse_immediate(imm, true)?,
                }))
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Jump {
                    op: op.parse()?,
                    target: Self::parse_target(target)?,
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse()
                    .map_err(|_| ParserError::InvalidRegister(inst.to_string()))?;
                let imm = args
                    .get(1)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::Regimm {
                    op: ast::ITypeOp::Regimm,
                    rs,
                    sub: op.parse()?,
                    imm: Self::parse_immediate(imm, true)?,
                }))
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim();
                Ok(ast::Token::Instruction(ast::Instruction::CopI {
                    op: Self::parse_cop_num(op)?,
                    rs: ast::CopRs::Bc,
                    rt: op.replace(&['0', '1'][..], "").parse()?,
                    imm: Self::parse_immediate(offset, true)?,
                }))
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
                    .get(0)
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
                Ok(ast::Token::Instruction(ast::Instruction::CopR {
                    op: Self::parse_cop_num(op)?,
                    rs: Box::new(Self::parse_cop_op(op)?),
                    rt: Box::new(rt),
                    rd: Box::new(rd),
                    sa: Box::new(ast::Null),
                    func: Box::new(ast::Null),
                }))
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
                    .get(0)
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
                Ok(ast::Token::Instruction(ast::Instruction::CopR {
                    op: Self::parse_cop_num(op)?,
                    rs: Box::new(Self::parse_cop_op(op)?),
                    rt: Box::new(rt),
                    rd: Box::new(rd),
                    sa: Box::new(ast::Null),
                    func: Box::new(ast::Null),
                }))
            }
            // -----------------------------------------------------------------
            // |   COPz    |CO|      0000 0000 0000 0000 000       |    op     |
            // ------6------1-------------------19-----------------------6------
            //  Format:  op
            // TODO: Figure out what CO is
            "eret" | "tlbp" | "tlbr" | "tlbwi" | "tlbwr" => {
                Ok(ast::Token::Instruction(ast::Instruction::CopR {
                    op: ast::CopNum::Cop0,
                    rs: Box::new(ast::Register::null()),
                    rt: Box::new(ast::Register::null()),
                    rd: Box::new(ast::Register::null()),
                    sa: Box::new(ast::Null),
                    func: Box::new(op.parse::<ast::RTypeOp>()?),
                }))
            }
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
                    .get(0)
                    .ok_or_else(|| ParserError::InvalidInstruction(inst.to_string()))?
                    .trim()
                    .parse::<ast::FloatRegister>()
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
                    op: op.parse()?,
                    rs: base,
                    rt: Box::new(ft),
                    imm: Self::parse_immediate(offset.trim(), true)?,
                }))
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
                        .get(0)
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
                    Ok(ast::Token::Instruction(ast::Instruction::CopR {
                        op: ast::CopNum::Cop1,
                        rs: Box::new(op[op.len() - 1..].parse::<ast::CopFmt>()?),
                        rt: Box::new(ft),
                        rd: Box::new(fs),
                        sa: Box::new(fd),
                        func: Box::new(op.to_lowercase()[..op.len() - 2].parse::<ast::FloatOp>()?),
                    }))
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
                        .get(0)
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
                    Ok(ast::Token::Instruction(ast::Instruction::CopR {
                        op: ast::CopNum::Cop1,
                        rs: Box::new(op[op.len() - 1..].replace('.', "").parse::<ast::CopFmt>()?),
                        rt: Box::new(ast::Null),
                        rd: Box::new(fs),
                        sa: Box::new(fd),
                        func: Box::new(
                            op.to_lowercase()[..op.len() - 2]
                                .replace('.', "")
                                .parse::<ast::FloatOp>()?,
                        ),
                    }))
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
                            .get(0)
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
                        return Ok(ast::Token::Instruction(ast::Instruction::CopR {
                            op: ast::CopNum::Cop1,
                            rs: Box::new(
                                op[op.len() - 1..].replace('.', "").parse::<ast::CopFmt>()?,
                            ),
                            rt: Box::new(ft),
                            rd: Box::new(fs),
                            sa: Box::new(ast::Null),
                            func: Box::new(
                                op.to_lowercase()[2..op.len() - 2]
                                    .parse::<ast::FloatCompareCond>()?,
                            ),
                        }));
                    }
                    Err(ParserError::InvalidInstruction(inst.to_string()))
                }
            },
        }
    }

    fn parse_immediate(offset: &str, signed: bool) -> Result<ast::Immediate, ParserError> {
        if offset.starts_with('.') {
            return Ok(ast::Immediate::Label(offset.to_string()));
        }
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

    fn parse_cop_num(cop_num: &str) -> Result<ast::CopNum, ParserError> {
        if cop_num.contains('0') {
            Ok(ast::CopNum::Cop0)
        } else if cop_num.contains('1') {
            Ok(ast::CopNum::Cop1)
        } else {
            //Err(ParserError::InvalidCopNum(cop_num.to_string()))
            panic!()
        }
    }

    fn parse_cop_op(op: &str) -> Result<ast::CopRs, ParserError> {
        match op[..2].to_lowercase().as_str() {
            "mf" => Ok(ast::CopRs::Mf),
            "cf" => Ok(ast::CopRs::Cf),
            "mt" => Ok(ast::CopRs::Mt),
            "ct" => Ok(ast::CopRs::Ct),
            "bc" => Ok(ast::CopRs::Bc),
            _ => match op[..3].to_lowercase().as_str() {
                "dmf" => Ok(ast::CopRs::Dmf),
                "dmt" => Ok(ast::CopRs::Dmt),
                _ => panic!(),
            },
        }
    }
}
