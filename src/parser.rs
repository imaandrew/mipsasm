use crate::ast;
use regex::Regex;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("Label: `{0}` defined multiple times")]
    MultipleLabelDefinition(String),
    #[error("`{0}` is not a valid instruction")]
    InvalidInstruction(String),
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
        if line.starts_with(".") {
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
        Ok(ast::Token::Label(ast::Target::Label(label.to_string())))
    }

    fn parse_inst(&self, inst: &'a str) -> Result<ast::Token, ParserError> {
        let mut line = inst.split_whitespace();
        let op = match line.next() {
            Some(x) => x,
            None => return Err(ParserError::InvalidInstruction(inst.to_string())),
        };
        let args = line.collect::<String>();
        let mut args = args.split(',');

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
                let op = op.parse().unwrap();
                let rt = args.next().unwrap().parse().unwrap();
                let x = args.next().unwrap();
                let base = base_regex.find(x).unwrap().as_str().replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .unwrap();
                let offset = offset_regex.find(x).unwrap().as_str().replace("(", "");
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op,
                    rs: base,
                    rt,
                    imm: Self::parse_immediate(offset.trim(), true),
                }))
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |          immediate            |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, rs, immediate
            "addi" | "addiu" | "andi" | "daddi" | "daddiu" | "ori" | "slti" | "sltiu" | "xori" => {
                let rt = args.next().unwrap().trim().parse().unwrap();
                let rs = args.next().unwrap().trim().parse().unwrap();
                let imm = args.next().unwrap().trim();
                if op == "andi" || op == "ori" || op == "xori" {
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rt,
                        rs,
                        imm: Self::parse_immediate(imm, false),
                    }))
                } else {
                    Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                        op: op.parse().unwrap(),
                        rt,
                        rs,
                        imm: Self::parse_immediate(imm, true),
                    }))
                }
            }
            // -----------------------------------------------------------------
            // |    op     |  00000  |   rt    |           immediate           |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rt, immediate
            "lui" => {
                let rt = args.next().unwrap().trim().parse().unwrap();
                let imm = args.next().unwrap().trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt,
                    rs: ast::Register::null(),
                    imm: Self::parse_immediate(imm, true),
                }))
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |  00000  |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, offset
            "bgtz" | "bgtzl" | "blez" | "blezl" => {
                let rs = args.next().unwrap().trim().parse().unwrap();
                let imm = args.next().unwrap().trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt: ast::Register::null(),
                    rs,
                    imm: Self::parse_immediate(imm, true),
                }))
            }
            // -----------------------------------------------------------------
            // |    op     |   rs    |   rt    |            offset             |
            // ------6----------5---------5-------------------16----------------
            //  Format:  op rs, rt, offset
            "beq" | "beql" | "bne" | "bnel" => {
                let rs = args.next().unwrap().trim().parse().unwrap();
                let rt = args.next().unwrap().trim().parse().unwrap();
                let imm = args.next().unwrap().trim();
                Ok(ast::Token::Instruction(ast::Instruction::Immediate {
                    op: op.parse().unwrap(),
                    rt,
                    rs,
                    imm: Self::parse_immediate(imm, true),
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
                funct: op.parse().unwrap(),
            })),
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rs, rt
            "add" | "addu" | "and" | "dadd" | "daddu" | "dsub" | "dsubu" | "nor" | "or" | "slt"
            | "sltu" | "sub" | "subu" | "xor" => {
                let rd = args.next().unwrap().trim().parse().unwrap();
                let rs = args.next().unwrap().trim().parse().unwrap();
                let rt = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse().unwrap(),
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |  00000  |   rt    |    rd   |   sa    |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rt, sa
            "dsll" | "dsll32" | "dsra" | "dsra32" | "dsrl" | "dsrl32" | "sll" | "sra" | "srl" => {
                let rd = args.next().unwrap().trim().parse().unwrap();
                let rt = args.next().unwrap().trim().parse().unwrap();
                let sa = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs: ast::Register::null(),
                    rt,
                    sa,
                    funct: op.parse().unwrap(),
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |    rd   |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rt, rs
            "dsllv" | "dsrav" | "dsrlv" | "sllv" | "srav" | "srlv" => {
                let rd = args.next().unwrap().trim().parse().unwrap();
                let rt = args.next().unwrap().trim().parse().unwrap();
                let rs = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse().unwrap(),
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
                funct: op.parse().unwrap(),
            })),
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |   rt    |   0000 0000 00    |    op     |
            // ------6----------5---------5--------------10--------------6------
            //  Format:  op rs, rt
            "ddiv" | "ddivu" | "div" | "divu" | "dmult" | "dmultu" | "mult" | "multu" | "teq"
            | "tge" | "tgeu" | "tlt" | "tltu" | "tne" => {
                let rs = args.next().unwrap().trim().parse().unwrap();
                let rt = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd: ast::Register::null(),
                    rs,
                    rt,
                    sa: 0,
                    funct: op.parse().unwrap(),
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |  00000  |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rs, rd
            "jalr" => {
                let rs = args.next().unwrap().trim().parse().unwrap();
                let rd = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse().unwrap(),
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |     0000 0000 0000 000      |    op     |
            // ------6----------5------------------15--------------------6------
            //  Format:  op rs
            "jr" | "mthi" | "mtlo" => {
                let rs = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd: ast::Register::null(),
                    rs,
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse().unwrap(),
                }))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   0000 0000 00    |   rd    |  00000  |    op     |
            // ------6---------------10-------------5---------5----------6------
            //  Format:  op rd
            "mfhi" | "mflo" => {
                let rd = args.next().unwrap().trim().parse().unwrap();
                Ok(ast::Token::Instruction(ast::Instruction::Register {
                    op: ast::RTypeOp::Special,
                    rd,
                    rs: ast::Register::null(),
                    rt: ast::Register::null(),
                    sa: 0,
                    funct: op.parse().unwrap(),
                }))
            }
            _ => {
                return Err(ParserError::InvalidInstruction(op.to_string()));
            }
        }
    }

    fn parse_immediate(offset: &str, signed: bool) -> ast::Immediate {
        let offset = offset.replace("0x", "");
        if signed {
            if offset.ends_with('`') {
                ast::Immediate::Signed(offset.trim_end_matches("`").parse::<i16>().unwrap())
            } else {
                ast::Immediate::Signed(i16::from_str_radix(&offset, 16).unwrap())
            }
        } else {
            if offset.ends_with('`') {
                ast::Immediate::Unsigned(offset.trim_end_matches("`").parse::<u16>().unwrap())
            } else {
                ast::Immediate::Unsigned(u16::from_str_radix(&offset, 16).unwrap())
            }
        }
    }

    fn parse_target(target: &str) -> ast::Target {
        if target.starts_with('.') {
            ast::Target::Label(target.to_string())
        } else if target.ends_with('`') {
            ast::Target::Address(target.trim_end_matches("`").parse::<u32>().unwrap())
        } else {
            let addr = target.replace("0x", "");
            ast::Target::Address(u32::from_str_radix(&addr, 16).unwrap())
        }
    }
}
