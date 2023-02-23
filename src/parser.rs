use crate::ast;
use crate::error::{Line, ParserError, ParserWarning};
use crate::{error, warning};
use indexmap::IndexMap;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::HashMap;
use std::mem;
use std::ops::Index;

macro_rules! inst {
    (Imm, $op:expr, $rs:expr, $rt:expr, $imm:expr) => {
        ast::Instruction::Immediate {
            op: $op.parse().unwrap(),
            rs: $rs,
            rt: $rt,
            imm: $imm,
            bytes: vec![],
        }
    };
    (Jump, $op:expr, $target:expr) => {
        ast::Instruction::Jump {
            op: $op.parse().unwrap(),
            target: $target,
            bytes: vec![],
        }
    };
    (Reg, $op:expr, $rs:expr, $rt:expr, $rd:expr) => {
        ast::Instruction::Register {
            op: $op.parse().unwrap(),
            rs: $rs,
            rt: $rt,
            rd: $rd,
            sa: 0,
            bytes: vec![],
        }
    };
    (Reg, $op:expr, $rs:expr, $rt:expr, $rd:expr, $sa:expr) => {
        ast::Instruction::Register {
            op: $op.parse().unwrap(),
            rs: $rs,
            rt: $rt,
            rd: $rd,
            sa: $sa,
            bytes: vec![],
        }
    };
    (Bytes, $imm:expr) => {
        ast::Instruction::Bytes {
            bytes: ast::Immediate::Int($imm),
        }
    };
}

// Regex to match anything after a comment
static COMMENT_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"(/{2}|;).*").unwrap());
static OFFSET_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r".+\s*\(").unwrap());
static BASE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\(.*?\)").unwrap());
static IMM_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\(.*\)").unwrap());

pub struct Parser<'a> {
    input: Vec<&'a str>,
    insts: Vec<(usize, ast::Instruction)>,
    labels: IndexMap<String, usize>,
    local_labels: HashMap<String, (usize, String)>,
    local_labels_dropped: HashMap<String, Vec<(usize, String)>>,
    base_addr: u32,
    syms: &'a HashMap<u32, &'a str>,
    line_num: usize,
    errors: Vec<ParserError>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, base_addr: u32, syms: &'a HashMap<u32, &'a str>) -> Parser<'a> {
        Parser {
            input: input.lines().collect(),
            insts: vec![],
            labels: IndexMap::new(),
            local_labels: HashMap::new(),
            local_labels_dropped: HashMap::new(),
            base_addr,
            syms,
            line_num: 0,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<Vec<ast::Instruction>, Vec<ParserError>> {
        for i in 0..self.input.len() {
            self.line_num += 1;
            let l = COMMENT_RE.replace_all(self.input.get(i).unwrap().trim(), "");
            self.scan_line(&l).unwrap_or_else(|e| self.errors.push(e));
        }
        if !self.local_labels_dropped.is_empty() {
            let local_labels = mem::take(&mut self.local_labels);
            for (k, v) in local_labels {
                if let std::collections::hash_map::Entry::Vacant(e) =
                    self.local_labels_dropped.entry(k.clone())
                {
                    e.insert(vec![v]);
                } else {
                    self.local_labels_dropped.get_mut(&k).unwrap().push(v);
                }
            }
            self.local_labels.clear()
        }
        self.adjust_labels()
            .unwrap_or_else(|e| e.into_iter().for_each(|e| self.errors.push(e)));
        if self.errors.is_empty() {
            Ok(mem::take(&mut self.insts)
                .into_iter()
                .map(|(_, i)| i)
                .collect())
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn scan_line(&mut self, line: &str) -> Result<(), ParserError> {
        if line.ends_with(':') {
            if line.starts_with("@@") {
                let last_label = self.labels.last();
                if let Some((last_label, _)) = last_label {
                    self.local_labels.insert(
                        self.parse_label(line.strip_suffix(':').unwrap().to_string())?,
                        (self.insts.len(), last_label.clone()),
                    );
                } else {
                    self.local_labels.insert(
                        self.parse_label(line.strip_suffix(':').unwrap().to_string())?,
                        (self.insts.len(), String::new()),
                    );
                }
            } else {
                self.labels.insert(
                    self.parse_label(line.strip_suffix(':').unwrap().to_string())?,
                    self.insts.len(),
                );
                let local_labels = mem::take(&mut self.local_labels);
                for (k, v) in local_labels {
                    if let std::collections::hash_map::Entry::Vacant(e) =
                        self.local_labels_dropped.entry(k.clone())
                    {
                        e.insert(vec![v]);
                    } else {
                        self.local_labels_dropped.get_mut(&k).unwrap().push(v);
                    }
                }
            }
        } else if !line.is_empty() {
            self.insts.push((self.line_num, self.parse_inst(line)?));
        }

        Ok(())
    }

    fn parse_label(&self, label: String) -> Result<String, ParserError> {
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
                let base = BASE_RE
                    .find_iter(x)
                    .last()
                    .unwrap()
                    .as_str()
                    .replace(&['(', ')'][..], "")
                    .trim()
                    .parse()
                    .unwrap();
                if let Some(x) = OFFSET_RE.find(x) {
                    Ok(inst!(
                        Imm,
                        op,
                        base,
                        rt,
                        self.parse_immediate::<i16>(&x.as_str()[..x.as_str().len() - 1])?
                    ))
                } else {
                    Ok(inst!(Imm, op, base, rt, self.parse_immediate::<i16>("0")?))
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
                    Ok(inst!(Imm, op, rs, rt, self.parse_immediate::<u16>(imm)?))
                } else {
                    Ok(inst!(Imm, op, rs, rt, self.parse_immediate::<i16>(imm)?))
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
                Ok(inst!(
                    Imm,
                    op,
                    ast::Register::null(),
                    rt,
                    self.parse_immediate::<u16>(imm)?
                ))
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
                Ok(inst!(
                    Imm,
                    op,
                    rs,
                    ast::Register::null(),
                    self.parse_immediate::<i16>(imm)?
                ))
            }
            "bgez" | "bgezal" | "bgezall" | "bgezl" | "bltz" | "bltzal" | "bltzall" | "bltzl"
            | "beqz" | "bnez" | "beqzl" | "bnezl" | "bgtz" | "bgtzl" | "blez" | "blezl" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let offset = args.get(1).unwrap();
                let imm = self.parse_immediate::<u32>(offset)?;
                if !imm.is_label() && imm.as_u32() % 4 != 0 {
                    eprintln!("{}", warning!(self, UnalignedBranch, offset.to_string()));
                }
                Ok(inst!(Imm, op, rs, ast::Register::null(), imm))
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
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let rt = args.get(1).unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let offset = args.get(2).unwrap();
                let imm = self.parse_immediate::<u32>(offset)?;
                if !imm.is_label() && imm.as_u32() % 4 != 0 {
                    eprintln!("{}", warning!(self, UnalignedBranch, offset.to_string()));
                }
                Ok(inst!(Imm, op, rs, rt, imm))
            }
            // -----------------------------------------------------------------
            // |    op     |                       target                      |
            // ------6-------------------------------26-------------------------
            //  Format:  op target
            "j" | "jal" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let target_str = args.first().unwrap().trim();
                let target = self.parse_target(target_str)?;
                if !target.is_label() && target.as_u32() % 4 != 0 {
                    eprintln!("{}", warning!(self, UnalignedJump, target_str.to_string()));
                }
                Ok(inst!(Jump, op, target))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |      0000 0000 0000 000     |  stype  |    op     |
            // ------6-------------------15-------------------5---------6-------
            //  Format:  op          (stype = 0 implied)
            "nop" | "sync" => Ok(inst!(
                Reg,
                op,
                ast::Register::null(),
                ast::Register::null(),
                ast::Register::null()
            )),
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
                Ok(inst!(Reg, op, rs, rt, rd))
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
                Ok(inst!(
                    Reg,
                    op,
                    ast::Register::null(),
                    rt,
                    rd,
                    if sa.ends_with('`') || !sa.contains("0x") {
                        sa.trim_end_matches('`').parse::<i32>().unwrap() as u32
                    } else {
                        let sa = sa.replace("0x", "");
                        i32::from_str_radix(&sa, 16).unwrap() as u32
                    }
                ))
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
                Ok(inst!(Reg, op, rs, rt, rd))
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

                Ok(inst!(
                    Reg,
                    op,
                    ast::Register::null(),
                    ast::Register::null(),
                    ast::Register::null(),
                    code.as_u32()
                ))
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
                Ok(inst!(Reg, op, rs, rt, ast::Register::null()))
            }
            // -----------------------------------------------------------------
            // |  SPECIAL  |   rs    |  00000  |   rd    |  00000  |    op     |
            // ------6----------5---------5---------5---------5----------6------
            //  Format:  op rd, rs
            "jalr" => {
                if args.len() != 2 && args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                if args.len() == 1 {
                    Ok(inst!(Reg, op, rs, ast::Register::null(), ast::Register::Ra))
                } else {
                    let rd = args.get(1).unwrap().parse().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(inst!(Reg, op, rd, ast::Register::null(), rs))
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
                Ok(inst!(Reg, op, rs, ast::Register::null(), rd))
            }
            "b" | "bal" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }

                let offset = args.first().unwrap();
                let imm = self.parse_immediate::<u32>(offset)?;
                if !imm.is_label() && imm.as_u32() % 4 != 0 {
                    eprintln!("{}", warning!(self, UnalignedBranch, offset.to_string()));
                }
                Ok(inst!(
                    Imm,
                    op,
                    ast::Register::null(),
                    ast::Register::null(),
                    imm
                ))
            }
            "li" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(inst!(
                    Imm,
                    op,
                    ast::Register::null(),
                    rt,
                    self.parse_immediate::<i32>(imm)?
                ))
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
                    Ok(inst!(Reg, op, rs, ast::Register::null(), rd))
                } else {
                    let rt = args.get(2).unwrap().parse().map_err(
                        |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                    )?;
                    Ok(inst!(Reg, op, rs, rt, rd))
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
                if op.to_lowercase().trim() == "jr" && !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let rs = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                Ok(inst!(
                    Reg,
                    op,
                    rs,
                    ast::Register::null(),
                    ast::Register::null()
                ))
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
                Ok(inst!(
                    Reg,
                    op,
                    ast::Register::null(),
                    ast::Register::null(),
                    rd
                ))
            }
            "dli" => {
                if args.len() != 2 {
                    return Err(error!(self, InvalidOperandCount, arg, 2, args.len()));
                }
                let rt = args.first().unwrap().parse().map_err(
                    |ast::RegParseError::RegParseError(e)| error!(self, InvalidRegister, e),
                )?;
                let imm = args.get(1).unwrap();
                Ok(inst!(
                    Imm,
                    op,
                    ast::Register::null(),
                    rt,
                    self.parse_immediate::<i64>(imm)?
                ))
            }
            // -----------------------------------------------------------------
            // |   COPz    |   op    |    bc    |           offset             |
            // ------6----------5----------5------------------16----------------
            //  Format:  op offset
            "bc0f" | "bc1f" | "bc0fl" | "bc1fl" | "bc0t" | "bc1t" | "bc0tl" | "bc1tl" => {
                if args.len() != 1 {
                    return Err(error!(self, InvalidOperandCount, arg, 1, args.len()));
                }
                if !self.insts.is_empty() {
                    let (_, last) = self.insts.last().unwrap();
                    if last.has_delay_slot() {
                        eprintln!("{}", warning!(self, InvalidInstructionInDelaySlot))
                    }
                }
                let offset = args.first().unwrap();
                let imm = self.parse_immediate::<u32>(offset)?;
                if !imm.is_label() && imm.as_u32() % 4 != 0 {
                    eprintln!("{}", warning!(self, UnalignedBranch, offset.to_string()));
                }

                Ok(inst!(
                    Imm,
                    op,
                    ast::Register::null(),
                    ast::Register::null(),
                    imm
                ))
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

                Ok(inst!(Reg, op, ast::Register::null(), rt, rd.into()))
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

                Ok(inst!(Reg, op, ast::Register::null(), rt, rd.into()))
            }
            // -----------------------------------------------------------------
            // |   COPz    |CO|      0000 0000 0000 0000 000       |    op     |
            // ------6------1-------------------19-----------------------6------
            //  Format:  op
            "eret" | "tlbp" | "tlbr" | "tlbwi" | "tlbwr" => Ok(inst!(
                Reg,
                op,
                ast::Register::null(),
                ast::Register::null(),
                ast::Register::null()
            )),
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
                let base = BASE_RE
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
                if let Some(x) = OFFSET_RE.find(x) {
                    Ok(inst!(
                        Imm,
                        op,
                        base,
                        ast::Register::from(ft),
                        self.parse_immediate::<i16>(&x.as_str().replace('(', ""))?
                    ))
                } else {
                    Ok(inst!(
                        Imm,
                        op,
                        base,
                        ast::Register::from(ft),
                        self.parse_immediate::<i16>("0")?
                    ))
                }
            }
            ".word" => Ok(ast::Instruction::Bytes {
                bytes: self.parse_immediate::<u32>(arg)?.as_u32(),
            }),
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

                    Ok(inst!(
                        Reg,
                        op,
                        ast::Register::from(fs),
                        ast::Register::from(ft),
                        ast::Register::from(fd)
                    ))
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

                    Ok(inst!(
                        Reg,
                        op,
                        ast::Register::from(fs),
                        ast::Register::null(),
                        ast::Register::from(fd)
                    ))
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

                        return Ok(inst!(
                            Reg,
                            format!("C.{}", op.chars().last().unwrap()),
                            ast::Register::from(fs),
                            ast::Register::from(ft),
                            ast::Register::null(),
                            self.parse_float_cond(
                                op.split('.').collect::<Vec<&str>>().get(1).unwrap(),
                            )?
                        ));
                    }
                    Err(error!(self, InvalidOpcode, op))
                }
            },
        }
    }

    // Convert each label to an absolute immediate or address
    fn adjust_labels(&mut self) -> Result<(), Vec<ParserError>> {
        let mut errors = Vec::new();
        'a: for i in 0..self.insts.len() {
            if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::Label(lbl),
                ..
            } = &self.insts[i].1
            {
                let lbl_addr = self.labels.get(lbl.as_str()).unwrap();
                let imm = ast::Immediate::Short((*lbl_addr as isize - (i + 1) as isize) as u16);
                self.insts[i].1 = ast::Instruction::Immediate {
                    op: *op,
                    rs: *rs,
                    rt: *rt,
                    // Calculate the offset from the label to the current instruction
                    imm,
                    bytes: vec![],
                };
            } else if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::LocalLabel(loc),
                ..
            } = &self.insts[i].1
            {
                let lbls = self.local_labels_dropped.get(loc.as_str());
                if lbls.is_none() {
                    errors.push(error!(self, UndefinedLabel, self.insts[i].0, loc));
                    continue 'a;
                }
                for (addr, lbl) in lbls.unwrap() {
                    let (lower, upper) = self.get_label_scope(lbl).unwrap();
                    if i < lower || i > upper {
                        continue;
                    }
                    let imm = ast::Immediate::Short((*addr as isize - (i + 1) as isize) as u16);
                    self.insts[i].1 = ast::Instruction::Immediate {
                        op: *op,
                        rs: *rs,
                        rt: *rt,
                        // Calculate the offset from the label to the current instruction
                        imm,
                        bytes: vec![],
                    };
                    continue 'a;
                }
                errors.push(error!(self, LocalLabelOutOfScope, self.insts[i].0, loc));
            } else if let ast::Instruction::Immediate {
                op,
                rs,
                rt,
                imm: ast::Immediate::Int(addr),
                ..
            } = &self.insts[i].1
            {
                if !op.to_string().starts_with('b') {
                    continue;
                }

                // Make sure the address is within the bounds of the program
                if *addr < self.base_addr || *addr > self.base_addr + self.insts.len() as u32 * 4 {
                    errors.push(error!(
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
                    bytes: vec![],
                };
            } else if let ast::Instruction::Jump {
                op,
                target: ast::Target::Label(lbl),
                ..
            } = &self.insts[i].1
            {
                let lbl_addr = self.labels.get(lbl.as_str());
                if lbl_addr.is_none() {
                    errors.push(error!(self, UndefinedLabel, self.insts[i].0, lbl));
                    continue;
                }
                self.insts[i].1 = ast::Instruction::Jump {
                    op: *op,
                    target: ast::Target::Address(self.base_addr + *lbl_addr.unwrap() as u32 * 4),
                    bytes: vec![],
                };
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }
        Ok(())
    }

    fn parse_immediate<T>(&self, imm: &str) -> Result<ast::Immediate, ParserError>
    where
        T: num::PrimInt + std::str::FromStr,
    {
        let imm = imm.trim();

        if imm.starts_with("@@") {
            return Ok(ast::Immediate::LocalLabel(imm.to_string()));
        }

        if self.labels.contains_key(imm) {
            return Ok(ast::Immediate::Label(imm.to_string()));
        }

        if let Some(x) = IMM_RE.find(imm) {
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
        if let Some(x) = self.syms.iter().find(|(_, v)| **v == target) {
            return Ok(ast::Target::Address(*x.0));
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

    fn get_label_scope(&self, lbl: &str) -> Result<(usize, usize), ParserError> {
        if lbl.is_empty() {
            return Ok((0, self.insts.len()));
        }
        let label = self.labels.get(lbl).unwrap();
        let next_index = self.labels.get_index_of(lbl).unwrap() + 1;
        if next_index >= self.labels.len() {
            Ok((*label, self.insts.len()))
        } else {
            let next_label = self.labels.index(next_index);
            Ok((*label, *next_label))
        }
    }
}
