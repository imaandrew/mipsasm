use std::fmt::Write;
use std::{cmp, fmt};

#[macro_export]
macro_rules! error {
    ($self:ident, MultipleLabelDefinition, $label:expr, $first:expr) => {
        ParserError::MultipleLabelDefinition {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            label: $label.to_string(),
            first: Line::new($first + 1, $self.input.get($first).unwrap().to_string()),
        }
    };
    ($self:ident, InvalidLabel, $label:expr) => {
        ParserError::InvalidLabel {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            label: $label.to_string(),
        }
    };
    ($self:ident, InvalidInstruction) => {
        ParserError::InvalidInstruction {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
        }
    };
    ($self:ident, InvalidOperandCount, $ops:expr, $expected:expr, $found:expr) => {
        ParserError::InvalidOperandCount {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            ops: $ops.to_string(),
            expected: $expected,
            found: $found,
        }
    };
    ($self:ident, InvalidOpcode, $opcode:expr) => {
        ParserError::InvalidOpcode {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            opcode: $opcode.to_string(),
        }
    };
    ($self:ident, InvalidRegister, $register:expr) => {
        ParserError::InvalidRegister {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            register: $register.to_string(),
        }
    };
    ($self:ident, InvalidTargetAddress, $target:expr) => {
        ParserError::InvalidTargetAddress {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            address: $target.to_string(),
        }
    };
    ($self:ident, InvalidImmediate, $immediate:expr) => {
        ParserError::InvalidImmediate {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            immediate: $immediate.to_string(),
        }
    };
    ($self:ident, InvalidFloatCond, $cond:expr) => {
        ParserError::InvalidFloatCond {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            cond: $cond.to_string(),
        }
    };
    ($self:ident, BranchOutOfBounds, $line:expr, $target:expr, $bounds:expr) => {
        ParserError::BranchOutOfBounds {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            branch: $target,
            bounds: $bounds,
        }
    };
    ($self:ident, LocalLabelOutOfScope, $line_num:expr, $label:expr) => {
        ParserError::LocalLabelOutOfScope {
            line: Line::new(
                $line_num,
                $self.input.get($line_num - 1).unwrap().to_string(),
            ),
            label: $label.to_string(),
        }
    };
    ($self:ident, UndefinedLabel, $line_num:expr, $label:expr) => {
        ParserError::UndefinedLabel {
            line: Line::new(
                $line_num,
                $self.input.get($line_num - 1).unwrap().to_string(),
            ),
            label: $label.to_string(),
        }
    };
}

#[macro_export]
macro_rules! warning {
    ($self:ident, InvalidInstructionInDelaySlot) => {
        ParserWarning::InvalidInstructionInDelaySlot {
            delay_slot_inst: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            line: Line::new(
                $self.line_num - 1,
                $self.input.get($self.line_num - 2).unwrap().to_string(),
            ),
        }
    };
    ($self:ident, UnalignedBranch, $target:expr) => {
        ParserWarning::UnalignedBranch {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            offset: $target,
        }
    };
    ($self:ident, UnalignedJump, $target:expr) => {
        ParserWarning::UnalignedJump {
            line: Line::new(
                $self.line_num,
                $self.input.get($self.line_num - 1).unwrap().to_string(),
            ),
            target: $target,
        }
    };
}

#[derive(Debug)]
pub struct Line {
    num: usize,
    content: String,
}

impl Line {
    pub fn new(num: usize, content: String) -> Self {
        Self { num, content }
    }
}

#[derive(Debug)]
pub enum ParserWarning {
    InvalidInstructionInDelaySlot { line: Line, delay_slot_inst: Line },
    UnalignedBranch { line: Line, offset: String },
    UnalignedJump { line: Line, target: String },
}

impl fmt::Display for ParserWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidInstructionInDelaySlot {
                line: Line { num, content },
                delay_slot_inst:
                    Line {
                        num: delay_num,
                        content: delay_content,
                    },
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "warning: instruction `{}` cannot be in a delay slot",
                    delay_content.trim()
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *delay_num,
                        delay_content,
                        margin,
                        false,
                        "this instruction cannot be in a delay slot",
                        true,
                        delay_content.trim()
                    )
                )?;
                writeln!(f, "\x1b[94m...\x1b[0m")?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *num,
                        content,
                        margin,
                        true,
                        "delay slot occurs after this instruction",
                        false,
                        content.trim()
                    )
                )
            }
            Self::UnalignedBranch {
                line: Line { num, content },
                offset,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "warning: branch offset `{}` is not aligned to a 4-byte boundary",
                    offset
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *num,
                        content,
                        margin,
                        true,
                        "offset is not divisible by 4",
                        false,
                        offset
                    )
                )
            }
            Self::UnalignedJump {
                line: Line { num, content },
                target,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "warning: jump target `{}` is not aligned to a 4-byte boundary",
                    target
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(
                        *num,
                        content,
                        margin,
                        true,
                        "target is not divisible by 4",
                        false,
                        target
                    )
                )
            }
        }
    }
}

#[derive(Debug)]
pub enum ParserError {
    MultipleLabelDefinition {
        line: Line,
        label: String,
        first: Line,
    },
    InvalidLabel {
        line: Line,
        label: String,
    },
    InvalidInstruction {
        line: Line,
    },
    InvalidOperandCount {
        line: Line,
        expected: usize,
        found: usize,
        ops: String,
    },
    InvalidOpcode {
        line: Line,
        opcode: String,
    },
    InvalidRegister {
        line: Line,
        register: String,
    },
    InvalidTargetAddress {
        line: Line,
        address: String,
    },
    InvalidImmediate {
        line: Line,
        immediate: String,
    },
    InvalidFloatCond {
        line: Line,
        cond: String,
    },
    BranchOutOfBounds {
        line: Line,
        branch: String,
        bounds: (u32, u32),
    },
    LocalLabelOutOfScope {
        line: Line,
        label: String,
    },
    UndefinedLabel {
        line: Line,
        label: String,
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
            Self::LocalLabelOutOfScope {
                line: Line { num, content },
                label,
            } => {
                let margin = num.to_string().len();
                writeln!(
                    f,
                    "\x1b[91merror\x1b[0m: local label `{}` used outside of its scope",
                    label
                )?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "", true, label)
                )
            }
            Self::UndefinedLabel {
                line: Line { num, content },
                label,
            } => {
                let margin = num.to_string().len();
                writeln!(f, "\x1b[91merror\x1b[0m: label `{}` is not defined", label)?;
                writeln!(
                    f,
                    "{}",
                    fmt_line(*num, content, margin, false, "used here", true, label)
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
