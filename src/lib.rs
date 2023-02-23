//! # Mipsasm
//!
//! A MIPS assembler/disassembler specifically targeting the N64.
//!
//! ## Features
//! - Full support for the MIPS R4300i instruction set
//! - Supports many psuedo-instructions for common operations
//! - o32 ABI GPR and FPR register names
//! - Coprocessor 0 register names
//! - Fully-featured CLI
//!
//! ## Example
//!
//! ```rust
//! use mipsasm::{Mipsasm, get_bytes};
//! # use std::error::Error;
//! # use mipsasm::ParserError;
//!
//! # fn main() -> Result<(), Vec<ParserError>> {
//! let asm = "add $a0, $a1, $a2";
//! let inst = Mipsasm::new().base(0x80000000).assemble(asm)?;
//! let bytes: Vec<u32> = get_bytes(&inst);
//! assert_eq!(bytes, vec![0x00a62020]);
//!
//! let insts = Mipsasm::new().base(0x80000000).disassemble(&bytes);
//! assert_eq!(insts, vec!["func_80000000:", "add        $a0, $a1, $a2"]);
//!
//! let insts = Mipsasm::new().base(0x80000000).debug().disassemble(&bytes);
//! assert_eq!(insts, vec!["add $a0, $a1, $a2"]);
//! # Ok(())
//! # }
//! ```

mod assembler;
mod ast;
mod disassembler;
mod error;
mod parser;

pub use ast::Instruction;
pub use error::ParserError;

use std::collections::HashMap;

/// An instance of the assembler/disassembler
pub struct Mipsasm<'a> {
    base_addr: u32,
    syms: HashMap<u32, &'a str>,
    debug: bool,
}

impl<'a> Default for Mipsasm<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Mipsasm<'a> {
    /// Create a new `Mipsasm` instance.
    ///
    /// Sets the base address to 0 and the debug flag to false.
    pub fn new() -> Mipsasm<'a> {
        Mipsasm {
            base_addr: 0,
            syms: HashMap::new(),
            debug: false,
        }
    }

    /// Set the base address for the assembler.
    ///
    /// # Examples
    ///
    /// ```
    /// use mipsasm::Mipsasm;
    ///
    /// let mut mipsasm = Mipsasm::new();
    /// mipsasm.base(0x8000_0000);
    /// ```
    pub fn base(&mut self, addr: u32) -> &mut Mipsasm<'a> {
        self.base_addr = addr;
        self
    }

    /// Provides the assembler with a set of symbols.
    ///
    /// # Examples
    ///
    /// ```
    /// use mipsasm::Mipsasm;
    /// use std::collections::HashMap;
    ///
    /// let mut mipsasm = Mipsasm::new();
    /// mipsasm.symbols(HashMap::from_iter(vec![(0x8000_0000, "foo")]));
    /// ```
    pub fn symbols(&mut self, syms: HashMap<u32, &'a str>) -> &mut Mipsasm<'a> {
        self.syms = syms;
        self
    }

    /// Set the debug flag for the assembler.
    ///
    /// When debug is set to true, the disassembler will print instructions with all extra whitespace stripped and will not emit labels.
    /// This is used for testing and will most likely not be useful for other purposes.
    ///
    /// # Examples
    ///
    /// ```
    /// use mipsasm::Mipsasm;
    ///
    /// let mut mipsasm = Mipsasm::new();
    /// mipsasm.debug();
    /// ```
    pub fn debug(&mut self) -> &mut Mipsasm<'a> {
        self.debug = true;
        self
    }

    /// Assembles a set of MIPS assembly instructions.
    ///
    /// # Examples
    ///
    /// ```
    /// use mipsasm::Mipsasm;
    /// let mut mipsasm = Mipsasm::new();
    /// let instructions = mipsasm.assemble("
    ///    add $t0, $t1, $t2
    ///   addi $t0, $t1, 0x1234
    /// ");
    /// ```
    pub fn assemble(&self, input: &str) -> Result<Vec<Instruction>, Vec<ParserError>> {
        let mut parser = parser::Parser::new(input, self.base_addr, &self.syms);
        let mut insts = parser.parse().unwrap();
        assembler::assemble(&mut insts);
        Ok(insts)
    }

    /// Disassembles a set of MIPS instructions.
    ///
    /// # Examples
    ///
    /// ```
    /// use mipsasm::Mipsasm;
    ///
    /// let mut mipsasm = Mipsasm::new();
    /// let instructions = mipsasm.disassemble(&[0x00850018]);
    /// ```
    pub fn disassemble(&self, input: &[u32]) -> Vec<String> {
        let mut x = disassembler::disassemble(input.to_vec(), self.base_addr);
        self.match_syms(&mut x);

        if self.debug {
            x.iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>()
        } else {
            let mut out = vec![];
            let mut func_start = 0;
            let mut function_ended = false;

            out.push(format!("{}:", self.get_sym(self.base_addr)));

            for i in 0..x.len() {
                if function_ended {
                    out.push(x[i].to_string());
                    func_start = i * 4;
                    function_ended = false;
                    if i < x.len() - 1 {
                        out.push(String::new());
                        out.push(format!(
                            "{}:",
                            self.get_sym(self.base_addr + (i + 1) as u32 * 4)
                        ));
                    }
                } else {
                    if x[i].is_unconditional_jump()
                        && (x[i].get_jump_target() < Some(func_start as u32 + self.base_addr)
                            || x[i].get_jump_target() > Some(i as u32 * 4 + self.base_addr))
                    {
                        function_ended = true;
                    }

                    out.push(x[i].to_string());
                }
            }

            out
        }
    }

    // Iterates over a vector of instructions and replaces any value with a defined symbol with the actual symbol string.
    fn match_syms(&self, insts: &mut Vec<ast::Instruction>) {
        for i in insts {
            if let ast::Instruction::Jump {
                op,
                target: ast::Target::Address(addr),
                bytes,
            } = i
            {
                if let Some(sym) = self.syms.get(addr) {
                    *i = ast::Instruction::Jump {
                        op: *op,
                        target: ast::Target::Label(sym.to_string()),
                        bytes: bytes.to_owned(),
                    };
                }
            }
        }
    }

    fn get_sym(&self, addr: u32) -> String {
        if let Some(sym) = self.syms.get(&addr) {
            sym.to_string()
        } else {
            format!("func_{:08x}", addr)
        }
    }
}

/// Collects all of the bytes from a slice of Instructions and returns them in a vector
///
/// # Examples
///
/// ```
/// use mipsasm::Mipsasm;
/// let mut mipsasm = Mipsasm::new();
/// let instructions = mipsasm.assemble("
///    add $t0, $t1, $t2
///   addi $t0, $t1, 0x1234
/// ").unwrap();
/// assert_eq!(mipsasm::get_bytes(&instructions), vec![0x012a4020, 0x21281234])
/// ```
pub fn get_bytes(insts: &[Instruction]) -> Vec<u32> {
    insts.iter().flat_map(|x| x.get_bytes()).collect()
}
