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
//! use mipsasm::Mipsasm;
//! # use std::error::Error;
//! # use mipsasm::ParserError;
//!
//! # fn main() -> Result<(), Vec<ParserError>> {
//! let asm = "add $a0, $a1, $a2";
//! let bin = Mipsasm::new().base(0x80000000).assemble(asm)?;
//! assert_eq!(bin, vec![0x00a62020]);
//!
//! let insts = Mipsasm::new().base(0x80000000).disassemble(&bin);
//! assert_eq!(insts, vec!["add        $a0, $a1, $a2"]);
//! # Ok(())
//! # }
//! ```

mod assembler;
mod ast;
mod disassembler;
mod error;
mod parser;

pub use error::ParserError;

use std::collections::HashMap;

/// An instance of the assembler/disassembler
pub struct Mipsasm<'a> {
    base_addr: u32,
    syms: HashMap<&'a str, u32>,
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
    /// mipsasm.symbols(HashMap::from_iter(vec![("foo", 0x8000_0000)]));
    /// ```
    pub fn symbols(&mut self, syms: HashMap<&'a str, u32>) -> &mut Mipsasm<'a> {
        self.syms = syms;
        self
    }

    /// Set the debug flag for the assembler.
    ///
    /// When debug is set to true, the disassembler will print instructions with all extra whitespace stripped.
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
    pub fn assemble(&self, input: &str) -> Result<Vec<u32>, Vec<ParserError>> {
        let mut parser = parser::Parser::new(input, self.base_addr, &self.syms);
        Ok(assembler::assemble(parser.parse()?))
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
    /// assert_eq!(instructions, vec!["mult       $a0, $a1"]);
    /// ```
    pub fn disassemble(&self, input: &[u32]) -> Vec<String> {
        let mut x = disassembler::disassemble(input.to_vec(), self.base_addr);
        self.match_syms(&mut x);
        if self.debug {
            x.iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>()
        } else {
            x.iter().map(|x| x.to_string()).collect::<Vec<String>>()
        }
    }

    // Iterates over a vector of instructions and replaces any value with a defined symbol with the actual symbol string.
    fn match_syms(&self, insts: &mut Vec<ast::Instruction>) {
        for i in insts {
            if let ast::Instruction::Jump {
                op,
                target: ast::Target::Address(addr),
                ..
            } = i
            {
                if let Some(sym) = self.syms.iter().find(|(_, v)| **v == *addr) {
                    *i = ast::Instruction::Jump {
                        op: *op,
                        target: ast::Target::Label(sym.0.to_string()),
                        bytes: 0,
                    };
                }
            }
        }
    }
}
