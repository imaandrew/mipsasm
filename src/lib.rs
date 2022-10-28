mod assembler;
mod ast;
mod disassembler;
mod parser;

use std::collections::HashMap;

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
    pub fn new() -> Mipsasm<'a> {
        Mipsasm {
            base_addr: 0,
            syms: HashMap::new(),
            debug: false,
        }
    }

    pub fn base(&mut self, addr: u32) -> &mut Mipsasm<'a> {
        self.base_addr = addr;
        self
    }

    pub fn symbols(&mut self, syms: HashMap<&'a str, u32>) -> &mut Mipsasm<'a> {
        self.syms = syms;
        self
    }

    pub fn debug(&mut self) -> &mut Mipsasm<'a> {
        self.debug = true;
        self
    }

    pub fn assemble(&self, input: &str) -> Result<Vec<u32>, String> {
        let mut parser = parser::Parser::new(input, self.base_addr, &self.syms);
        Ok(assembler::assemble(parser.parse().unwrap()))
    }

    pub fn disassemble(&self, input: &[u32]) -> Vec<String> {
        let mut x = disassembler::disassemble(input.to_vec());
        self.match_syms(&mut x);
        if self.debug {
            x.iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>()
        } else {
            x.iter().map(|x| x.to_string()).collect::<Vec<String>>()
        }
    }

    fn match_syms(&self, insts: &mut Vec<ast::Instruction>) {
        for i in insts {
            if let ast::Instruction::Jump {
                op,
                target: ast::Target::Address(addr),
            } = i
            {
                if let Some(sym) = self.syms.iter().find(|(_, v)| **v == *addr) {
                    *i = ast::Instruction::Jump {
                        op: *op,
                        target: ast::Target::Label(sym.0.to_string()),
                    };
                }
            }
        }
    }
}
