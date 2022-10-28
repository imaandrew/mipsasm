# mipsasm

[![Test](https://github.com/imaandrew/mipsasm/workflows/Tests/badge.svg?event=push)](https://github.com/imaandrew/mipsasm/actions)
[![Crate](https://img.shields.io/crates/v/mipsasm)](https://crates.io/crates/mipsasm)
[![Docs](https://img.shields.io/docsrs/mipsasm)](https://docs.rs/mipsasm)

A MIPS assembler targeting the Nintendo 64

## Getting Started
### Pre-Built Binaries
Pre-Built binaries can be found on the [releases page](https://github.com/imaandrew/mipsasm/releases)

### From Source
mipsasm can be installed using rust's package manager:
```cargo install mipsasm```

### Usage
CLI usage can be found by running `mipsasm --help`
```
    mipsasm [OPTIONS] <MODE> <INPUT_FILE>

ARGS:
    <MODE>          Assemble or disassemble the input file [possible values: asm, disasm]
    <INPUT_FILE>    Use this file as input

OPTIONS:
    -b <base addr>        Use this address as the base address of the program [default: 0x80000000]
    -h, --help            Print help information
    -o <output>           Write output to this file
    -s <syms>             Import symbols from this file
    -V, --version         Print version information
```

API documentation can be found on [docs.rs](https://docs.rs/mipsasm)
