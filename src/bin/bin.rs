extern crate yaml_rust;

use clap::{Parser, ValueEnum};
use mipsasm::{get_bytes, Mipsasm};
use std::collections::HashMap;
use std::error;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use yaml_rust::YamlLoader;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    /// Assemble or disassemble the input file
    #[clap(arg_enum, value_parser)]
    mode: Mode,
    /// Write output to this file
    #[clap(short, value_parser, value_name = "output")]
    output_file: Option<PathBuf>,
    /// Import symbols from this file
    #[clap(short, value_parser, value_name = "syms")]
    syms: Option<PathBuf>,
    /// Use this file as input
    #[clap(value_parser)]
    input_file: PathBuf,
    /// Use this address as the base address of the program
    #[clap(default_value_t = String::from("0x80000000"), short, value_parser, value_name = "base addr")]
    base_addr: String,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Mode {
    Asm,
    Disasm,
}

fn main() -> Result<(), Box<dyn error::Error>> {
    let cli = Cli::parse();

    let syms: String = match cli.syms.as_deref() {
        Some(syms) => fs::read_to_string(syms)?.parse()?,
        None => String::new(),
    };

    let yaml = YamlLoader::load_from_str(&syms).unwrap();
    let syms = yaml.first().map_or_else(HashMap::new, |y| {
        y.as_hash()
            .map(|hash| {
                hash.iter()
                    .map(|(k, v)| {
                        (
                            k.as_i64().unwrap_or_default() as u32,
                            v.as_str().unwrap_or_default(),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default()
    });

    let addr = cli.base_addr.replace("0x", "");
    let addr = u32::from_str_radix(&addr, 16).unwrap_or_else(|_| {
        eprintln!("Error: Invalid base address `{}`", cli.base_addr);
        std::process::exit(1);
    });

    match cli.mode {
        Mode::Asm => {
            let data: String = fs::read_to_string(cli.input_file)?.parse()?;
            let output = match Mipsasm::new().base(addr).symbols(syms).assemble(&data) {
                Ok(output) => output,
                Err(e) => {
                    for err in e {
                        eprintln!("{}", err);
                    }
                    std::process::exit(1);
                }
            };

            if let Some(output_file) = cli.output_file {
                let output = get_bytes(&output);
                let bytes: Vec<u8> = output
                    .iter()
                    .flat_map(|word| word.to_be_bytes().to_vec())
                    .collect();
                File::create(output_file)?.write_all(&bytes)?;
            } else {
                println!("{:08X?}", output);
            }
        }
        Mode::Disasm => {
            let mut words = vec![];
            let mut bytes = fs::read(cli.input_file)?;
            loop {
                // Copy bytes from the input file as words
                let mut word = [0; 4];
                word.copy_from_slice(&bytes[0..4]);
                words.push(u32::from_be_bytes(word));
                bytes.drain(0..4);
                if bytes.is_empty() {
                    break;
                }
            }
            let output = Mipsasm::new().base(addr).symbols(syms).disassemble(&words);

            if let Some(output_file) = cli.output_file {
                let mut f = File::create(output_file)?;
                for inst in output {
                    write!(f, "{}", inst)?;
                }
            } else {
                for inst in output {
                    println!("{}", inst);
                }
            }
        }
    }
    Ok(())
}
