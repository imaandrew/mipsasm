use clap::{arg, command, value_parser};
use std::collections::HashMap;
use std::error;
use std::fs;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn error::Error>> {
    let matches = command!()
        .arg(
            arg!([FILE] "File to assemble")
                .required(true)
                .value_parser(value_parser!(PathBuf)),
        )
        .arg(
            arg!([SYMS] "List of symbols")
                .required(false)
                .value_parser(value_parser!(PathBuf)),
        )
        .get_matches();

    let data: String = match matches.get_one::<PathBuf>("FILE") {
        Some(file) => fs::read_to_string(file)?.parse()?,
        None => panic!(),
    };
    let syms: String = match matches.get_one::<PathBuf>("SYMS") {
        Some(file) => fs::read_to_string(file)?.parse()?,
        None => panic!(),
    };
    let symbols: HashMap<String, u32> = HashMap::from_iter(syms.lines().map(|s| {
        let mut parts = s.split('=');
        let name = parts.next().unwrap().trim();
        let value = parts.next().unwrap();
        let value = u32::from_str_radix(value.replace("0x", "").trim(), 16).unwrap();
        (name.to_string(), value)
    }));
    let x = match mipsasm::parser::scan(&data, 0x800F52E0, symbols) {
        Ok(n) => n,
        Err(e) => panic!("Error: {}", e),
    };
    println!("{:#?}", x);
    let a = mipsasm::assembler::assemble(x);
    for i in a {
        println!("{:08x}", i);
    }
    Ok(())
}
