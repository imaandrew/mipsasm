use clap::{arg, command, value_parser};
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
        .get_matches();

    let data: String = match matches.get_one::<PathBuf>("FILE") {
        Some(file) => fs::read_to_string(file)?.parse()?,
        None => panic!(),
    };
    let x = match mipsasm::parser::scan(&data) {
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
