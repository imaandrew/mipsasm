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
    match mipsasm::parser::parse(&data) {
        Ok(n) => println!("{:#?}", n),
        Err(e) => println!("Error: {}", e),
    }
    Ok(())
}
