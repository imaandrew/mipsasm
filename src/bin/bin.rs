use mipsasm;

fn main() {
    match mipsasm::parser::parse(&std::fs::read_to_string("a").unwrap()) {
        Ok(n) => println!("{:#?}", n),
        Err(e) => println!("Error: {}", e),
    }
}
