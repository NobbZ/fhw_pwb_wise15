#![feature(convert)]

extern crate pwb_ws_15 as pwb;

use std::io;

use pwb::parser;

fn main() {
    let mut line = String::new();

    // That damn range is exclusive at the end... So it really means 4 times!
    for _ in 1..5 {
        let garbage = io::stdin().read_line(&mut line).unwrap();
    }

    let (values, storage, recipes, fluid) =
        parser::input(line.as_str()).unwrap();

    println!("{:?}", values);
    println!("{:?}", storage);
    println!("{:?}", recipes);
    println!("{:?}", fluid);
}
