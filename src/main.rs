#![feature(convert)]

extern crate pwb_ws_15 as pwb;

use std::io;

use pwb::parser;

fn main() {
    let mut line = String::new();

    // That damn range is exclusive at the end... So it really means 4 times!
    for _ in 1..5 {
        let _garbage = io::stdin().read_line(&mut line).unwrap();
    }

    // Lets assume that there is only valid input during the contest
    let (values, storage, recipes) =
        parser::input(line.as_str()).unwrap();

    println!("{:?}", values);
    println!("{:?}", storage);
    println!("{:?}", recipes);
    println!("{:?}", storage.get_fluid());
}
