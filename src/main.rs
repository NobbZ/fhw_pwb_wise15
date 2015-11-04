#![feature(convert)]

extern crate pwb_ws_15 as pwb;

use std::io;

use pwb::parser;
use pwb::data::{Values,Storage};

fn main() {
    let mut line = String::new();

    let garbage = io::stdin().read_line(&mut line).unwrap();
    let garbage = io::stdin().read_line(&mut line).unwrap();
    let garbage = io::stdin().read_line(&mut line).unwrap();
    let garbage = io::stdin().read_line(&mut line).unwrap();

    let (values, storage, recipes, fluid) =
        parser::input(line.as_str()).unwrap();

    println!("{:?}", values);
    println!("{:?}", storage);
    println!("{:?}", recipes);
    println!("{:?}", fluid);
}
