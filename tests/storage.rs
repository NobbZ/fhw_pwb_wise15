extern crate pwb_ws_15 as sut;

use sut::data::storage::Storage;

#[test]
fn create_empty_storage_test() {
    let vec: Vec<usize> = vec![];
    let exp: Vec<usize> = vec![];
    let store = Storage::new(vec);
    assert_eq!(store.get_content(), exp);
    assert_eq!(store.get_fluid(), 0);
}

#[test]
fn set_fluid_once() {
    let vec: Vec<usize> = vec![];
    let exp: Vec<usize> = vec![];
    let store = Storage::new(vec).set_fluid(100);
    assert_eq!(store.get_content(), exp);
    assert_eq!(store.get_fluid(), 100);
}

#[test]
fn alter_fluid_after_it_has_been_set_before() {
    let vec: Vec<usize> = vec![];
    let exp: Vec<usize> = vec![];
    let store = Storage::new(vec).set_fluid(100);
    assert_eq!(store.get_content(), exp.clone());
    assert_eq!(store.get_fluid(), 100);
    let store = store.set_fluid(200);
    assert_eq!(store.get_content(), exp);
    assert_eq!(store.get_fluid(), 200);
}

#[test]
fn consume_item() {
    let original = vec![1];
    let consumed = vec![0];
    let store = Storage::new(original);
    let exp = Storage::new(consumed);
    assert!(store.consume(0));
    assert_eq!(store, exp);
}

#[test]
fn consume_items() {
    let original = vec![2];
    let consumed = vec![0];
    let store = Storage::new(original);
    let exp = Storage::new(consumed);
    assert!(store.consume(0));
    assert!(store.consume(0));
    assert_eq!(store, exp);
}

#[test]
fn consume_more_items_than_are_available() {
    let original = vec![1];
    let store = Storage::new(original);
    assert!(store.consume(0));
    assert!(!store.consume(0));
}

#[test]
fn consume_fluid() {
    let store = Storage::new(vec![0]).set_fluid(100);
    assert!(store.heat(50));
    assert_eq!(store.get_fluid(), 50);
}

#[test]
fn consume_more_fluid_than_is_available () {
    let store = Storage::new(vec![0]).set_fluid(100);
    assert!(!store.heat(1_000));
    assert_eq!(store.get_fluid(), 100);
}
