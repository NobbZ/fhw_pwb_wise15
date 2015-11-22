extern crate pwb_ws_15 as pwb;

use pwb::data::Recipes;
use pwb::data::storage::Storage;
use pwb::data::values::Values;
use pwb::data::tree2::Node;

#[test]
fn new_root() {
    let a = Storage::default();
    let b = Recipes::default();
    let c = Values::default();
    let root = Node::new_root(&a,&b,&c);
    assert!(root.is_active());
    assert_eq!("", format!("{:?}", root));
}

#[test]
fn new_root_2() {
    let a = Storage::new(vec![1]);
    let b = Recipes::default();
    let c = Values::new(vec![1]);
    let root = Node::new_root(&a,&b,&c);
    assert!(root.is_active());
    assert_eq!("", format!("{:?}", root));
}
