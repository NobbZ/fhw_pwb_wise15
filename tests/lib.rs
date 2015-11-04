extern crate pwb_ws_15 as sut;

use sut::data;
use sut::parser::{values,storage,recipes};

#[test]
fn values_test_empty () {
    let exp = data::Values::new(vec![]);
    assert_eq!(values("[]"), Ok(exp));
}

#[test]
fn values_test_single () {
    let exp = data::Values::new(vec![1]);
    assert_eq!(values("[1]"), Ok(exp));
}

#[test]
fn values_test_single_negative () {
    let exp = data::Values::new(vec![-1]);
    assert_eq!(values("[-1]"), Ok(exp));
}

#[test]
fn values_test_multiple () {
    let exp = data::Values::new(vec![1,2,3,4,5,-5]);
    assert_eq!(values("[1,2,3,4,5,-5]"), Ok(exp));
}

#[test]
fn storage_test_empty () {
    let exp = data::Storage::new(vec![]);
    assert_eq!(storage("[]"), Ok(exp));
}

#[test]
fn storage_test_single () {
    let exp = data::Storage::new(vec![1]);
    assert_eq!(storage("[1]"), Ok(exp));
}

#[test]
fn storage_test_multiple () {
    let exp = data::Storage::new(vec![1,2,3,4,5,5]);
    assert_eq!(storage("[1,2,3,4,5,5]"), Ok(exp));
}

#[test]
fn recipes_test_empty () {
    let exp = data::Recipes::new(vec![]);
    assert_eq!(recipes("[]"), Ok(exp));
}

#[test]
fn recipes_test_very_simple () {
    let rcp = data::Recipe::new(vec![], vec![], 0);
    let exp = data::Recipes::new(vec![rcp]);
    assert_eq!(recipes("[([],[],0)]"), Ok(exp));
}

#[test]
fn recipes_test_simple () {
    let rcp = data::Recipe::new(vec![0], vec![0], 1);
    let exp = data::Recipes::new(vec![rcp]);
    assert_eq!(recipes("[([0],[0],1)]"), Ok(exp));
}

#[test]
fn recipes_test_multiple () {
    let rcp1 = data::Recipe::new(vec![0,1], vec![2], 10);
    let rcp2 = data::Recipe::new(vec![1,1], vec![3,1], 1024);
    let exp = data::Recipes::new(vec![rcp1,rcp2]);
    assert_eq!(recipes("[([0,1],[2],10),([1,1],[3,1],1024)]"), Ok(exp));
}
