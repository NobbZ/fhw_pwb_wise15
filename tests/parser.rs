extern crate pwb_ws_15 as sut;

use sut::data::storage::Storage;
use sut::data::{Values,Recipes,Recipe};
use sut::parser::{values,storage,recipes,input};

#[test]
fn values_test_empty () {
    let exp = Values::new(vec![]);
    assert_eq!(values("[]"), Ok(exp));
}

#[test]
fn values_test_single () {
    let exp = Values::new(vec![1]);
    assert_eq!(values("[1]"), Ok(exp));
}

#[test]
fn values_test_single_negative () {
    let exp = Values::new(vec![-1]);
    assert_eq!(values("[-1]"), Ok(exp));
}

#[test]
fn values_test_multiple () {
    let exp = Values::new(vec![1,2,3,4,5,-5]);
    assert_eq!(values("[1,2,3,4,5,-5]"), Ok(exp));
}

#[test]
fn storage_test_empty () {
    let exp = Storage::new(vec![]);
    assert_eq!(storage("[]"), Ok(exp));
}

#[test]
fn storage_test_single () {
    let exp = Storage::new(vec![1]);
    assert_eq!(storage("[1]"), Ok(exp));
}

#[test]
fn storage_test_multiple () {
    let exp = Storage::new(vec![1,2,3,4,5,5]);
    assert_eq!(storage("[1,2,3,4,5,5]"), Ok(exp));
}

#[test]
fn recipes_test_empty () {
    let exp = Recipes::new(vec![]);
    assert_eq!(recipes("[]"), Ok(exp));
}

#[test]
fn recipes_test_very_simple () {
    let rcp = Recipe::new(vec![], vec![], 0);
    let exp = Recipes::new(vec![rcp]);
    assert_eq!(recipes("[([],[],0)]"), Ok(exp));
}

#[test]
fn recipes_test_simple () {
    let rcp = Recipe::new(vec![0], vec![0], 1);
    let exp = Recipes::new(vec![rcp]);
    assert_eq!(recipes("[([0],[0],1)]"), Ok(exp));
}

#[test]
fn recipes_test_multiple () {
    let rcp1 = Recipe::new(vec![0,1], vec![2], 10);
    let rcp2 = Recipe::new(vec![1,1], vec![3,1], 1024);
    let exp = Recipes::new(vec![rcp1,rcp2]);
    assert_eq!(recipes("[([0,1],[2],10),([1,1],[3,1],1024)]"), Ok(exp));
}

#[test]
fn input_test_001 () {
    let rcp1 = Recipe::new(vec![0,0], vec![1], 1);
    let rcp2 = Recipe::new(vec![0,1], vec![2], 5);
    let exp = (Values::new(vec![10,12,25]),
        Storage::new(vec![7,3,0]),
        Recipes::new(vec![rcp1,rcp2]),
        23);
    assert_eq!(input("[10,12,25]\n[7,3,0]\n[([0,0],[1],1),([0,1],[2],5)]\n23\n"), Ok(exp))
}

#[test]
fn input_test_002 () {
    let rcp1 = Recipe::new(vec![], vec![0], 1);
    let rcp2 = Recipe::new(vec![], vec![1], 2);
    let exp = (Values::new(vec![1, 3]),
        Storage::new(vec![5,5]),
        Recipes::new(vec![rcp1,rcp2]),
        1414);
    assert_eq!(input("[1,3]\n[5,5]\n[([],[0],1),([],[1],2)]\n1414\n"), Ok(exp))
}

#[test]
fn input_test_003 () {
    let rcp1 = Recipe::new(vec![0], vec![1], 1);
    let rcp2 = Recipe::new(vec![1], vec![2], 1);
    let exp = (Values::new(vec![1, 2, 3]),
        Storage::new(vec![1,0,0]),
        Recipes::new(vec![rcp1,rcp2]),
        2);
    assert_eq!(input("[1,2,3]\n[1,0,0]\n[([0],[1],1),([1],[2],1)]\n2\n"), Ok(exp))
}

#[test]
fn input_test_004 () {
    let rcp = Recipe::new(vec![0,1], vec![0,2], 5);
    let exp = (Values::new(vec![0,5,20]),
        Storage::new(vec![1,10,0]),
        Recipes::new(vec![rcp]),
        1337);
    assert_eq!(input("[0,5,20]\n[1,10,0]\n[([0,1],[0,2],5)]\n1337\n"), Ok(exp))
}

#[test]
fn input_test_005 () {
    let rcp = Recipe::new(vec![0], vec![1], 1);
    let exp = (Values::new(vec![5,10]),
        Storage::new(vec![0,0]),
        Recipes::new(vec![rcp]),
        2718);
    assert_eq!(input("[5,10]\n[0,0]\n[([0],[1],1)]\n2718\n"), Ok(exp))
}

#[test]
fn input_test_006 () {
    let rcp = Recipe::new(vec![0], vec![0], 1);
    let exp = (Values::new(vec![100]),
        Storage::new(vec![1]),
        Recipes::new(vec![rcp]),
        42);
    assert_eq!(input("[100]\n[1]\n[([0],[0],1)]\n42\n"), Ok(exp));
}
