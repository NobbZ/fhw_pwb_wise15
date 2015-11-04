extern crate pwb_ws_15 as sut;

use sut::data;
use sut::parser::{values};

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