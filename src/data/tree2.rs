use ramp::int::Int;

use data::Recipe;
use data::Recipes;
use data::storage::Storage;
use data::values::Values;

#[derive(Debug)]
pub enum Node<'a> {
    Pending(&'a Storage, &'a Recipe, &'a Values),
    Active(&'a Storage, Int, Vec<Option<Box<Node<'a>>>>),
    Finished(Int, Vec<Node<'a>>),
    Impossible,
}

impl<'a> Node<'a> {
    pub fn new_root(initial: &'a Storage, rs: &'a Recipes, vs: &'a Values) -> Self {
        let mut cs: Vec<Option<Box<Node>>> = Vec::new();
        let value = Int::zero(); // Calculate current value here!

        for r in rs.iter() {
            if r.producable(&initial) {
                cs.push(Some(Box::new(
                    Node::Pending(
                        initial,
                        r,
                        vs
                    )
                )))
            }
        }

        Node::Active(initial, value, cs)
    }

    pub fn is_active(&self) -> bool {
        match self {
            &Node::Active(_,_,_) => true,
            _ => false
        }
    }
}
