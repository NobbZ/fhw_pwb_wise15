use ramp::int::Int;
use std::sync::Arc;
use super::storage::Storage;
use super::Recipes;

pub struct Node {
    store: Storage,
    children: Vec<ChildLink>,
}

pub enum ChildLink {
    Producable(Option<Arc<Node>>),
    Impossible,
    Finished(Int, Vec<usize>),
}

impl Node {
    pub fn new(store: Storage, recipes: &Recipes) -> Self {
        let mut children: Vec<ChildLink> = Vec::new();

        for r in &recipes.val {
            if r.producable(&store) {
                children.push(ChildLink::Producable(None));
            } else {
                children.push(ChildLink::Impossible);
            }
        }

        Node {
            store: store,
            children: children,
        }
    }
}
