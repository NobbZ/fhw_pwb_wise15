use ramp::int::Int;
use std::sync::Arc;
use data::storage::Storage;
use data::values::Values;
use data::Recipes;

pub struct Node<'a> {
    store: Storage,
    children: Vec<ChildLink<'a>>,
    values: &'a Values,
}

pub enum ChildLink<'a> {
    Producable(Option<Arc<Node<'a>>>),
    Impossible,
    Finished(Int, Vec<usize>),
}

impl<'a> Node<'a> {
    pub fn new(store: Storage, vals: &'a Values, recipes: &Recipes) -> Self {
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
            values: vals,
        }
    }

    pub fn get_overall_value(&self) -> Int {
        let store_iter = self.store.iter();
        let zipped = self.values.iter().zip(store_iter.into_iter());

        zipped.fold(Int::zero(), |acc: Int, (&value, &count)| {
            let current = Int::from(value) * Int::from(count);
            if current >= 0 {
                acc + current
            } else {
                acc + current.clone() * current
            }
        })
    }
}
