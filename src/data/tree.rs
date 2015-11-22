use ramp::int::Int;
use std::cell::Cell;
use std::cell::RefCell;
use std::sync::{Arc,Mutex};
use std::collections::HashMap;

use data::storage::Storage;
use data::values::Values;
use data::Recipes;

// #[derive(Debug)]
// pub struct Node<'a> {
//     store: Storage,
//     children: Vec<ChildLink<'a>>,
//     values: &'a Values,
// }
#[derive(Debug,Clone)]
pub struct Node {
    store: Storage,
    children: HashMap<u32, RefCell<ChildLink>>,
}
#[derive(Debug,Clone)]
pub enum ChildLink {
    Producable(Option<Box<Node>>),
    Impossible,
    Finished(Int, Vec<usize>)
}

impl ChildLink {
    pub fn unwrap(&self) -> &Option<Box<Node>> {
        match self {
            &ChildLink::Producable(ref r) => r,
            _ => panic!()
        }
    }
}

impl Node {
    pub fn new(storage: Storage, recipes: &Recipes) -> Self {
        let mut children: HashMap<u32, RefCell<ChildLink>> = HashMap::with_capacity(recipes.count());
        let mut i: u32 = 0;

        for r in recipes.val.iter() {
            if r.producable(&storage) {
                children.insert(i, RefCell::new(ChildLink::Producable(None)));
            } else {
                children.insert(i, RefCell::new(ChildLink::Impossible));
            }
            i = i + 1;
        }

        Node {
            store: storage,
            children: children,
        }
    }

    pub fn get_child_node<'a>(&'a mut self, idx: u32, values: &Values, recipes: &Recipes) -> Option<Box<Self>> {
        match *self.children.get(&idx).unwrap().borrow() {
            ChildLink::Producable(None) => {
                self.eval(idx, values, recipes);
                self.children.get(&idx).unwrap().borrow().unwrap().clone()
            },
            ChildLink::Producable(Some(ref c)) => Some(c),
            _ => None
        }
    }

    pub fn eval(&mut self, idx: u32, values: &Values, recipes: &Recipes) {
        match *self.children.get(&idx).unwrap().borrow() {
            ChildLink::Producable(None) => {
                let nstore = recipes.val[idx as usize].produce(&self.store).unwrap();
                let cnode = Box::new(Node::new(nstore, &recipes));
                let mut a = self.children.get_mut(&idx).unwrap();
                *a = RefCell::new(ChildLink::Producable(Some(cnode.clone())));
            }
            _ => ()
        }
    }
}

//
// #[derive(Clone,Debug)]
// pub enum ChildLink<'a> {
//     Producable(Option<Arc<Node<'a>>>),
//     Impossible,
//     Finished(Int, Vec<usize>),
// }
//
// impl<'a> Node<'a> {
//     pub fn new(store: Storage, vals: &'a Values, recipes: &Recipes) -> Self {
//         let mut children: Vec<ChildLink> = Vec::new();
//
//         for r in &recipes.val {
//             if r.producable(&store) {
//                 children.push(ChildLink::Producable(None));
//             } else {
//                 children.push(ChildLink::Impossible);
//             }
//         }
//
//         Node {
//             store: store,
//             children: children,
//             values: vals,
//         }
//     }
//
//     pub fn get_child_vec(&self) -> Vec<ChildLink> {
//         self.children.clone()
//     }
//
//     pub fn get_overall_value(&self) -> Int {
//         let store_iter = self.store.iter();
//         let zipped = self.values.iter().zip(store_iter.into_iter());
//
//         zipped.fold(Int::zero(), |acc: Int, (&value, &count)| {
//             let current = Int::from(value) * Int::from(count);
//             if current >= 0 {
//                 acc + current
//             } else {
//                 acc + current.clone() * current
//             }
//         })
//     }
// }
