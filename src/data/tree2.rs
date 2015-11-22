use ramp::int::Int;

use std::ops::Index;
use std::sync::Arc;
use std::sync::Mutex;

use data::Recipe;
use data::Recipes;
use data::storage::Storage;
use data::values::Values;

#[derive(Debug)]
pub enum Node<'a> {
    Pending(&'a Storage, &'a Recipe, &'a Values),
    Active(&'a Storage, Int, Vec<Option<Box<Node<'a>>>>),
    Finished(Int, Vec<usize>),
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

    pub fn is_pending(&self) -> bool {
        match self {
            &Node::Pending(_,_,_) => true,
            _ => false
        }
    }

    pub fn is_active(&self) -> bool {
        match self {
            &Node::Active(_,_,_) => true,
            _ => false
        }
    }

    pub fn is_finished(&self) -> bool {
        match self {
            &Node::Finished(_,_) => true,
            _ => false
        }
    }

    pub fn is_impossible(&self) -> bool {
        match self {
            &Node::Impossible => true,
            _ => false
        }
    }

    pub fn activate(&mut self) {
        match self {
            &mut Node::Pending(s, r, v) => {
                match r.produce(s) {
                    None => {self = &mut Node::Impossible;},
                    Some(s) => {
                        let mut cs: Vec<Option<Box<Node>>> = Vec::new();
                        let value = Int::zero();

                        for r in
                    }
                }
            },
            _ => ()
        }
    }

    pub fn get_child_at(&'a mut self, idx: Vec<usize>) -> Option<*mut Node> {
        use self::Node as N;
        match self {
            &mut N::Impossible => None,
            &mut N::Pending(_,_,_) => {
                self.activate();
                if idx.is_empty() {
                    Some(self)
                } else {
                    self.get_child_at(idx)
                }
            },
        }

    }
}

// impl<'a> Index<&'a [usize]> for Arc<Mutex<Node<'a>>> {
//     type Output = Option<Box<&'a Node<'a>>>;
//
//     fn index(&'a self, index: &'a [usize]) -> &Self::Output {
//         use self::Node::*;
//         let path = index.split_first();
//         match path {
//             Some((i, is)) => match self {
//                 &Active(_,_,v) => match v[*i] {
//                     Some(n) => &n[is],
//                     None => &None
//                 },
//                 &Pending(_,_,_) => unimplemented!(),
//                 &Finished(_,_) => unimplemented!(),
//                 &Impossible => &None
//             },
//             None => &Some(Box::new(self))
//         }
//     }
// }
