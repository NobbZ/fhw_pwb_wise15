use std::cell::{Cell,RefCell};
use std::slice::Iter;
use data::iter::VecRefIter;

/// A list of amounts that are in storage.
///
/// The number of items of item x is at position x of the Storage
#[derive(PartialEq,Eq,Debug,Clone)]
pub struct Storage {
    store: RefCell<Vec<usize>>,
    fluid: Cell<usize>,
}

impl Storage {
    /// Creates a new Storage. The storage will contain all items listed in `content`
    /// and no cooling fluid.
    /// Please add some cooling fluid after creating the storage by using `set_fluid`.
    pub fn new(content: Vec<usize>) -> Self {
        Storage {
            store: RefCell::new(content),
            fluid: Cell::new(0),
        }
    }

    pub fn get_content(&self) -> Vec<usize> {
        (*self.store.borrow()).clone()
    }

    pub fn set_fluid(self, fluid: usize) -> Self {
        self.fluid.set(fluid);
        self
    }

    pub fn get_fluid(&self) -> usize {
        self.fluid.get()
    }

    pub fn consume(&self, id: usize) -> bool {
        let mut nstore = self.store.borrow_mut();
        if nstore[id] == 0 {
            false
        } else {
            nstore[id] -= 1;
            true
        }
    }

    pub fn heat(&self, amount: usize) -> bool {
        let fluid_available = self.fluid.get();
        if fluid_available >= amount {
            self.fluid.set(fluid_available - amount);
            true
        } else {
            false
        }
    }

    pub fn iter(&self) -> VecRefIter<usize> {
        VecRefIter { r: self.store.borrow() }
    }
}
