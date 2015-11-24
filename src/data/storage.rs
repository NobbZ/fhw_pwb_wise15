use std::sync::Arc;
use std::sync::Mutex;
use std::sync::MutexGuard;

// use data::iter::VecRefIter;

/// A list of amounts that are in storage.
///
/// The number of items of item x is at position x of the Storage
#[derive(Debug,Clone)]
pub struct Storage {
    store: Arc<Mutex<Vec<usize>>>,
    fluid: Arc<Mutex<usize>>,
}

impl Storage {
    /// Creates a new Storage. The storage will contain all items listed in `content`
    /// and no cooling fluid.
    /// Please add some cooling fluid after creating the storage by using `set_fluid`.
    pub fn new(content: Vec<usize>) -> Self {
        Storage {
            store: Arc::new(Mutex::new(content)),
            fluid: Arc::new(Mutex::new(0)),
        }
    }

    pub fn get_content(&self) -> MutexGuard<Vec<usize>> {
        (*self.store).lock().unwrap()
    }

    pub fn set_fluid(self, fluid: usize) -> Self {
        *(*self.fluid).lock().unwrap() = fluid;
        self
    }

    pub fn get_fluid(&self) -> usize {
        *(*self.fluid).lock().unwrap()
    }

    pub fn consume(&self, id: usize) -> bool {
        let mut nstore = self.store.lock().unwrap();
        if nstore[id] == 0 {
            false
        } else {
            nstore[id] -= 1;
            true
        }
    }

    pub fn produce(&self, id: usize) {
        let mut nstore = self.store.lock().unwrap();
        nstore[id] += 1;
    }

    pub fn heat(&self, amount: usize) -> bool {
        let mut fluid_available = self.fluid.lock().unwrap();
        if *fluid_available >= amount {
            *fluid_available -= amount;
            true
        } else {
            false
        }
    }

    // pub fn iter(&self) -> VecRefIter<usize> {
    //     VecRefIter { r: self.store.borrow() }
    // }
}
