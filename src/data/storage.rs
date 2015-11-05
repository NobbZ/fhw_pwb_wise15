/// A list of amounts that are in storage.
///
/// The number of items of item x is at position x of the Storage
#[derive(PartialEq,Eq,Debug,Clone)]
pub struct Storage {
    store: Vec<usize>,
    fluid: usize,
}

impl Storage {
    pub fn new(content: Vec<usize>) -> Self {
        Storage {
            store: content,
            fluid: 0,
        }
    }

    pub fn consume(&mut self, id: usize) -> bool {
        if self.store[id] == 0 {
            false
        } else {
            self.store[id] = self.store[id] - 1;
            true
        }
    }

    pub fn heat(&mut self, amount: usize) -> bool {
        if self.fluid >= amount {
            self.fluid -= amount;
            true
        } else {
            false
        }
    }
}
