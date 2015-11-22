//! Datatypes used for Bookkeeping stuff.

pub mod tree;
pub mod storage;
pub mod values;
pub mod iter;

use data::storage::Storage;

/// What recipes are available during this game round?
#[derive(PartialEq,Eq,Debug)]
pub struct Recipes {
    val: Vec<Recipe>,
}

/// A recipe determines what ressources are consumed during a craft and which
/// ones are produced, also it can tell you how much fluid will get consumed
/// during this process.
#[derive(PartialEq,Eq,Debug)]
pub struct Recipe {
    ingredients: Vec<usize>,
    output: Vec<usize>,
    fluid: usize,
}

impl Recipes {
    pub fn new(content: Vec<Recipe>) -> Self {
        Recipes {
            val: content
        }
    }

    pub fn count(&self) -> usize {
        self.val.len()
    }
}

impl Recipe {
    pub fn new(input: Vec<usize>, output: Vec<usize>, fluid: usize) -> Self {
        Recipe {
            ingredients: input,
            output: output,
            fluid: fluid
        }
    }

    pub fn produce(&self, store: &Storage) -> Storage {
        let nstore = store.clone();

        for i in &self.ingredients {
            nstore.consume(*i);
        }
        for i in &self.output {
            nstore.produce(*i);
        }
        nstore.heat(self.fluid);
        nstore
    }

    pub fn producable(&self, store: &Storage) -> bool {
        let nstore = store.clone();

        for i in &self.ingredients {
            if !nstore.consume(*i) {
                return false;
            }
        }
        return true;
    }
}
