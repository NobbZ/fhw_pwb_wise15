//! Datatypes used for Bookkeeping stuff.

pub mod tree;
pub mod storage;

use self::storage::Storage;

/// A list of the items values.
///
/// The list is indexed by the ID of an item.
#[derive(PartialEq,Eq,Debug)]
pub struct Values {
    val: Vec<isize>
}

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

impl Values {
    pub fn new(content: Vec<isize>) -> Self {
        Values {
            val: content
        }
    }
}

impl Recipes {
    pub fn new(content: Vec<Recipe>) -> Self {
        Recipes {
            val: content
        }
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

    pub fn producable(&self, store: &Storage) -> bool {
        let mut nstore = store.clone();

        for i in &self.ingredients {
            if !nstore.consume(*i) {
                return false;
            }
        }
        return true;
    }
}
