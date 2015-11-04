//! Datatypes used for Bookkeeping stuff.

/// A list of the items values.
///
/// The list is indexed by the ID of an item.
#[derive(PartialEq,Eq,Debug)]
pub struct Values {
    val: Vec<isize>
}


/// A list of amounts that are in storage.
///
/// The number of items of item x is at position x of the Storage
#[derive(PartialEq,Eq,Debug)]
pub struct Storage {
    val: Vec<usize>
}

/// A Recipe shows which items can be produced by combining other items, also it
/// tells how much cooling fluid will be consumed in the process.
pub struct Recipes {
    val: Vec<Recipe>
}

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

impl Storage {
    pub fn new(content: Vec<usize>) -> Self {
        Storage {
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