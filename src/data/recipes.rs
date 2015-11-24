use data::Storage;

use std::slice::Iter;
use std::sync::Arc as Rc;

/// What recipes are available during this game round?
#[derive(PartialEq,Eq,Debug,Default,Clone)]
pub struct Recipes {
    val: Vec<Recipe>,
}

/// A recipe determines what ressources are consumed during a craft and which
/// ones are produced, also it can tell you how much fluid will get consumed
/// during this process.
#[derive(PartialEq,Eq,Debug,Default,Clone)]
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

    pub fn iter(&self) -> Iter<Recipe> {
        self.val.iter()
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

    pub fn produce(&self, store: Rc<Storage>) -> Option<Rc<Storage>> {
        let nstore = store.clone();

        for i in &self.ingredients {
            if !nstore.consume(*i) {
                return None;
            }
        }
        if !nstore.heat(self.fluid) {
            return None;
        }
        for i in &self.output {
            nstore.produce(*i);
        }
        Some(nstore)
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
