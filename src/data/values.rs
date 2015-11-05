use std::slice::Iter;

/// A list of the items values.
///
/// The list is indexed by the ID of an item.
#[derive(PartialEq,Eq,Debug)]
pub struct Values {
    val: Vec<isize>
}

impl Values {
    pub fn new(content: Vec<isize>) -> Self {
        Values {
            val: content
        }
    }

    pub fn get_value(&self, id: usize) -> isize {
        self.val[id]
    }

    pub fn as_vec(&self) -> Vec<isize> {
        self.val.clone()
    }

    pub fn as_ref_vec(&self) -> &Vec<isize> {
        self.val.as_ref()
    }

    pub fn iter(&self) -> Iter<isize> {
        self.val.iter()
    }
}
