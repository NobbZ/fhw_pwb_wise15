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
}
