use std::cell::Ref;
use std::slice::Iter;

pub struct VecRefIter<'a, T: 'a> {
    pub r: Ref<'a, Vec<T>>
}

impl <'a, 'b: 'a, T: 'a> IntoIterator for &'b VecRefIter<'a, T> {
    type IntoIter = Iter<'a, T>;
    type Item = &'a T;

    fn into_iter(self) -> Iter<'a, T> {
        self.r.iter()
    }
}
