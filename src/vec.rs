use smallvec::{Array, SmallVec};
use std::{
    ops::{Deref, DerefMut, Index, IndexMut},
    ptr,
};

use crate::UInt;

pub struct SVec<V: Array>(SmallVec<V>);

impl<V: Array> SVec<V> {
    pub fn pop(&mut self) -> V::Item {
        // This actually gives about 15% speedup somehow
        assert!(self.len() > 0);
        unsafe {
            self.0.set_len(self.0.len() - 1);
            ptr::read(self.0.as_ptr().add(self.0.len()))
        }
    }

    pub fn last(&self) -> &V::Item {
        self.0.last().unwrap()
    }

    pub fn len(&self) -> UInt {
        self.0.len() as UInt
    }

    pub fn new() -> Self {
        SVec(SmallVec::new())
    }
}

impl<V: Array> Index<UInt> for SVec<V> {
    type Output = V::Item;

    fn index(&self, index: UInt) -> &Self::Output {
        &self.0[index as usize]
    }
}

impl<V: Array> IndexMut<UInt> for SVec<V> {
    fn index_mut(&mut self, index: UInt) -> &mut Self::Output {
        &mut self.0[index as usize]
    }
}

impl<V: Array> Deref for SVec<V> {
    type Target = SmallVec<V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<V: Array> DerefMut for SVec<V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
