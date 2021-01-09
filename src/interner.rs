use std::{cell::RefCell, hash::BuildHasherDefault, mem};

use rustc_hash::FxHasher;
use smol_str::SmolStr;
use string_interner::{backend::BucketBackend, symbol::SymbolU32, StringInterner, Symbol};

pub type StrId = SymbolU32;

thread_local! {
    static INTERN: RefCell<StringInterner::<StrId, BucketBackend<StrId>, BuildHasherDefault<FxHasher>>> = RefCell::new(StringInterner::with_capacity(500));
}

/// Return interned variant of given string
pub fn intern<T: AsRef<str>>(of: T) -> StrId {
    INTERN.with(|i| i.borrow_mut().get_or_intern(of))
}

/// Return string of interned id
pub fn str(of: StrId) -> SmolStr {
    INTERN.with(|i| SmolStr::new(i.borrow().resolve(of).unwrap()))
}

/// Hash an interned string. This uses FNV like clox, but only
/// on a single value (the id itself, not the string bytes).
fn hash(id: StrId) -> usize {
    let mut hash = 2166136261;
    hash ^= id.to_usize();
    hash *= 16777619;
    hash
}

fn mask(cap: usize) -> usize {
    cap - 1
}

/// A simple map or hashtable using interned strings as keys.
/// API is mostly identical to stdlib HashMap.
#[derive(Debug, Clone, PartialEq)]
pub struct Map<V: Clone> {
    pub len: u32,
    pub entries: Vec<Option<Entry<V>>>,
}

impl<V: Clone> Map<V> {
    pub fn get(&self, id: StrId) -> Option<&V> {
        self.entry(id).as_ref().map(|e| &e.value)
    }

    pub fn insert(&mut self, key: StrId, value: V) -> Option<V> {
        self.reserve(1);
        let entry = self.entry_mut(key);
        let previous = mem::replace(entry, Some(Entry { key, value }));
        self.len += previous.is_none() as u32;
        previous.map(|p| p.value)
    }

    pub fn add_missing(&mut self, other: &Map<V>) {
        self.reserve(other.len());
        for other_entry in other.entries.iter().filter_map(|e| e.as_ref()) {
            let entry = self.entry_mut(other_entry.key);
            if entry.is_none() {
                *entry = Some(other_entry.clone());
                self.len += 1;
            }
        }
    }

    pub fn entry(&self, id: StrId) -> &Option<Entry<V>> {
        let mask = mask(self.entries.capacity());
        let mut index = hash(id) & mask;
        loop {
            match &self.entries[index] {
                Some(entry) if entry.key == id => return &self.entries[index],
                None => return &self.entries[index],
                _ => index = (index + 1) & mask,
            }
        }
    }

    pub fn entry_mut(&mut self, id: StrId) -> &mut Option<Entry<V>> {
        let mask = mask(self.entries.capacity());
        let mut index = hash(id) & mask;
        loop {
            match &mut self.entries[index] {
                Some(entry) if entry.key == id => return &mut self.entries[index],
                None => return &mut self.entries[index],
                _ => index = (index + 1) & mask,
            }
        }
    }

    pub fn values(&self) -> impl Iterator<Item = &V> + '_ {
        self.entries
            .iter()
            .filter_map(|e| e.as_ref())
            .map(|e| &e.value)
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    fn reserve(&mut self, amount: usize) {
        if self.len() + amount > ((self.entries.capacity() as f64) * 0.75) as usize {
            self.grow();
        }
    }

    fn grow(&mut self) {
        let new = vec![None; self.entries.capacity() * 2];
        let old = mem::replace(&mut self.entries, new);

        for entry in old.into_iter().filter_map(|e| e) {
            let empty_entry = self.entry_mut(entry.key);
            *empty_entry = Some(entry);
        }
    }

    pub fn new() -> Self {
        Self {
            len: 0,
            entries: vec![None; 8],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Entry<V: Clone> {
    pub key: StrId,
    pub value: V,
}
