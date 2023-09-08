use std::{rc::{Rc, Weak}, cell::{RefCell, Ref, RefMut}, ops::{Deref, DerefMut}, mem, fmt};

/// Essentially a wrapper around a Vec<T> that allows multiple references to
/// contained elements and can handle circular references. Can also be
/// described as a "sorted allocation list".
pub struct MemPool<T> {
    items: Vec<Rc<RefCell<T>>>,
}

impl<T> MemPool<T> {
    /// Creates a new empty MemPool<T>.
    pub fn new() -> Self {
        Self { items: vec![] }
    }

    /// Adds a new element to the MemPool<T> and returns a reference to it.
    pub fn add(&mut self, item: T) -> MemRef<T> {
        let rc = Rc::new(RefCell::new(item));
        let weak = Rc::downgrade(&rc);
        self.items.push(rc);
        MemRef { reference: weak }
    }

    /// Returns a reference to the element at the given index.
    pub fn get(&self, index: usize) -> MemRef<T> {
        MemRef {
            reference: Rc::downgrade(&self.items[index]),
        }
    }

    /// Removes the element at the given index, replacing it with the last
    /// element.
    pub fn remove(&mut self, index: usize) -> Rc<RefCell<T>> {
        self.items.swap_remove(index)
    }

    pub fn iter(&self) -> impl Iterator<Item = MemRef<T>> + '_ {
        self.items.iter().map(|rc| MemRef {
            reference: Rc::downgrade(rc),
        })
    }
}

/// Holds a reference to an element in a MemPool<T>.
pub struct MemRef<T> {
    reference: Weak<RefCell<T>>,
}

impl<T> MemRef<T> {
    /// Returns a derefenced read-only reference to the element.
    pub fn get(&self) -> MemDeref<'_, T> {
        let rc = self.reference.upgrade().unwrap();
        let borrowed = unsafe { mem::transmute(rc.borrow()) };
        MemDeref {
            _rc: rc,
            borrowed,
        }
    }

    /// Returns a derefenced mutable reference to the element.
    pub fn get_mut(&self) -> MemDerefMut<'_, T> {
        let rc = self.reference.upgrade().unwrap();
        let borrowed = unsafe { mem::transmute(rc.borrow_mut()) };
        MemDerefMut {
            _rc: rc,
            borrowed,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for MemRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &*self.get())
    }
}

impl<T> Clone for MemRef<T> {
    fn clone(&self) -> Self {
        Self {
            reference: self.reference.clone(),
        }
    }
}

impl<T> From<Weak<RefCell<T>>> for MemRef<T> {
    fn from(reference: Weak<RefCell<T>>) -> Self {
        Self { reference }
    }
}

impl<T> PartialEq for MemRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.reference.ptr_eq(&other.reference)
    }
}

impl<T> Eq for MemRef<T> {}

pub struct MemDeref<'a, T> {
    _rc: Rc<RefCell<T>>,
    borrowed: Ref<'a, T>,
}

impl<T> Deref for MemDeref<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.borrowed
    }
}

pub struct MemDerefMut<'a, T> {
    _rc: Rc<RefCell<T>>,
    borrowed: RefMut<'a, T>,
}

impl<T> Deref for MemDerefMut<'_, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.borrowed
    }
}

impl<T> DerefMut for MemDerefMut<'_, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.borrowed
    }
}
impl<T> Drop for MemDerefMut<'_, T> {
    fn drop(&mut self) {
    }
}
