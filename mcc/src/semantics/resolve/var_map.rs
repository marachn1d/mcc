use ast::parse::prelude::*;
use std::collections::hash_map::Entry;
use std::sync::atomic::{AtomicU32, Ordering};
use symtab::StoreT;
use symtab::{Key, Store};

static COUNTER: AtomicU32 = AtomicU32::new(0);

#[derive(Clone)]
pub struct VarMap<'a> {
    counter: &'static AtomicU32,
    pub map: HashMap<Key<'a>, Var<'a>>,
    pub s: &'a Store,
}

impl<'a> VarMap<'a> {
    pub fn new(s: &'a Store) -> Self {
        Self {
            counter: &COUNTER,
            map: HashMap::new(),
            s,
        }
    }

    pub fn new_scope(other: &Self) -> Self {
        let mut new = other.clone();
        for Var {
            from_current_block, ..
        } in new.map.values_mut()
        {
            *from_current_block = false;
        }
        new
    }

    pub fn add_var(&mut self, dec: &VarDec<'a>) -> Result<(), super::Error> {
        use std::collections::hash_map::Entry;
        match self.map.entry(dec.name) {
            Entry::Occupied(mut e) => {
                let prev = e.get();
                if prev.from_current_block && !prev.has_external_linkage {
                    return Err(super::Error::ConflictingDec);
                };
                e.insert(Var::var(dec));
                Ok(())
            }
            Entry::Vacant(e) => {
                e.insert(Var::var(dec));
                Ok(())
            }
        }
    }

    pub fn param_name(&self, name: &Key<'a>) -> Var<'a> {
        Var {
            name: self.temp_name(name),
            from_current_block: true,
            has_external_linkage: false,
        }
    }

    fn temp_name(&self, name: &Key) -> Key<'a> {
        let name = format!(
            "t{name}.{number}",
            number = COUNTER.fetch_add(1, Ordering::SeqCst)
        );

        unsafe { self.s.store_unchecked(&name) }
    }

    pub fn add_fn(&mut self, f: &FnDec<'a>) -> Result<(), super::Error> {
        if let Some(var) = self.map.get(&f.name) {
            if var.from_current_block && !var.has_external_linkage {
                return Err(super::Error::DuplicateDeclaration);
            }
        }
        self.map.insert(f.name, Var::fun(f));
        Ok(())
    }
}

impl VarMap<'_> {
    fn inc(&mut self) {
        self.counter.fetch_add(1, Ordering::AcqRel);
    }

    const fn var<'a>(VarDec { sc, name, .. }: &VarDec<'a>) -> Var<'a> {
        Var {
            name: *name,
            from_current_block: true,
            has_external_linkage: Self::linkage(sc),
        }
    }

    const fn fun<'a>(FnDec { sc, name, .. }: &FnDec<'a>) -> Var<'a> {
        Var {
            name: *name,
            from_current_block: true,
            has_external_linkage: Self::linkage(sc),
        }
    }

    const fn linkage(sc: &Option<StorageClass>) -> bool {
        if let Some(StorageClass::Static) = sc {
            false
        } else {
            true
        }
    }
}

use std::collections::HashMap;
//type VarMap<'a> = HashMap<Key<'a>, Var<'a>>;
#[derive(Clone, Debug, PartialEq)]
pub struct Var<'a> {
    pub name: Key<'a>,
    pub from_current_block: bool,
    pub has_external_linkage: bool,
}

fn temp_name<'a>(name: &Key, s: &'a Store) -> Key<'a> {
    let name = format!(
        "t{name}.{number}",
        number = COUNTER.fetch_add(1, Ordering::SeqCst)
    );

    unsafe { s.store_unchecked(&name) }
}

impl Var<'_> {
    const fn linkage(sc: &Option<StorageClass>) -> bool {
        !matches!(sc, Some(StorageClass::Static))
    }
}

impl<'a> Var<'a> {
    fn fun(FnDec { name: n, sc, .. }: &FnDec<'a>) -> Self {
        Self {
            name: *n,
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
        }
    }

    const fn var(VarDec { name, sc, .. }: &VarDec<'a>) -> Self {
        Self {
            name: *name,
            from_current_block: true,
            has_external_linkage: Self::linkage(sc),
        }
    }
}

/*
impl<'a> Var<'a> {
    fn new_unique(key: &Key, sc: &Option<StorageClass>, s: &'a Store) -> Self {
        Self {
            name: temp_name(key, s),
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
            number: COUNTER.fetch_add(1, Ordering::SeqCst),
        }
    }

    fn new_var(name: Key<'a>, sc: &Option<StorageClass>) -> Self {
        Self {
            name,
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
            number: COUNTER.fetch_add(1, Ordering::SeqCst),
        }
    }
    fn new_fn(name: Key<'a>, sc: &Option<StorageClass>) -> Self {
        Self {
            name,
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
            number: COUNTER.fetch_add(1, Ordering::SeqCst),
        }
    }
}
*/
