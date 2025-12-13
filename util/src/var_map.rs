use ast::Ident;
use ast::parse::StorageClass;
use derive_more::{Deref, DerefMut};
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};

static COUNTER: AtomicU32 = AtomicU32::new(0);

#[derive(Deref, DerefMut, Clone)]
pub struct VarMap(HashMap<Ident, Var>);

#[derive(Clone, Debug, PartialEq)]
pub struct Var {
    pub name: Ident,
    pub from_current_block: bool,
    pub has_external_linkage: bool,
}

impl Var {
    pub fn new_var(name: &Ident, sc: &Option<StorageClass>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
        }
    }
    pub fn new_fn(name: &Ident, sc: &Option<StorageClass>) -> Self {
        Self {
            name: name.clone(),
            from_current_block: true,
            has_external_linkage: sc.is_some_and(|x| x == StorageClass::Extern),
        }
    }
}

impl Borrow<HashMap<Ident, Var>> for VarMap {
    fn borrow(&self) -> &HashMap<Ident, Var> {
        &self.0
    }
}

impl BorrowMut<HashMap<Ident, Var>> for VarMap {
    fn borrow_mut(&mut self) -> &mut HashMap<Ident, Var> {
        &mut self.0
    }
}

impl VarMap {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn new_var(&self, name: &Ident) -> Ident {
        Ident::from(format!(
            "t{name}.{number}",
            number = COUNTER.fetch_add(1, Ordering::SeqCst)
        ))
    }
}
