use crate::managed_global_ref::ManagedGlobalRef;
use dioxus_core::ScopeId;
use dioxus_signals::{Signal, UnsyncStorage};
use emacs::Env;
use scoped_tls::scoped_thread_local;
use std::cell::RefCell;
use std::collections::HashMap;

pub struct HandleTable<T> {
    counter: usize,
    table: Vec<T>,
}

impl<T> HandleTable<T> {
    pub fn new() -> Self {
        Self {
            counter: 0,
            table: Vec::<T>::new(),
        }
    }

    #[inline]
    pub fn set(&mut self, item: T) -> usize {
        let index = self.counter;
        self.counter += 1;
        if index < self.table.len() {
            self.table[index] = item; // Overwrite existing
        } else {
            self.table.push(item); // Grow the Vec
        }
        index
    }
    #[inline]
    pub fn get(&self, index: usize) -> &T {
        &self.table[index]
    }
    #[inline]
    pub fn get_mut(&mut self, index: usize) -> &mut T {
        &mut self.table[index]
    }
    #[inline]
    pub fn reset_counter(&mut self) {
        self.counter = 0;
    }
}

pub struct SignalTable {
    pub signal: HandleTable<Signal<ManagedGlobalRef, UnsyncStorage>>,
}

impl SignalTable {
    pub fn new() -> Self {
        Self {
            signal: HandleTable::<Signal<ManagedGlobalRef, UnsyncStorage>>::new(),
        }
    }

    pub fn reset_counters(&mut self) {
        self.signal.reset_counter();
    }
}

scoped_thread_local! {
    pub static ROOT_COMPONENT: ManagedGlobalRef
}

scoped_thread_local! {
    pub static CURRENT_EMACS_ENV: Env
}

thread_local! {
    pub static SIGNAL_TABLES: RefCell<HashMap<ScopeId, SignalTable>> = RefCell::new(HashMap::new());
    pub static TEMPLATE_REGISTRY: RefCell<Vec<dioxus_core::Template>> = RefCell::new(Vec::new());
}
