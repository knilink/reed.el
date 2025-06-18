use crate::managed_global_ref::ManagedGlobalRef;
use dioxus_core::ScopeId;
use dioxus_signals::{Memo, Signal, UnsyncStorage};
use emacs::Env;
use scoped_tls::scoped_thread_local;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct SignalTable<T> {
    pub signal: HandleTable<T>,
}

impl<T> SignalTable<T> {
    pub fn new() -> Self {
        Self {
            signal: HandleTable::<T>::new(),
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

scoped_thread_local! {
    pub static DIOXUS_EVENT:dioxus_core::Event<ManagedGlobalRef>
}

thread_local! {
    pub static SIGNAL_TABLES: RefCell<HashMap<ScopeId, SignalTable<Signal<ManagedGlobalRef, UnsyncStorage>>>> = RefCell::new(HashMap::new());
    pub static MEMO_TABLES: RefCell<HashMap<ScopeId, SignalTable<Memo<ManagedGlobalRef>>>> = RefCell::new(HashMap::new());
    pub static TEMPLATE_REGISTRY: RefCell<Vec<dioxus_core::Template>> = RefCell::new(Vec::new());
    pub static LAST_ELISP_ERROR: RefCell<Option<emacs::Error>> = RefCell::new(None);
}

// Store an error in thread-local storage
pub fn set_elisp_error(err: emacs::Error, callback_ref: &ManagedGlobalRef) {
    let func_desc = CURRENT_EMACS_ENV.with(|env| {
        env.call("prin1-to-string", [callback_ref.bind(env)])
            .unwrap()
            .into_rust::<String>()
            .unwrap()
    });
    let stack_trace = std::backtrace::Backtrace::capture();

    // Print stack trace for debugging
    eprintln!(
        "Elisp callback error occurred: {}\nStack trace:\n{}\n{}",
        err, stack_trace, func_desc
    );

    LAST_ELISP_ERROR.with(|e| {
        *e.borrow_mut() = Some(err);
    });
}

// Retrieve and clear the last error
pub fn take_elisp_error() -> Option<emacs::Error> {
    LAST_ELISP_ERROR.with(|e| e.borrow_mut().take())
}
