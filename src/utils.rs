use emacs::{FromLisp, Value};
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static STRING_INTERNER: RefCell<HashMap<String, &'static str>> = RefCell::new(HashMap::new());
}

pub fn intern(s: &str) -> &'static str {
    STRING_INTERNER.with(|interner| {
        let mut interner = interner.borrow_mut();

        // Return the interned string if it exists
        if let Some(&interned) = interner.get(s) {
            return interned;
        }

        // Otherwise, create a new static string and store it
        let static_str = Box::leak(s.to_string().into_boxed_str());
        interner.insert(s.to_string(), static_str);
        static_str
    })
}

pub fn plist_get<'e, T>(value: Value<'e>, key: &str) -> T
where
    T: FromLisp<'e>,
{
    value
        .env
        .call("plist-get", [value, value.env.intern(key).unwrap()])
        .unwrap()
        .into_rust::<T>()
        .unwrap()
}

pub fn symbol_name(value: Value) -> String {
    value
        .env
        .call("symbol-name", [value])
        .unwrap()
        .into_rust::<String>()
        .unwrap()
}
