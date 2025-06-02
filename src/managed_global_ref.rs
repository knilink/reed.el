use emacs::{Env, GlobalRef, Value};
use std::sync::Arc;

use crate::globals::CURRENT_EMACS_ENV;

#[derive(Debug)]
struct GlobalRefHolder {
    global_ref: Option<GlobalRef>,
}

impl Drop for GlobalRefHolder {
    fn drop(&mut self) {
        if let Some(_ref) = self.global_ref.take() {
            CURRENT_EMACS_ENV.with(|env: &Env| {
                let _ = _ref.free(env);
            });
        }
    }
}

#[derive(Debug)]
pub struct ManagedGlobalRef {
    inner: Arc<GlobalRefHolder>,
}

impl PartialEq for ManagedGlobalRef {
    fn eq(&self, other: &Self) -> bool {
        CURRENT_EMACS_ENV.with(|env: &Env| {
            let value1 = self.bind(env);
            let value2 = other.bind(env);
            value1.eq(value2)
        })
    }
}

impl ManagedGlobalRef {
    pub fn new(global_ref: GlobalRef) -> Self {
        Self {
            inner: Arc::new(GlobalRefHolder {
                global_ref: Some(global_ref),
            }),
        }
    }

    #[inline]
    pub fn as_ref(&self) -> &GlobalRef {
        self.inner
            .global_ref
            .as_ref()
            .expect("GlobalRef should always be Some")
    }

    #[inline]
    pub fn bind<'e, 'g: 'e>(&'g self, env: &'e Env) -> Value<'e> {
        let _ref = self.as_ref();
        _ref.bind(env)
    }
}

impl Clone for ManagedGlobalRef {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

impl From<Value<'_>> for ManagedGlobalRef {
    fn from(value: Value<'_>) -> Self {
        let global_ref = value.make_global_ref();
        ManagedGlobalRef::new(global_ref)
    }
}
