use crate::emacs_symbols::*;
use crate::globals::{
    CURRENT_EMACS_ENV, MEMO_TABLES, SIGNAL_TABLES, set_elisp_error, take_elisp_error,
};
use crate::managed_global_ref::ManagedGlobalRef;
use dioxus_core::ScopeId;
use dioxus_signals::Writable;
use emacs::{Env, IntoLisp, Result, Value, defun};

#[defun]
fn use_hook<'e>(env: &'e Env, initializer: Value) -> Result<Value<'e>> {
    let mut captured_error: Option<emacs::Error> = None;
    let value_ref = dioxus_core::use_hook(|| {
        ManagedGlobalRef::from(initializer.call([]).unwrap_or_else(|e| {
            captured_error = Some(e);
            nil.bind(env)
        }))
    });
    match captured_error {
        Some(err) => Err(err),
        None => env.call("identity", [value_ref.bind(env)]),
    }
}

#[defun]
fn use_drop<'e>(_: &'e Env, destroy: Value) -> Result<()> {
    let destroy_ref = ManagedGlobalRef::from(destroy);
    dioxus_core::prelude::use_drop(move || {
        CURRENT_EMACS_ENV.with(|env| {
            if let Err(e) = destroy_ref.as_ref().call(env, []) {
                set_elisp_error(e);
            }
        })
    });
    Ok(())
}

#[defun]
fn use_hook_with_cleanup<'e>(
    env: &'e Env,
    initializer: Value,
    cleanup: Value,
) -> Result<Value<'e>> {
    let mut captured_error: Option<emacs::Error> = None;
    let cleanup_ref = ManagedGlobalRef::from(cleanup);
    let value_ref = dioxus_core::prelude::use_hook_with_cleanup(
        || {
            ManagedGlobalRef::from(initializer.call([]).unwrap_or_else(|e| {
                captured_error = Some(e);
                nil.bind(env)
            }))
        },
        move |value_ref| {
            CURRENT_EMACS_ENV.with(|env| {
                if let Err(err) = cleanup_ref.as_ref().call(env, [value_ref.bind(env)]) {
                    set_elisp_error(err);
                }
            })
        },
    );
    match captured_error {
        Some(err) => Err(err),
        None => env.call("identity", [value_ref.bind(env)]),
    }
}

#[defun]
fn use_signal<'e>(env: &'e Env, init: Value) -> Result<Value<'e>> {
    let mut captured_error: Option<emacs::Error> = None;
    let scope_id = dioxus_core::prelude::current_scope_id()?;

    let signal = dioxus_hooks::use_signal(|| match init.call([]) {
        Ok(res) => ManagedGlobalRef::from(res),
        Err(error) => {
            captured_error = Some(error);
            ManagedGlobalRef::from(nil.bind(env))
        }
    });
    let signal_index = SIGNAL_TABLES.with(|signal_tables| {
        signal_tables
            .borrow_mut()
            .get_mut(&scope_id)
            .unwrap()
            .signal
            .set(signal)
    });

    match captured_error {
        Some(err) => Err(err),
        None => env.cons(scope_id.0.into_lisp(env)?, signal_index.into_lisp(env)?),
    }
}

#[defun]
fn signal_get<'e>(env: &'e Env, handle: Value) -> Result<Value<'e>> {
    CURRENT_EMACS_ENV.set(env, || {
        SIGNAL_TABLES.with(|signal_tables| {
            let scope_id = ScopeId(handle.car::<usize>()?);
            let index = handle.cdr::<usize>()?;
            let value_ref = signal_tables
                .borrow()
                .get(&scope_id)
                .unwrap()
                .signal
                .get(index)();
            env.call("identity", [value_ref.as_ref().bind(env)])
        })
    })
}

#[defun]
fn signal_set<'e>(env: &'e Env, handle: Value, value: Value) -> Result<()> {
    let res = CURRENT_EMACS_ENV.set(env, || {
        SIGNAL_TABLES.with(|signal_tables| {
            let scope_id = ScopeId(handle.car::<usize>()?);
            let index = handle.cdr::<usize>()?;
            signal_tables
                .borrow_mut()
                .get_mut(&scope_id)
                .unwrap()
                .signal
                .get_mut(index)
                .set(ManagedGlobalRef::from(value));
            Ok(())
        })
    });
    res
}

#[defun]
fn use_memo<'e>(env: &'e Env, init: Value) -> Result<Value<'e>> {
    let scope_id = dioxus_core::prelude::current_scope_id()?;
    let init_ref = ManagedGlobalRef::from(init);
    let memo = CURRENT_EMACS_ENV.set(env, || {
        dioxus_hooks::use_memo(move || {
            CURRENT_EMACS_ENV.with(|inner_env| {
                let res = match init_ref.as_ref().call(inner_env, []) {
                    Ok(res) => ManagedGlobalRef::from(res),
                    Err(err) => {
                        set_elisp_error(err);
                        ManagedGlobalRef::from(nil.bind(inner_env))
                    }
                };
                res
            })
        })
    });
    let memo_index = MEMO_TABLES.with(|memo_tables| {
        memo_tables
            .borrow_mut()
            .get_mut(&scope_id)
            .unwrap()
            .signal
            .set(memo)
    });
    match take_elisp_error() {
        Some(err) => Err(err),
        None => env.cons(scope_id.0.into_lisp(env)?, memo_index.into_lisp(env)?),
    }
}

#[defun]
fn memo_get<'e>(env: &'e Env, handle: Value) -> Result<Value<'e>> {
    CURRENT_EMACS_ENV.set(env, || {
        MEMO_TABLES.with(|memo_tables| {
            let scope_id = ScopeId(handle.car::<usize>()?);
            let index = handle.cdr::<usize>()?;
            let value_ref = memo_tables
                .borrow()
                .get(&scope_id)
                .unwrap()
                .signal
                .get(index)();
            env.call("identity", [value_ref.as_ref().bind(env)])
        })
    })
}

#[defun]
fn use_effect<'e>(_: &'e Env, f: Value) -> Result<()> {
    let f_ref = ManagedGlobalRef::from(f);
    dioxus_hooks::use_effect(move || {
        CURRENT_EMACS_ENV.with(|env| {
            if let Err(e) = f_ref.as_ref().call(env, []) {
                set_elisp_error(e);
            }
        })
    });
    Ok(())
}

#[defun]
fn use_after_render<'e>(_: &'e Env, f: Value) -> Result<()> {
    let f_ref = ManagedGlobalRef::from(f);
    dioxus_core::prelude::use_after_render(move || {
        CURRENT_EMACS_ENV.with(|env| {
            if let Err(e) = f_ref.as_ref().call(env, []) {
                set_elisp_error(e);
            }
        })
    });
    Ok(())
}

#[defun]
fn use_context_provider<'e>(env: &'e Env, init: Value) -> Result<Value<'e>> {
    let mut captured_error: Option<emacs::Error> = None;
    let context_ref = dioxus_hooks::use_context_provider(|| {
        ManagedGlobalRef::from(init.call([]).unwrap_or_else(|e| {
            captured_error = Some(e);
            nil.bind(env)
        }))
    });

    match captured_error {
        None => env.call("identity", [context_ref.bind(env)]),
        Some(err) => Err(err),
    }
}

#[defun]
fn use_context<'e>(env: &'e Env) -> Result<Value<'e>> {
    let context_ref = dioxus_hooks::use_context::<ManagedGlobalRef>();
    env.call("identity", [context_ref.bind(env)])
}
