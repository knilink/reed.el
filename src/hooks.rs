use crate::globals::{CURRENT_EMACS_ENV, SIGNAL_TABLES};
use crate::managed_global_ref::ManagedGlobalRef;
use dioxus_core::ScopeId;
use dioxus_signals::Writable;
use emacs::{Env, IntoLisp, Result, Value, defun};

#[defun]
fn use_signal<'e>(env: &'e Env, init: Value) -> Result<Value<'e>> {
    let res = CURRENT_EMACS_ENV.set(env, || {
        SIGNAL_TABLES.with(|signal_tables| {
            let scope_id = dioxus_core::prelude::current_scope_id()?;
            let init_ref = ManagedGlobalRef::from(init);
            let signal = dioxus_hooks::use_signal(|| {
                CURRENT_EMACS_ENV.with(|inner_env| {
                    let res = init_ref.as_ref().call(inner_env, []).unwrap();
                    ManagedGlobalRef::from(res)
                })
            });
            println!("[use_signal][scope_id] {:?}", scope_id);
            let res = signal_tables
                .borrow_mut()
                .get_mut(&scope_id)
                .unwrap()
                .signal
                .set(signal);
            env.call("cons", [scope_id.0.into_lisp(env)?, res.into_lisp(env)?])
        })
    });
    println!("[use_signal][end]");
    res
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
    println!("[signal_set] start");
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
    println!("[signal_set] end");
    res
}

#[defun]
fn use_effect<'e>(_: &'e Env, f: Value) -> Result<()> {
    println!("[use_effect][start]");
    let f_ref = ManagedGlobalRef::from(f);
    dioxus_hooks::use_effect(move || {
        CURRENT_EMACS_ENV.with(|env| {
            println!("[use_effect]");
            f_ref.as_ref().call(env, []).unwrap();
        })
    });
    println!("[use_effect][end]");
    Ok(())
}

#[defun]
fn use_after_render<'e>(_: &'e Env, f: Value) -> Result<()> {
    let f_ref = ManagedGlobalRef::from(f);
    dioxus_core::prelude::use_after_render(move || {
        CURRENT_EMACS_ENV.with(|env| {
            f_ref.as_ref().call(env, []).unwrap();
        })
    });
    Ok(())
}
