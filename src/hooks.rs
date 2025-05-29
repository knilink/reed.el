use crate::globals::{CURRENT_EMACS_ENV, SIGNAL_TABLES, set_elisp_error};
use crate::managed_global_ref::ManagedGlobalRef;
use dioxus_core::ScopeId;
use dioxus_signals::Writable;
use emacs::{Env, IntoLisp, Result, Value, defun};

#[defun]
fn use_hook<'e>(env: &'e Env, initializer: Value) -> Result<Value<'e>> {
    let value_ref = dioxus_core::use_hook(|| ManagedGlobalRef::from(initializer.call([]).unwrap()));
    let res = env.call("identity", [value_ref.bind(env)]);
    res
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
    let cleanup_ref = ManagedGlobalRef::from(cleanup);
    let value_ref = dioxus_core::prelude::use_hook_with_cleanup(
        || ManagedGlobalRef::from(initializer.call([]).unwrap()),
        move |value_ref| {
            CURRENT_EMACS_ENV.with(|env| {
                cleanup_ref
                    .as_ref()
                    .call(env, [value_ref.bind(env)])
                    .unwrap();
            })
        },
    );
    env.call("identity", [value_ref.bind(env)])
}

// #[defun]
// fn use_memo<'e>(env: &'e Env, init: Value) -> Result<Value<'e>> {
//     let init_ref = ManagedGlobalRef::from(init);
//     let value_ref_memo = CURRENT_EMACS_ENV.set(env, || {
//         dioxus_hooks::use_memo(move || {
//             CURRENT_EMACS_ENV
//                 .with(|env| ManagedGlobalRef::from(init_ref.as_ref().call(env, []).unwrap()))
//         })
//     });
//     let value_ref = value_ref_memo.read();
//     env.call("identity", [value_ref.bind(env)])
// }

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
            let res = signal_tables
                .borrow_mut()
                .get_mut(&scope_id)
                .unwrap()
                .signal
                .set(signal);
            env.call("cons", [scope_id.0.into_lisp(env)?, res.into_lisp(env)?])
        })
    });
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
