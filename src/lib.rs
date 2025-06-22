mod emacs_symbols;
mod events;
mod globals;
mod hooks;
mod managed_global_ref;
mod mutation_writer;
mod rendering_context;
mod template;
mod text_measurement;
mod utils;
mod wrapper_components;
use crate::globals::{CURRENT_EMACS_ENV, DIOXUS_EVENT, take_elisp_error};
use crate::rendering_context::RenderingContext;
use crate::template::register;
use std::cell::RefCell;
use std::collections::HashMap;

use emacs::{CallEnv, Env, IntoLisp, Result, Value, Vector, defun};
use managed_global_ref::ManagedGlobalRef;
use taffy::{
    CompactLength,
    style_helpers::{FromFr, FromLength, FromPercent, TaffyAuto, TaffyZero},
};
use utils::symbol_name;

thread_local! {
    static RENDERING_CONTEXTS: RefCell<HashMap<String, RenderingContext>> = RefCell::new(HashMap::new());
}

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "reed")]
fn init(env: &Env) -> Result<()> {
    fn cleanup(call_env: &CallEnv) -> Result<()> {
        clear_rendering_contexts(&*call_env)
    }
    env.call(
        "add-hook",
        [
            env.intern("kill-emacs-hook")?,
            emacs::lambda!(env, cleanup, 0..0)?,
        ],
    )?;
    Ok(())
}

#[defun]
fn init_tracing<'e>(_: &'e Env) -> Result<()> {
    use tracing_subscriber::fmt;

    // Basic initialization
    fmt().with_ansi(false).init();

    tracing::info!("Tracing initialized");
    Ok(())
}

#[defun]
fn register_template<'e>(
    env: &'e Env,
    template: Vector,
    node_paths: Vector,
    attr_paths: Vector,
) -> Result<usize> {
    register(env, template, node_paths, attr_paths)
}

#[defun]
fn register_app<'e>(env: &'e Env, name: String, root_component: Value) -> Result<()> {
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            let a = RenderingContext::new(root_component);
            contexts.borrow_mut().insert(name.to_string(), a);
        });
    });
    Ok(())
}

#[defun]
fn render_immediate<'e>(env: &'e Env, name: String) -> Result<Value<'e>> {
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            let mut ctxs = contexts.borrow_mut();
            let ctx = ctxs.get_mut(&name).unwrap();
            let render_result = ctx.render();

            match take_elisp_error() {
                None => {
                    match render_result {
                        Some((result, faces)) => {
                            let mut faces_lisp = ().into_lisp(env)?;
                            for (begin, end, face_ref) in faces.iter().rev() {
                                faces_lisp = env.cons(
                                    env.call(
                                        "list",
                                        [
                                            begin.into_lisp(env)?,
                                            end.into_lisp(env)?,
                                            face_ref.bind(env),
                                        ],
                                    )?,
                                    faces_lisp,
                                )?;
                            }
                            // let mut edit_instruction_lisp = ().into_lisp(env)?;
                            // for (begin, end, text) in result.iter() {
                            //     edit_instruction_lisp = env.cons(
                            //         env.call(
                            //             "list",
                            //             [
                            //                 begin.into_lisp(env)?,
                            //                 end.into_lisp(env)?,
                            //                 text.into_lisp(env)?,
                            //             ],
                            //         )?,
                            //         edit_instruction_lisp,
                            //     )?
                            // }
                            // let elapsed = start.elapsed(); // Get elapsed time as `Duration`
                            //
                            // env.cons(edit_instruction_lisp, faces_lisp)
                            env.cons(result.into_lisp(env).unwrap(), faces_lisp)
                        }
                        None => env.list([]),
                    }
                }
                Some(e) => Err(e),
            }
        })
    })
}

#[defun]
fn clear_rendering_contexts<'e>(env: &'e Env) -> Result<()> {
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            contexts.borrow_mut().clear();
        })
    });
    Ok(())
}

#[defun]
fn set_size<'e>(_: &'e Env, name: String, value: Value) -> Result<()> {
    RENDERING_CONTEXTS.with(|contexts| {
        let mut ctxs = contexts.borrow_mut();
        let ctx = ctxs.get_mut(&name).unwrap();
        ctx.set_size((value.car().ok(), value.cdr().ok()));
    });
    Ok(())
}

#[defun]
fn get_size<'e>(env: &'e Env, name: String) -> Result<Value<'e>> {
    let maybe_size =
        RENDERING_CONTEXTS.with(|contexts| contexts.borrow().get(&name).map(|ctx| ctx.get_size()));
    match maybe_size {
        Some((width, height)) => env.cons(width, height),
        None => ().into_lisp(env),
    }
}

#[defun]
fn taffy_length<'e>(_: &'e Env, unit: Value, value: Value) -> Result<String> {
    let value: f32 = if let Ok(value) = value.into_rust::<i64>() {
        value as f32
    } else if let Ok(value) = value.into_rust::<f64>() {
        value as f32
    } else {
        return Err(emacs::Error::msg(format!("value must be a number")));
    };
    let unit_name = symbol_name(unit);
    let res = match unit_name.as_ref() {
        "percent" => Some(serde_lexpr::to_string(&CompactLength::from_percent(value))),
        "length" => Some(serde_lexpr::to_string(&CompactLength::from_length(value))),
        "fr" => Some(serde_lexpr::to_string(&CompactLength::from_fr(value))),
        "auto" => Some(serde_lexpr::to_string(&CompactLength::AUTO)),
        "zero" => Some(serde_lexpr::to_string(&CompactLength::ZERO)),
        _ => None,
    };
    match res {
        Some(Ok(res)) => Ok(res),
        Some(Err(err)) => Err(err.into()),
        None => Err(emacs::Error::msg(format!("unknown unit {}", unit_name))),
    }
}

#[defun]
fn handle_cursor_event<'e>(
    env: &'e Env,
    name: String,
    event_name: Value,
    position: usize,
    event_payload: Value,
) -> Result<()> {
    let event_name: Value = env.call("symbol-name", [event_name])?;
    let event_name: String = event_name.into_rust().unwrap();
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            let mut ctxs = contexts.borrow_mut();
            let ctx = ctxs.get_mut(&name).unwrap();
            ctx.handle_cursor_event(event_name, position, ManagedGlobalRef::from(event_payload));
        });
    });
    match take_elisp_error() {
        None => Ok(()),
        Some(e) => Err(e),
    }
}

#[defun]
fn get_layout<'e>(env: &'e Env, name: String, element_id: usize) -> Result<Value<'e>> {
    let maybe_layout_str = RENDERING_CONTEXTS.with(|contexts| {
        let ctxs = contexts.borrow();
        ctxs.get(&name)
            .map(|ctx| ctx.get_serialized_layout(element_id))
    });
    if let Some(layout_str) = maybe_layout_str {
        let layout_lisp = env.call("read-from-string", [layout_str.into_lisp(env)?])?;
        layout_lisp.car()
    } else {
        ().into_lisp(env)
    }
}

#[defun]
fn get_absolut_location<'e>(env: &'e Env, name: String, element_id: usize) -> Result<Value<'e>> {
    let maybe_location = RENDERING_CONTEXTS.with(|contexts| {
        let ctxs = contexts.borrow();
        ctxs.get(&name)
            .map(|ctx| ctx.get_absolute_location(element_id))
            .flatten()
    });
    if let Some((x, y)) = maybe_location {
        env.cons(x, y)
    } else {
        ().into_lisp(env)
    }
}

#[defun]
fn emit_event<'e>(
    env: &'e Env,
    name: String,
    event_name: Value,
    element_id: usize,
    event_payload: Value,
    propagates: Value,
) -> Result<()> {
    let event_name: Value = env.call("symbol-name", [event_name])?;
    let event_name: String = event_name.into_rust()?;
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            let ctxs = contexts.borrow();
            let ctx = ctxs.get(&name).unwrap();
            ctx.emit_event(
                event_name,
                dioxus_core::ElementId(element_id),
                ManagedGlobalRef::from(event_payload),
                propagates.is_not_nil(),
            );
        });
    });
    Ok(())
}

#[defun]
fn event_stop_propagation<'e>(_: &'e Env) -> Result<()> {
    DIOXUS_EVENT.with(|e| e.stop_propagation());
    Ok(())
}
