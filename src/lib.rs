mod globals;
mod hooks;
mod managed_global_ref;
mod mutation_writer;
mod rendering_context;
mod template;
mod utils;
mod wrapper_components;
use crate::globals::CURRENT_EMACS_ENV;
use crate::rendering_context::RenderingContext;
use crate::template::register;
use std::cell::RefCell;
use std::collections::HashMap;

use emacs::{Env, Result, Value, Vector, defun};
use taffy::{
    CompactLength,
    style_helpers::{FromLength, FromPercent, TaffyAuto, TaffyZero},
};
use utils::symbol_name;

thread_local! {
    static RENDERING_CONTEXTS: RefCell<HashMap<String, RenderingContext>> = RefCell::new(HashMap::new());
}

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "reed")]
fn init(_: &Env) -> Result<()> {
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
fn render_immediate<'e>(env: &'e Env, name: String) -> Result<String> {
    CURRENT_EMACS_ENV.set(env, || {
        RENDERING_CONTEXTS.with(|contexts| {
            let mut ctxs = contexts.borrow_mut();
            let ctx = ctxs.get_mut(&name).unwrap();
            Ok(ctx.render())
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
fn taffy_length<'e>(_: &'e Env, unit: Value, value: f64) -> Result<String> {
    let value: f32 = value as f32;
    let unit_name = symbol_name(unit);
    let res = match unit_name.as_ref() {
        "percent" => Some(serde_lexpr::to_string(&CompactLength::from_percent(value))),
        "length" => Some(serde_lexpr::to_string(&CompactLength::from_length(value))),
        "AUTO" => Some(serde_lexpr::to_string(&CompactLength::AUTO)),
        "ZERO" => Some(serde_lexpr::to_string(&CompactLength::ZERO)),
        _ => None,
    };
    match res {
        Some(Ok(res)) => Ok(res),
        Some(Err(err)) => Err(err.into()),
        None => Err(emacs::Error::msg(format!("unknown unit {}", unit_name))),
    }
}
