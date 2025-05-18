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

use emacs::{Env, IntoLisp, Result, Value, Vector, defun};

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
fn test_error<'e>(env: &'e Env, f: Value) -> Result<()> {
    use emacs::ErrorKind;
    match env.call(f, []) {
        Ok(_) => {}
        Err(error) => {
            // env.call("message", ["error string %s", error]);
            let a = error.downcast_ref::<ErrorKind>();

            if let Some(ErrorKind::Signal { symbol, data }) = a {
                // Accessing TempValue is unsafe, so ensure it's done within the scope
                // where the Env from which the error originated is still valid.
                // Get the data (error message) associated with the signal.  It's typically a list.
                let data_value = unsafe { data.value(env) };
                println!(
                    "data_value: {:?}",
                    env.call("error-message-string", [data_value])
                        .unwrap()
                        .into_rust::<String>()
                );
                // let maybe_message_value = data_value.car::<String>();
                // println!("maybe_message_value: {:?}", maybe_message_value);
                //
                // // Assuming the message is the first element in the list.
                // if let Ok(message) = maybe_message_value {
                //     println!("resolved error message{:?}", message);
                // }
            } else {
                println!("generic error message {}", error.to_string()); // Fallback to the generic error message.
            }
        }
    };
    Ok(())
}
