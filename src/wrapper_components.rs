use crate::globals::{
    CURRENT_EMACS_ENV, MEMO_TABLES, ROOT_COMPONENT, SIGNAL_TABLES, SignalTable, TEMPLATE_REGISTRY,
    set_elisp_error,
};
use crate::managed_global_ref::ManagedGlobalRef;
use crate::utils::{intern, plist_get, symbol_name};
use dioxus_core::{Element, IntoDynNode, fc_to_builder};
use dioxus_core_macro::{Props, component};
use emacs::{Value, Vector};

struct ListIter<'a> {
    list: Value<'a>,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Value<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.list.is_not_nil() {
            let a = self.list.car().unwrap();
            self.list = self.list.cdr().unwrap();
            a
        } else {
            None
        }
    }
}

fn build_element(vnode: Value) -> Element {
    let mut cur = vnode;
    let key: Option<String> = {
        let key: Value = cur.car().unwrap();
        if key.is_not_nil() {
            Some(key.into_rust::<String>().unwrap())
        } else {
            None
        }
    };
    cur = cur.cdr().unwrap();
    let template_id: usize = cur.car().unwrap();
    cur = cur.cdr().unwrap();
    let dynamic_nodes: Vector = cur.car().unwrap();
    cur = cur.cdr().unwrap();
    let dynamic_attrs: Vector = cur.car().unwrap();

    let template = TEMPLATE_REGISTRY.with(|registry| {
        let registry = registry.borrow();
        registry[template_id]
    });
    dioxus_core::Element::Ok(dioxus_core::VNode::new(
        key,
        template,
        build_dynamic_nodes(dynamic_nodes).into_boxed_slice(),
        build_dynamic_attrs(dynamic_attrs).into_boxed_slice(),
    ))
}

fn build_dynamic_nodes(nodes: Vector) -> Vec<dioxus_core::DynamicNode> {
    nodes
        .into_iter()
        .map(|node| {
            let node_type: Value = node.car().unwrap();
            let node_type_name = symbol_name(node_type);
            match node_type_name.as_str() {
                "dynamic-node:component" => {
                    let node_fields: Value = node.cdr().unwrap();
                    dioxus_core::DynamicNode::Component({
                        use dioxus_core::prelude::Properties;
                        let component_ref =
                            ManagedGlobalRef::from(plist_get::<Value>(node_fields, ":type"));
                        let props_ref =
                            ManagedGlobalRef::from(plist_get::<Value>(node_fields, ":props"));
                        let __comp = ({
                            fc_to_builder(WrapperComponent)
                                .component_ref(component_ref)
                                .props_ref(props_ref)
                                .build()
                        })
                        .into_vcomponent(WrapperComponent);
                        __comp
                    })
                }
                "dynamic-node:element" => {
                    let element: Value = node.cdr::<Value>().unwrap().car().unwrap();
                    if let Ok(ele) = element.into_rust::<String>() {
                        ele.into_dyn_node()
                    } else if let Ok(ele) = element.into_rust::<i64>() {
                        ele.to_string().into_dyn_node()
                    } else if !element.is_not_nil() {
                        // {null}
                        dioxus_core::DynamicNode::default()
                    } else if let Ok(first) = element.car::<Value>() {
                        if !first.is_not_nil() || first.into_rust::<String>().is_ok() {
                            build_element(element).into_dyn_node()
                        } else {
                            let iter = ListIter { list: element };
                            iter.map(|item| build_element(item)).into_dyn_node()
                        }
                    } else {
                        dioxus_core::DynamicNode::default()
                    }
                }
                _ => {
                    panic!(
                        "Unknown dynamic node type: {:?}. \
                         Expected either 'dynamic-node:element' or 'dynamic-node:component'",
                        node_type_name
                    );
                }
            }
        })
        .collect()
}

fn build_attr_value(value: Value) -> dioxus_core::AttributeValue {
    // #[allow(deprecated)]
    // super::$name(event_handler)

    if let Ok(value) = value.into_rust::<String>() {
        dioxus_core::AttributeValue::Text(value)
    } else if let Ok(value) = value.into_rust::<i64>() {
        dioxus_core::AttributeValue::Int(value)
    } else if let Ok(value) = value.into_rust::<f64>() {
        dioxus_core::AttributeValue::Float(value)
    } else if value.env.call("functionp", [value]).unwrap().is_not_nil() {
        let callback_ref = ManagedGlobalRef::from(value);
        dioxus_core::AttributeValue::listener(move |e: dioxus_core::Event<ManagedGlobalRef>| {
            CURRENT_EMACS_ENV.with(|env| {
                if let Err(e) = callback_ref.as_ref().call(env, [e.data.bind(env)]) {
                    set_elisp_error(e, &callback_ref);
                }
            });
        })
    } else {
        dioxus_core::AttributeValue::any_value(ManagedGlobalRef::from(value))
    }
}

fn build_dynamic_attrs(attrs: Vector) -> Vec<Box<[dioxus_core::Attribute]>> {
    let res = attrs
        .into_iter()
        .map(|attr| {
            let tag: String = plist_get(attr, ":tag");
            let name: String = plist_get(attr, ":name");
            let value: Value = plist_get(attr, ":value");
            let value: dioxus_core::AttributeValue = if name == "ref" {
                dioxus_core::AttributeValue::any_value(ManagedGlobalRef::from(value))
            } else {
                build_attr_value(plist_get(attr, ":value"))
            };
            let volatile = tag == "input" && name == "value";
            if value == dioxus_core::AttributeValue::None {
                #[cfg(feature = "tracing")]
                tracing::warn!(
                    "[build_dynamic_attrs] unknown attribute type tag:{} name:{}",
                    tag,
                    name
                );
            }
            [dioxus_core::Attribute::new(
                intern(&name),
                value,
                None,
                volatile,
            )]
            .to_vec()
            .into_boxed_slice()
        })
        .collect();
    res
}

// fn error_message_element(message: String) -> Element {
//     dioxus_core::Element::Ok({
//         let __dynamic_nodes: [dioxus_core::DynamicNode; 1usize] =
//             [dioxus_core::DynamicNode::Text(dioxus_core::VText { value: message })];
//
//         static __TEMPLATE_ROOTS: &[dioxus_core::TemplateNode] =
//             &[dioxus_core::TemplateNode::Element {
//                 tag: "error",
//                 namespace: None,
//                 attrs: &[],
//                 children: &[dioxus_core::TemplateNode::Dynamic { id: 0usize }],
//             }];
//
//         static ___TEMPLATE: dioxus_core::Template = dioxus_core::Template {
//             roots: __TEMPLATE_ROOTS,
//             node_paths: &[&[0u8, 0u8]],
//             attr_paths: &[],
//         };
//
//         let __vnodes =
//             dioxus_core::VNode::new(None, ___TEMPLATE, Box::new(__dynamic_nodes), Box::new([]));
//         __vnodes
//     })
// }

#[component]
pub fn WrapperComponent(component_ref: ManagedGlobalRef, props_ref: ManagedGlobalRef) -> Element {
    let scope_id = dioxus_core::prelude::use_hook_with_cleanup(
        || {
            let scope_id = dioxus_core::prelude::current_scope_id().unwrap();
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.insert(scope_id, SignalTable::new());
            });
            MEMO_TABLES.with(|memo_tabels| {
                let mut memo_tabels = memo_tabels.borrow_mut();
                memo_tabels.insert(scope_id, SignalTable::new());
            });
            scope_id
        },
        |scope_id| {
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.remove(&scope_id);
            });
            MEMO_TABLES.with(|memo_tabels| {
                let mut memo_tabels = memo_tabels.borrow_mut();
                memo_tabels.remove(&scope_id);
            });
        },
    );

    SIGNAL_TABLES.with(|signal_tabels| {
        let mut signal_tabels = signal_tabels.borrow_mut();
        if let Some(table) = signal_tabels.get_mut(&scope_id) {
            table.reset_counters();
        }
    });

    CURRENT_EMACS_ENV.with(|env| {
        // let element = match component_ref.as_ref().call(env, [props_ref.bind(env)]) {
        //     Ok(element) => element,
        //     Err(err) => {
        //         return error_message_element(err.to_string());
        //     }
        // };
        let element = component_ref
            .as_ref()
            .call(env, [props_ref.bind(env)])
            .unwrap();
        build_element(element)
    })
}

#[component]
pub fn RootComponent() -> Element {
    let scope_id = dioxus_core::prelude::use_hook_with_cleanup(
        || {
            let scope_id = dioxus_core::prelude::current_scope_id().unwrap();
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.insert(scope_id, SignalTable::new());
            });
            MEMO_TABLES.with(|memo_tabels| {
                let mut memo_tabels = memo_tabels.borrow_mut();
                memo_tabels.insert(scope_id, SignalTable::new());
            });
            scope_id
        },
        |scope_id| {
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.remove(&scope_id);
            });
            MEMO_TABLES.with(|memo_tabels| {
                let mut memo_tabels = memo_tabels.borrow_mut();
                memo_tabels.remove(&scope_id);
            });
        },
    );

    SIGNAL_TABLES.with(|signal_tabels| {
        let mut signal_tabels = signal_tabels.borrow_mut();
        if let Some(table) = signal_tabels.get_mut(&scope_id) {
            table.reset_counters();
        }
    });

    CURRENT_EMACS_ENV.with(|env| {
        ROOT_COMPONENT.with(|root_component_ref| {
            // let element = match root_component_ref.as_ref().call(env, []) {
            //     Ok(element) => element,
            //     Err(err) => return error_message_element(err.to_string()),
            // };
            let element = root_component_ref.as_ref().call(env, []).unwrap();

            let mut cursor = element;
            let key: Option<String> = {
                let key: Value = cursor.car().unwrap();
                if key.is_not_nil() {
                    Some(key.into_rust::<String>().unwrap())
                } else {
                    None
                }
            };
            cursor = cursor.cdr().unwrap();
            let template_id: usize = cursor.car().unwrap();
            cursor = cursor.cdr().unwrap();
            let dynamic_nodes: Vector = cursor.car().unwrap();
            cursor = cursor.cdr().unwrap();
            let dynamic_attrs: Vector = cursor.car().unwrap();
            let template = TEMPLATE_REGISTRY.with(|registry| {
                let registry = registry.borrow();
                registry[template_id]
            });
            dioxus_core::Element::Ok(dioxus_core::VNode::new(
                key,
                template,
                build_dynamic_nodes(dynamic_nodes).into_boxed_slice(),
                build_dynamic_attrs(dynamic_attrs).into_boxed_slice(),
            ))
        })
    })
}
