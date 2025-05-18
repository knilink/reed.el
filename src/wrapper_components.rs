use crate::globals::{
    CURRENT_EMACS_ENV, ROOT_COMPONENT, SIGNAL_TABLES, SignalTable, TEMPLATE_REGISTRY,
};
use crate::managed_global_ref::ManagedGlobalRef;
use crate::utils::{intern, plist_get};
use dioxus_core::{Element, IntoDynNode, fc_to_builder};
use dioxus_core_macro::{Props, component};
use emacs::{Value, Vector};

fn build_element_attrs(attrs: Vector) -> &'static [dioxus_core::TemplateAttribute] {
    Box::leak(
        attrs
            .into_iter()
            .map(|attr| {
                let env = attr.env;
                let attr_type: Value = attr.car().unwrap();
                let attr_list: Value = attr.cdr().unwrap();


                if attr_type.eq(env.intern("template-attribute:static").unwrap()) {
                    let attr_name: String = plist_get(attr_list, ":name");
                    let attr_value: String = plist_get(attr_list, ":value");
                    dioxus_core::TemplateAttribute::Static {
                        name: intern(&attr_name),
                        // None for now
                        namespace: None,
                        value: Box::leak(attr_value.into_boxed_str()),
                    }
                } else if attr_type.eq(env.intern("template-attribute:dynamic").unwrap()) {
                    let id: usize = plist_get(attr_list, ":id");
                    dioxus_core::TemplateAttribute::Dynamic { id }
                } else {
                    panic!(
                        "Unknown attribute type: {:?}. Expected either 'template-attribute:static' or 'template-attribute:dynamic'",
                        attr_type
                    );
                }
            })
            .collect(),
    )
}

fn build_element(vnode: Value) -> Element {
    let mut cur = vnode;
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
        None,
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
            if node_type.eq(node_type.env.intern("dynamic-node:component").unwrap()) {
                // return dioxus_core::DynamicNode::Component(
                //     fc_to_builder(TestComponent)
                //         .dp(TestDropProp::new())
                //         .build()
                //         .into_vcomponent(TestComponent),
                // );
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
            } else if node_type.eq(node_type.env.intern("dynamic-node:element").unwrap()) {
                let element: Value = node.cdr::<Value>().unwrap().car().unwrap();
                if let Ok(ele) = element.into_rust::<String>() {
                    ele.into_dyn_node()
                } else if let Ok(ele) = element.into_rust::<i64>() {
                    ele.to_string().into_dyn_node()
                } else {
                    // dioxus_elements::events::onclick::call_with_explicit_closure
                    build_element(element).into_dyn_node()
                }
            } else {
                panic!("fucked up");
            }
        })
        .collect()
}

fn build_dynamic_attrs(attrs: Vector) -> Vec<Box<[dioxus_core::Attribute]>> {
    attrs
        .into_iter()
        .map(|attr| {
            let tag: String = plist_get(attr, ":tag");
            let name: String = plist_get(attr, ":name");
            let value: String = plist_get(attr, ":value");
            let volatile = tag == "input" && value == "value";
            [dioxus_core::Attribute::new(
                intern(&name),
                value,
                None,
                volatile,
            )]
            .to_vec()
            .into_boxed_slice()
        })
        .collect()
}

#[component]
pub fn WrapperComponent(component_ref: ManagedGlobalRef, props_ref: ManagedGlobalRef) -> Element {
    let scope_id = dioxus_core::prelude::use_hook_with_cleanup(
        || {
            let scope_id = dioxus_core::prelude::current_scope_id().unwrap();
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.insert(scope_id, SignalTable::new());
            });
            scope_id
        },
        |scope_id| {
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.remove(&scope_id);
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
        let element = env
            .call(component_ref.bind(env), [props_ref.bind(env)])
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
            scope_id
        },
        |scope_id| {
            SIGNAL_TABLES.with(|signal_tabels| {
                let mut signal_tabels = signal_tabels.borrow_mut();
                signal_tabels.remove(&scope_id);
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
            let element = root_component_ref.as_ref().call(env, []).unwrap();
            let mut cursor = element;
            let template_id: usize = element.car().unwrap();
            cursor = cursor.cdr().unwrap();
            let dynamic_nodes: Vector = cursor.car().unwrap();
            cursor = cursor.cdr().unwrap();
            let dynamic_attrs: Vector = cursor.car().unwrap();
            let template = TEMPLATE_REGISTRY.with(|registry| {
                let registry = registry.borrow();
                registry[template_id]
            });
            dioxus_core::Element::Ok(dioxus_core::VNode::new(
                None,
                template,
                build_dynamic_nodes(dynamic_nodes).into_boxed_slice(),
                build_dynamic_attrs(dynamic_attrs).into_boxed_slice(),
            ))
        })
    })
}
