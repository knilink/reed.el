use crate::globals::TEMPLATE_REGISTRY;
use crate::utils::{intern, plist_get, symbol_name};
use emacs::{Env, Result, Value, Vector};

fn build_element_attrs(attrs: Vector) -> &'static [dioxus_core::TemplateAttribute] {
    Box::leak(
        attrs
            .into_iter()
            .map(|attr| {
                let attr_type: Value = attr.car().unwrap();
                let attr_list: Value = attr.cdr().unwrap();
                let attr_type_name = symbol_name(attr_type);

                match attr_type_name.as_ref() {
                    "template-attribute:static" => {
                        let attr_name: String = plist_get(attr_list, ":name");
                        let attr_value: String = plist_get(attr_list, ":value");
                        dioxus_core::TemplateAttribute::Static {
                            name: intern(&attr_name),
                            // None for now
                            namespace: None,
                            value: Box::leak(attr_value.into_boxed_str()),
                        }
                    }
                    "template-attribute:dynamic" => {
                        let id: usize = plist_get(attr_list, ":id");
                        dioxus_core::TemplateAttribute::Dynamic { id }
                    }
                    _ => {
                        panic!(
                            "Unknown attribute type: {:?}. \
                             Expected either 'template-attribute:static' \
                             or 'template-attribute:dynamic'",
                            attr_type
                        );
                    }
                }
            })
            .collect(),
    )
}

fn build_template_nodes(nodes: Vector) -> &'static [dioxus_core::TemplateNode] {
    Box::leak(
        nodes
            .into_iter()
            .map(|node| {
                let node_type: Value = node.car().unwrap();
                let node_fields: Value = node.cdr().unwrap();
                let node_type_name: String = symbol_name(node_type);
                match node_type_name.as_ref() {
                    "template-note:element" => {
                        let tag: String = plist_get(node_fields, ":tag");
                        let attrs: Vector = plist_get(node_fields, ":attrs");
                        let children: Vector = plist_get(node_fields, ":children");
                        dioxus_core::TemplateNode::Element {
                            tag: intern(&tag),
                            namespace: None,
                            attrs: build_element_attrs(attrs),
                            children: build_template_nodes(children),
                        }
                    }
                    "template-note:dynamic" => {
                        let id: usize = plist_get(node_fields, ":id");
                        dioxus_core::TemplateNode::Dynamic { id }
                    }
                    "template-note:text" => {
                        let text: String = plist_get(node_fields, ":text");
                        dioxus_core::TemplateNode::Text {
                            text: Box::leak(text.into_boxed_str()),
                        }
                    }
                    _ => {
                        panic!(
                            "Unknown node type: {:?}. \
                             Expected one of: 'template-note:element', \
                             'template-note:dynamic', or 'template-note:text'",
                            node_type
                        );
                    }
                }
            })
            .collect(),
    )
}

fn build_paths(paths: Vector) -> &'static [&'static [u8]] {
    Box::leak(
        paths
            .into_iter()
            .map::<&'static [u8], _>(|path| {
                Box::leak(
                    path.into_rust::<Vector>()
                        .unwrap()
                        .into_iter()
                        .map::<u8, _>(|a| a.into_rust::<u8>().unwrap())
                        .collect(),
                )
            })
            .collect(),
    )
}

#[inline(always)]
pub fn register<'e>(
    _: &'e Env,
    template: Vector,
    node_paths: Vector,
    attr_paths: Vector,
) -> Result<usize> {
    let static_template = dioxus_core::Template {
        roots: build_template_nodes(template),
        node_paths: build_paths(node_paths),
        attr_paths: build_paths(attr_paths),
    };
    TEMPLATE_REGISTRY.with(|registry| {
        let mut registry = registry.borrow_mut();
        let id = registry.len();
        registry.push(static_template);
        Ok(id)
    })
}
