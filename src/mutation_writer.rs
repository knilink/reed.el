use crate::rendering_context::{
    BoxNodeContext, TextBoxLeafContext, TextLeafContext, TextNodeContext, TextTreeRootContext,
    TuiNodeContext,
};
use dioxus_core::{AttributeValue, ElementId, Template, TemplateNode};
use std::borrow::Cow;
use std::collections::HashSet;
use taffy::prelude::{NodeId, TaffyTree};
use taffy::{Dimension, Size, Style, TaffyResult, style_helpers::FromLength};

pub struct DioxusState {
    /// Store of templates keyed by unique name
    // templates: FxHashMap<Template, Vec<NodeId>>,
    /// Stack machine state for applying dioxus mutations
    pub stack: Vec<NodeId>,
    /// Mapping from vdom ElementId -> rdom NodeId
    pub node_id_mapping: Vec<Option<NodeId>>,
}

impl DioxusState {
    /// Convert an ElementId to a NodeId
    pub fn element_to_node_id(&self, element_id: ElementId) -> NodeId {
        self.try_element_to_node_id(element_id).unwrap()
    }

    /// Attempt to convert an ElementId to a NodeId. This will return None if the ElementId is not in the RealDom.
    pub fn try_element_to_node_id(&self, element_id: ElementId) -> Option<NodeId> {
        self.node_id_mapping.get(element_id.0).copied().flatten()
    }
}

pub struct MutationWriter<'a> {
    pub doc: &'a mut TaffyTree<TuiNodeContext>,
    // pub root_id: NodeId,
    pub state: &'a mut DioxusState,
    pub event_listeners: &'a mut HashSet<(&'static str, ElementId)>,
}

impl MutationWriter<'_> {
    fn set_id_mapping(&mut self, node_id: NodeId, element_id: ElementId) {
        let element_id: usize = element_id.0;

        // Ensure node_id_mapping is large enough to contain element_id
        if self.state.node_id_mapping.len() <= element_id {
            self.state.node_id_mapping.resize(element_id + 1, None);
        }

        // Set the new mapping
        self.state.node_id_mapping[element_id] = Some(node_id);
    }

    fn load_child(&self, path: &[u8]) -> NodeId {
        let mut current_id = *self.state.stack.last().unwrap();

        for &index in path {
            let index: usize = index.into();
            if let Some(TuiNodeContext::TextBox(ctx)) = self.doc.get_node_context(current_id) {
                current_id = ctx.text_tree_root
            };
            current_id = self.doc.child_at_index(current_id, index).unwrap();
        }
        current_id
    }

    fn remove_node(&mut self, id: NodeId) -> TaffyResult<NodeId> {
        if let Some(TuiNodeContext::TextBox(ctx)) = self.doc.get_node_context(id) {
            self.doc.remove(ctx.text_tree_root)
        } else {
            self.doc.remove(id)
        }
    }

    fn mark_text_box_dirty(&mut self, text_leaf: NodeId) {
        let mut current_id = text_leaf;
        while let Some(parent_id) = self.doc.parent(current_id) {
            current_id = parent_id;
            if let Some(TuiNodeContext::TextTreeRoot(ctx)) = self.doc.get_node_context(current_id) {
                self.doc.mark_dirty(ctx.container).unwrap();
                return;
            };
        }
    }
}

impl dioxus_core::WriteMutations for MutationWriter<'_> {
    fn append_children(&mut self, id: ElementId, m: usize) {
        #[cfg(feature = "tracing")]
        tracing::info!("[append_children] id:{} m:{}", id.0, m);

        let children = self.state.stack.split_off(self.state.stack.len() - m);
        let parent = self.state.element_to_node_id(id);

        add_childrens(self.doc, parent, &children);
    }

    fn assign_node_id(&mut self, path: &'static [u8], id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[assign_node_id] path:{:?} id:{}", path, id.0);

        // If there is an existing node already mapped to that ID and
        // it has no parent, then drop it
        if let Some(node_id) = self.state.try_element_to_node_id(id) {
            if let None = self.doc.parent(node_id) {
                self.remove_node(node_id).unwrap();
            }
        }

        let node_id = self.load_child(path);
        self.set_id_mapping(node_id, id);
    }

    fn create_placeholder(&mut self, id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[create_placeholder] id:{}", id.0);

        // TODO: no context
        let node_id = self.doc.new_leaf(Default::default()).unwrap();
        self.set_id_mapping(node_id, id);
        self.state.stack.push(node_id);
    }

    fn create_text_node(&mut self, value: &str, id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[create_text_node] id:{} text:{}", id.0, value);

        let node_id = self
            .doc
            .new_leaf_with_context(
                Default::default(),
                TuiNodeContext::TextLeaf(TextLeafContext {
                    text: Cow::Owned(value.to_string()),
                }),
            )
            .unwrap();
        self.set_id_mapping(node_id, id);
        self.state.stack.push(node_id);
    }

    fn load_template(&mut self, template: Template, index: usize, id: ElementId) {
        let clone_id = create_template_node(self.doc, &template.roots[index]);
        self.set_id_mapping(clone_id, id);

        self.state.stack.push(clone_id);
    }

    fn replace_node_with(&mut self, id: ElementId, m: usize) {
        #[cfg(feature = "tracing")]
        tracing::info!("[replace_node_with] id:{} m:{}", id.0, m);

        let new_nodes = self.state.stack.split_off(self.state.stack.len() - m);
        let anchor_node_id = self.state.element_to_node_id(id);
        insert_before(self.doc, anchor_node_id, &new_nodes);
        self.remove_node(anchor_node_id).unwrap();
    }

    fn replace_placeholder_with_nodes(&mut self, path: &'static [u8], m: usize) {
        #[cfg(feature = "tracing")]
        tracing::info!("[replace_placeholder_with_nodes] path:{:?} m:{}", path, m);

        let new_nodes = self.state.stack.split_off(self.state.stack.len() - m);
        let anchor_node_id = self.load_child(path);
        insert_before(self.doc, anchor_node_id, &new_nodes);
        self.remove_node(anchor_node_id).unwrap();
    }

    fn insert_nodes_after(&mut self, id: ElementId, m: usize) {
        #[cfg(feature = "tracing")]
        tracing::info!("[insert_nodes_after] id:{} m:{}", id.0, m);

        let new_nodes = self.state.stack.split_off(self.state.stack.len() - m);
        let anchor_node_id = self.state.element_to_node_id(id);
        insert_after(self.doc, anchor_node_id, &new_nodes);
    }

    fn insert_nodes_before(&mut self, id: ElementId, m: usize) {
        #[cfg(feature = "tracing")]
        tracing::info!("[insert_nodes_before] id:{} m:{}", id.0, m);

        let new_nodes = self.state.stack.split_off(self.state.stack.len() - m);
        let anchor_node_id = self.state.element_to_node_id(id);
        insert_before(self.doc, anchor_node_id, &new_nodes);
    }

    fn set_attribute(
        &mut self,
        name: &'static str,
        ns: Option<&'static str>,
        value: &AttributeValue,
        id: ElementId,
    ) {
        let node_id = self.state.element_to_node_id(id);

        #[cfg(feature = "tracing")]
        tracing::info!(
            "[set_attribute] node_id:{:?} ns: {:?} name:{}, value:{:?}",
            node_id,
            ns,
            name,
            value
        );

        let style = if let Some(ctx) = self.doc.get_node_context(node_id) {
            match ctx {
                TuiNodeContext::Box(_) => Some(self.doc.style(node_id)),
                TuiNodeContext::TextBox(_) => Some(self.doc.style(node_id)),
                _ => None,
            }
        } else {
            None
        };

        if let Some(Ok(style)) = style {
            let new_style: Option<Style> = match name {
                "width" => match value {
                    AttributeValue::Float(value) => {
                        let style = style.clone();
                        Some(Style {
                            size: Size {
                                width: Dimension::from_length((*value) as f32),
                                ..style.size
                            },
                            ..style
                        })
                    }
                    _ => None,
                },
                "height" => match value {
                    AttributeValue::Float(value) => {
                        let style = style.clone();
                        Some(Style {
                            size: Size {
                                height: Dimension::from_length((*value) as f32),
                                ..style.size
                            },
                            ..style
                        })
                    }
                    _ => None,
                },
                _ => None,
            };
            if let Some(new_style) = new_style {
                self.doc.set_style(node_id, new_style).unwrap();
            }
        };
    }

    fn set_node_text(&mut self, value: &str, id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[set_node_text] id:{} value:{}", id.0, value);

        let node_id = self.state.element_to_node_id(id);

        if let Some(TuiNodeContext::TextLeaf(ctx)) = self.doc.get_node_context_mut(node_id) {
            ctx.text = Cow::Owned(value.to_string());
        };
        self.mark_text_box_dirty(node_id);
    }

    fn create_event_listener(&mut self, name: &'static str, id: ElementId) {
        self.event_listeners.insert((name, id));
    }

    fn remove_event_listener(&mut self, name: &'static str, id: ElementId) {
        self.event_listeners.remove(&(name, id));
    }

    fn remove_node(&mut self, id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[remove_node] id:{}", id.0);

        let node_id = self.state.element_to_node_id(id);
        self.remove_node(node_id).unwrap();
    }

    fn push_root(&mut self, id: ElementId) {
        #[cfg(feature = "tracing")]
        tracing::info!("[push_root] id:{}", id.0,);

        let node_id = self.state.element_to_node_id(id);
        self.state.stack.push(node_id);
    }
}

#[inline(always)]
fn insert_before(
    doc: &mut TaffyTree<TuiNodeContext>,
    anchor_node_id: NodeId,
    new_nodes: &[NodeId],
) {
    insert_nodes(doc, anchor_node_id, new_nodes, false);
}

#[inline(always)]
fn insert_after(doc: &mut TaffyTree<TuiNodeContext>, anchor_node_id: NodeId, new_nodes: &[NodeId]) {
    insert_nodes(doc, anchor_node_id, new_nodes, true);
}

fn insert_nodes(
    doc: &mut TaffyTree<TuiNodeContext>,
    anchor_node_id: NodeId,
    new_nodes: &[NodeId],
    after: bool,
) {
    if let Some(parent) = doc.parent(anchor_node_id) {
        let children = doc.children(parent).unwrap();
        let index = children
            .iter()
            .position(|&n| n == anchor_node_id)
            .expect("Anchor node not found in parent's children");
        let index = if after { index + 1 } else { index };

        for node in new_nodes.iter().rev() {
            doc.insert_child_at_index(parent, index, *node)
                .expect("Failed to insert child");
        }
    }
}

fn add_childrens(doc: &mut TaffyTree<TuiNodeContext>, parent: NodeId, children: &[NodeId]) {
    let parent = if let Some(TuiNodeContext::TextBox(ctx)) = doc.get_node_context(parent) {
        ctx.text_tree_root
    } else {
        parent
    };

    for &child in children {
        doc.add_child(parent, child).unwrap();
    }
}

fn create_template_node(doc: &mut TaffyTree<TuiNodeContext>, node: &TemplateNode) -> NodeId {
    match node {
        TemplateNode::Element {
            tag,
            namespace: _,
            attrs,
            children,
        } => {
            let mut style: Style = Default::default();

            for attr in attrs.iter() {
                if let dioxus_core::TemplateAttribute::Static {
                    name,
                    value,
                    namespace: _,
                } = attr
                {
                    match *name {
                        "width" => {
                            let value: f32 = str::parse(value).unwrap();
                            style = Style {
                                size: Size {
                                    width: Dimension::from_length(value),
                                    ..style.size
                                },
                                ..style
                            }
                        }
                        "height" => {
                            let value: f32 = str::parse(value).unwrap();
                            style = Style {
                                size: Size {
                                    height: Dimension::from_length(value),
                                    ..style.size
                                },
                                ..style
                            }
                        }
                        _ => {}
                    }
                }
            }

            let new_context = match *tag {
                // box
                "div" => TuiNodeContext::Box(BoxNodeContext {}),
                // textbox
                "p" => TuiNodeContext::TextBox(TextBoxLeafContext {
                    text_tree_root: doc.new_leaf(Default::default()).unwrap(),
                }),
                // text
                "span" => TuiNodeContext::Text(TextNodeContext {}),
                _ => panic!("unknown tag: {:?}", tag),
            };

            let id = doc.new_leaf_with_context(style, new_context).unwrap();

            if let Some(TuiNodeContext::TextBox(ctx)) = doc.get_node_context(id) {
                doc.set_node_context(
                    ctx.text_tree_root,
                    Some(TuiNodeContext::TextTreeRoot(TextTreeRootContext {
                        container: id,
                    })),
                )
                .unwrap();
            }

            let child_ids: Vec<NodeId> = children
                .iter()
                .map(|child| create_template_node(doc, child))
                .collect();
            add_childrens(doc, id, &child_ids);
            id
        }
        TemplateNode::Text { text } => doc
            .new_leaf_with_context(
                Default::default(),
                TuiNodeContext::TextLeaf(TextLeafContext {
                    text: Cow::Borrowed(text),
                }),
            )
            .unwrap(),
        TemplateNode::Dynamic { .. } => doc.new_leaf(Default::default()).unwrap(),
    }
}
