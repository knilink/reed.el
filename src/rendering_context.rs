use crate::globals::ROOT_COMPONENT;
use crate::managed_global_ref::ManagedGlobalRef;
use crate::mutation_writer::{DioxusState, MutationWriter};
use crate::wrapper_components::RootComponent;

use dioxus_core::{ElementId, VirtualDom};
use emacs::Value;
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use taffy::prelude::{
    AvailableSpace, Dimension, FromLength, NodeId, Size, Style, TaffyMaxContent, TaffyTree,
};

pub struct TextLeafContext {
    pub text: Cow<'static, str>,
}

pub struct TextNodeContext {}

pub struct BoxNodeContext {}

pub struct ErrorMessageContext {}

pub struct TextTreeRootContext {
    pub container: NodeId,
}

pub struct TextBoxLeafContext {
    pub text_tree_root: NodeId,
}

pub enum TuiNodeContext {
    Box(BoxNodeContext),
    TextBox(TextBoxLeafContext),
    TextTreeRoot(TextTreeRootContext),
    Text(TextNodeContext),
    TextLeaf(TextLeafContext),
    ErrorMessage(ErrorMessageContext),
}

pub fn measure_text_block(
    known_dimensions: taffy::geometry::Size<Option<f32>>,
    available_space: taffy::geometry::Size<taffy::style::AvailableSpace>,
    content: &str,
) -> taffy::geometry::Size<f32> {
    // Handle width calculation
    let width = match known_dimensions.width {
        // If width is explicitly specified, use it
        Some(width) => width,
        // Otherwise, calculate based on available space and line lengths
        None => {
            match available_space.width {
                // If available space is definite, use the smaller of available space or max line length
                taffy::style::AvailableSpace::Definite(available_width) => {
                    let max_line_length =
                        content.lines().map(|l| l.len()).max().unwrap_or(0) as f32;
                    max_line_length.min(available_width)
                }
                // For min-content constraint, use the minimum non-zero line length or zero
                taffy::style::AvailableSpace::MinContent => {
                    content.lines().map(|l| l.len()).max().unwrap_or(0) as f32
                }
                // For max-content constraint, use the maximum line length
                taffy::style::AvailableSpace::MaxContent => {
                    content.lines().map(|l| l.len()).max().unwrap_or(0) as f32
                }
            }
        }
    };

    // Handle height calculation
    let height = match known_dimensions.height {
        // If height is explicitly specified, use it
        Some(height) => height,
        // Otherwise, calculate based on the number of lines
        None => content.lines().count() as f32,
    };

    // Return the calculated size
    taffy::geometry::Size { width, height }
}

struct TaffyTreeIterator<'a, T> {
    taffy: &'a TaffyTree<T>,
    stack: Vec<NodeId>,
}

impl<'a, T> TaffyTreeIterator<'a, T> {
    fn new(taffy: &'a TaffyTree<T>, root: NodeId) -> Self {
        Self {
            taffy,
            stack: vec![root],
        }
    }
}

impl<'a, T> Iterator for TaffyTreeIterator<'a, T> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(node) = self.stack.pop() {
            // Push children in reverse order so they're popped in correct order
            if let Ok(children) = self.taffy.children(node) {
                for &child in children.iter().rev() {
                    self.stack.push(child);
                }
            }
            Some(node)
        } else {
            None
        }
    }
}

pub fn collect_text_blocks(
    doc: &TaffyTree<TuiNodeContext>,
    root_id: NodeId,
) -> HashMap<NodeId, String> {
    let mut textbox_line = HashMap::<NodeId, String>::new();
    let iterator = TaffyTreeIterator::new(doc, root_id);
    for node in iterator {
        if let Some(TuiNodeContext::TextBox(ctx)) = doc.get_node_context(node) {
            let mut canvas = String::new();
            let text_iterator = TaffyTreeIterator::new(doc, ctx.text_tree_root);
            for text_node in text_iterator {
                if let Some(TuiNodeContext::TextLeaf(ctx)) = doc.get_node_context(text_node) {
                    let text: &str = &ctx.text;
                    let mut lines = text.split("\n");
                    if let Some(next_line) = lines.next() {
                        canvas.push_str(next_line);
                        while let Some(next_line) = lines.next() {
                            canvas.push_str("\n");
                            canvas.push_str(next_line);
                        }
                    };
                }
            }
            textbox_line.insert(node, canvas);
        } else if let Some(TuiNodeContext::TextLeaf(ctx)) = doc.get_node_context(node) {
            // This should only happen when parent is error message
            textbox_line.insert(node, ctx.text.to_string());
        }
    }
    textbox_line
}

pub struct Canvas {
    width: usize,
    height: usize,
    buffer: Vec<Vec<char>>,
}

impl Canvas {
    /// Create a new canvas filled with spaces
    pub fn new(width: usize, height: usize) -> Self {
        Canvas {
            width,
            height,
            buffer: vec![vec![' '; width]; height],
        }
    }

    /// Draw a multi-line string at the specified position
    pub fn draw_text(&mut self, x: usize, y: usize, text: &str) {
        for (dy, line) in text.lines().enumerate() {
            let current_y = y + dy;
            if current_y >= self.height {
                break; // Don't draw beyond canvas height
            }

            for (dx, ch) in line.chars().enumerate() {
                let current_x = x + dx;
                if current_x >= self.width {
                    break; // Don't draw beyond canvas width
                }
                self.buffer[current_y][current_x] = ch;
            }
        }
    }

    /// Convert the canvas to a string
    pub fn to_string(&self) -> String {
        self.buffer
            .iter()
            .map(|row| row.iter().collect::<String>())
            .collect::<Vec<String>>()
            .join("\n")
    }
}

pub struct RenderingContext {
    root_component_ref: ManagedGlobalRef,
    vdom: VirtualDom,
    doc: TaffyTree<TuiNodeContext>,
    root_id: NodeId,
    dioxus_state: DioxusState,
    event_listeners: HashSet<(&'static str, ElementId)>,
    size: Size<AvailableSpace>,
}

impl RenderingContext {
    pub fn new(root_component: Value) -> Self {
        let mut vdom = VirtualDom::new(RootComponent);
        let root_component_ref = ManagedGlobalRef::from(root_component);
        let mut doc: TaffyTree<TuiNodeContext> = TaffyTree::new();
        let root_id = doc.new_leaf(Default::default()).unwrap();
        let mut dioxus_state = DioxusState {
            stack: Vec::new(),
            node_id_mapping: [Some(root_id)].to_vec(),
        };
        let mut event_listeners = HashSet::new();
        // let mutation_writter =
        ROOT_COMPONENT.set(&root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut doc,
                // root_id,
                state: &mut dioxus_state,
                event_listeners: &mut event_listeners,
            };
            vdom.rebuild(&mut mutation_writer);
        });
        Self {
            root_component_ref,
            vdom,
            doc,
            root_id,
            dioxus_state,
            event_listeners,
            size: Size::MAX_CONTENT,
        }
    }

    pub fn set_width(&mut self, width: f32) {
        // self.size.width = AvailableSpace::from_length(width);
        let style = self.doc.style(self.root_id).unwrap();
        let _ = self.doc.set_style(
            self.root_id,
            Style {
                size: Size::<Dimension> {
                    width: Dimension::from_length(width),
                    ..style.size
                },
                ..style.clone()
            },
        );
    }

    pub fn render(&mut self) -> String {
        ROOT_COMPONENT.set(&self.root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut self.doc,
                // root_id: self.root_id,
                state: &mut self.dioxus_state,
                event_listeners: &mut self.event_listeners,
            };
            self.vdom.render_immediate(&mut mutation_writer);
        });

        let text_blocks = collect_text_blocks(&self.doc, self.root_id);

        self.doc
            .compute_layout_with_measure(
                self.root_id,
                Size::MAX_CONTENT,
                |known_dimensions, available_space, node_id, _node_context, _style| {
                    if let Some(text_block) = text_blocks.get(&node_id) {
                        #[cfg(feature = "tracing")]
                        tracing::debug!(
                            "[measure_text_block] known_dimensions:{:?} available_space:{:?} text_block:{:?}",
                            known_dimensions,
                            available_space,
                            text_block
                        );
                        let res = measure_text_block(known_dimensions, available_space, &text_block);
                        #[cfg(feature = "tracing")]
                        tracing::debug!(
                            "[measure_text_block] result:{:?}",
                            res
                        );
                        res
                    } else {
                        Size::ZERO
                    }
                },
            )
            .unwrap();

        let layout = self.doc.layout(self.root_id).unwrap();
        let mut canvas = Canvas::new(
            layout.content_box_width() as usize,
            layout.content_box_height() as usize,
        );

        for (&key, value) in &text_blocks {
            let layout = self.doc.layout(key).unwrap();
            canvas.draw_text(
                layout.content_box_x() as usize,
                layout.content_box_y() as usize,
                &value,
            );
        }
        canvas.to_string()
    }
}
