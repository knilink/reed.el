use crate::events::TuiEventManager;
use crate::globals::ROOT_COMPONENT;
use crate::managed_global_ref::ManagedGlobalRef;
use crate::mutation_writer::{DioxusState, MutationWriter};
use crate::wrapper_components::RootComponent;

use dioxus_core::VirtualDom;
use emacs::Value;
use std::borrow::Cow;
use std::collections::HashMap;
use std::usize;
use taffy::prelude::{
    Dimension, FromLength, Layout, NodeId, Size, Style, TaffyMaxContent, TaffyTree,
};

#[derive(Debug)]
pub struct TextLeafContext {
    pub text: Cow<'static, str>,
}

#[derive(Debug)]
pub struct TextNodeContext {}

#[derive(Debug)]
pub struct BoxNodeContext {}

#[derive(Debug)]
pub struct ErrorMessageContext {}

#[derive(Debug)]
pub struct TextTreeRootContext {
    pub container: NodeId,
}

#[derive(Debug)]
pub struct TextBoxLeafContext {
    pub text_tree_root: NodeId,
}

#[derive(Debug)]
pub enum TuiNodeContext {
    Box(BoxNodeContext),
    TextBox(TextBoxLeafContext),
    TextTreeRoot(TextTreeRootContext),
    Text(TextNodeContext),
    TextLeaf(TextLeafContext),
    ErrorMessage(ErrorMessageContext),
}

pub fn measure_text_block(
    _known_dimensions: taffy::geometry::Size<Option<f32>>,
    available_space: taffy::geometry::Size<taffy::style::AvailableSpace>,
    content: &str,
) -> taffy::geometry::Size<f32> {
    // Handle width calculation

    match available_space.width {
        taffy::style::AvailableSpace::Definite(available_width) => Size {
            width: available_width,
            height: textwrap::wrap(content, available_width as usize).len() as f32,
        },
        taffy::style::AvailableSpace::MinContent => Size {
            width: 1.0,
            height: content.split('\n').map(|l| l.len()).sum::<usize>() as f32,
        },
        taffy::style::AvailableSpace::MaxContent => {
            let lines = content.split('\n');
            let mut height = 0usize;
            let width = lines
                .map(|l| {
                    height += 1;
                    l.len()
                })
                .max()
                .unwrap_or(0);
            Size {
                width: width as f32,
                height: height as f32,
            }
        }
    }
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
    pub fn draw_text(&mut self, x: usize, y: usize, lines: &[&str]) {
        for (dy, line) in lines.into_iter().enumerate() {
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

    pub fn draw_border(&mut self, parent_x: usize, parent_y: usize, layout: &Layout) {
        let content_left = layout.content_box_x() as usize + parent_x;
        let content_top = layout.content_box_y() as usize + parent_y;
        let content_right = content_left + (layout.content_box_width() as usize); //.saturating_sub(1);
        let content_bottom = content_top + (layout.content_box_height() as usize); //.saturating_sub(1);
        let left = layout.location.x as usize + parent_x;
        let top = layout.location.y as usize + parent_y;
        let right = left + (layout.size.width as usize); //.saturating_sub(1);
        let bottom = top + (layout.size.height as usize); //.saturating_sub(1);
        let mut chars = "│─│─┌┐┘└".chars();
        // let mut chars = "████████".chars();
        // let mut chars = "|=|=/\\/\\".chars();
        // let mut chars = "".chars();

        let left_width = content_left - left;
        if let Some(c) = chars.next() {
            for i in 0..left_width {
                let ii = left_width - i - 1;
                for j in
                    std::cmp::max(content_top - ii, top)..std::cmp::min(content_bottom + ii, bottom)
                {
                    self.buffer[j][left + i] = c;
                }
            }
        }

        let top_width = content_top - top;
        if let Some(c) = chars.next() {
            for i in 0..top_width {
                let ii = top_width - i - 1;
                for j in
                    std::cmp::max(content_left - ii, left)..std::cmp::min(content_right + ii, right)
                {
                    self.buffer[top + i][j] = c;
                }
            }
        }

        let right_width = right - content_right;
        if let Some(c) = chars.next() {
            for i in 0..right_width {
                let ii = right_width - i - 1;
                for j in
                    std::cmp::max(content_top - ii, top)..std::cmp::min(content_bottom + ii, bottom)
                {
                    self.buffer[j][right - 1 - i] = c;
                }
            }
        }

        let bottom_width = bottom - content_bottom;
        if let Some(c) = chars.next() {
            for i in 0..bottom_width {
                let ii = bottom_width - i - 1;
                for j in
                    std::cmp::max(content_left - ii, left)..std::cmp::min(content_right + ii, right)
                {
                    self.buffer[bottom - 1 - i][j] = c;
                }
            }
        }

        if let Some(c) = chars.next() {
            for i in 0..std::cmp::min(left_width, top_width) {
                self.buffer[content_top - 1 - i][content_left - 1 - i] = c;
                // self.buffer[top + i][left + i] = c;
            }
        }

        if let Some(c) = chars.next() {
            for i in 0..std::cmp::min(right_width, top_width) {
                self.buffer[content_top - 1 - i][content_right + i] = c;
                // self.buffer[top + i][right - i - 1] = '┐';
            }
        }

        if let Some(c) = chars.next() {
            for i in 0..std::cmp::min(right_width, bottom_width) {
                self.buffer[content_bottom + i][content_right + i] = c;
                // self.buffer[bottom - i - 1][right - i - 1] = '┘';
            }
        }

        if let Some(c) = chars.next() {
            for i in 0..std::cmp::min(left_width, bottom_width) {
                self.buffer[content_bottom + i][content_left - i - 1] = c;
                // self.buffer[bottom - i - 1][left + i] = '└';
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
    event_manager: TuiEventManager,
    window_width: usize,
    // size: Size<AvailableSpace>,
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
        let mut event_manager = TuiEventManager::new();
        // let mutation_writter =
        ROOT_COMPONENT.set(&root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut doc,
                // root_id,
                state: &mut dioxus_state,
                event_manager: &mut event_manager,
            };
            vdom.rebuild(&mut mutation_writer);
        });
        Self {
            root_component_ref,
            vdom,
            doc,
            root_id,
            dioxus_state,
            event_manager,
            window_width: 80,
            // size: Size::MAX_CONTENT,
        }
    }

    pub fn set_width(&mut self, width: usize) {
        // self.size.width = AvailableSpace::from_length(width);
        self.window_width = width;
        let width = width as f32;
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

    pub fn get_width(&self) -> usize {
        self.window_width
    }

    pub fn render(&mut self) -> String {
        ROOT_COMPONENT.set(&self.root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut self.doc,
                // root_id: self.root_id,
                state: &mut self.dioxus_state,
                event_manager: &mut self.event_manager,
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
        let mut canvas = Canvas::new(layout.size.width as usize, layout.size.height as usize);

        for (&key, value) in &text_blocks {
            let layout = self.doc.layout(key).unwrap();
            let width = layout.content_box_width() as usize;
            let lines = textwrap::wrap(value, width);
            let mut parent_x = 0;
            let mut parent_y = 0;
            let mut current_ancestor = key;
            while let Some(next_ancestor) = self.doc.parent(current_ancestor) {
                current_ancestor = next_ancestor;
                let layout = self.doc.layout(current_ancestor).unwrap();
                parent_x += layout.location.x as usize;
                parent_y += layout.location.y as usize;
            }
            canvas.draw_text(
                parent_x + (layout.content_box_x() as usize),
                parent_y + (layout.content_box_y()) as usize,
                &lines.iter().map(|l| l.as_ref()).collect::<Vec<_>>(),
            );
        }

        let iterator = TaffyTreeIterator::new(&self.doc, self.root_id);
        for node in iterator {
            if let Ok(layout) = self.doc.layout(node) {
                let mut parent_x = 0;
                let mut parent_y = 0;
                let mut current_ancestor = node;
                while let Some(next_ancestor) = self.doc.parent(current_ancestor) {
                    current_ancestor = next_ancestor;
                    let layout = self.doc.layout(current_ancestor).unwrap();
                    parent_x += layout.location.x as usize;
                    parent_y += layout.location.y as usize;
                }
                canvas.draw_border(parent_x, parent_y, layout);
            }
        }

        canvas.to_string()
    }

    pub fn handle_cursor_event(
        &mut self,
        event_name: String,
        cursor_pos: usize,
        event_payload: ManagedGlobalRef,
    ) {
        let cursor_pos = cursor_pos - 1;
        self.event_manager.handle_cursor_event(
            event_name,
            self.vdom.runtime(),
            &self.doc,
            &self.dioxus_state.node_id_mapping,
            (
                (cursor_pos % (self.window_width + 1)),
                (cursor_pos / (self.window_width + 1)),
            ),
            event_payload,
        );
    }

    pub fn get_serialized_layout(&self, element_id: usize) -> Option<String> {
        if let Some(node_id) = self.dioxus_state.node_id_mapping[element_id] {
            Some(serde_lexpr::to_string(self.doc.layout(node_id).unwrap()).unwrap())
        } else {
            None
        }
    }

    pub fn get_absolute_location(&self, element_id: usize) -> Option<(usize, usize)> {
        if let Some(node_id) = self.dioxus_state.node_id_mapping[element_id] {
            let layout = self.doc.layout(node_id).unwrap();
            let mut x = layout.location.x as usize;
            let mut y = layout.location.y as usize;
            let mut current_node_id = node_id;
            while let Some(parent_node_id) = self.doc.parent(current_node_id) {
                let layout = self.doc.layout(parent_node_id).unwrap();
                x += layout.location.x as usize;
                y += layout.location.y as usize;
                current_node_id = parent_node_id;
            }
            Some((x, y))
        } else {
            None
        }
    }

    pub fn emit_event(
        &self,
        event_name: String,
        element_id: dioxus_core::ElementId,
        event_payload: ManagedGlobalRef,
        propagates: bool,
    ) {
        self.vdom.runtime().handle_event(
            &event_name,
            dioxus_core::Event::new(std::rc::Rc::new(event_payload), propagates),
            element_id,
        );
    }
}
