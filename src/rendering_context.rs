use crate::events::TuiEventManager;
use crate::globals::ROOT_COMPONENT;
use crate::managed_global_ref::ManagedGlobalRef;
use crate::mutation_writer::{DioxusState, MutationWriter};
use crate::text_measurement::measure_text_block;
use crate::wrapper_components::RootComponent;

use dioxus_core::VirtualDom;
use emacs::Value;
use similar::{ChangeTag, TextDiff};
use std::borrow::Cow;
use std::collections::HashMap;
use std::usize;
use taffy::{
    TraversePartialTree,
    prelude::{Dimension, FromLength, Layout, NodeId, Size, Style, TaffyMaxContent, TaffyTree},
};

#[derive(Debug)]
pub struct TextLeafContext {
    pub text: Cow<'static, str>,
}

// span
#[derive(Debug)]
pub struct TextNodeContext {
    pub face: Option<ManagedGlobalRef>,
}

// div
#[derive(Debug)]
pub struct BoxNodeContext {
    pub face: Option<ManagedGlobalRef>,
}

#[derive(Debug)]
pub struct ErrorMessageContext {}

#[derive(Debug)]
pub struct TextTreeRootContext {
    pub container: NodeId,
}

// p
#[derive(Debug)]
pub struct TextBoxLeafContext {
    pub text_tree_root: NodeId,
    pub face: Option<ManagedGlobalRef>,
    pub cache_text_block: String,
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

fn _edit_instruction(old_text: &str, new_text: &str) -> Vec<(usize, usize, String)> {
    let mut cursor = 0usize;
    let mut deletion_start = 0usize;
    let mut inserting = Vec::<&str>::new();
    let mut instructions = Vec::<(usize, usize, String)>::new();
    let diff = TextDiff::from_lines(old_text, new_text);
    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Delete => {
                cursor += change.value().chars().count();
            }
            ChangeTag::Insert => {
                inserting.push(change.value());
            }
            ChangeTag::Equal => {
                if deletion_start < cursor {
                    let replacement = inserting.join("");
                    let inserting_length = replacement.chars().count();
                    instructions.push((deletion_start, cursor, replacement));
                    inserting.clear();
                    cursor = deletion_start + inserting_length;
                } else if !inserting.is_empty() {
                    let replacement = inserting.join("");
                    let inserting_length = replacement.chars().count();
                    instructions.push((cursor, cursor, replacement));
                    inserting.clear();
                    cursor += inserting_length;
                }
                cursor += change.value().chars().count();
                deletion_start = cursor;
            }
        };
    }
    if deletion_start < cursor {
        let replacement = inserting.join("");
        instructions.push((deletion_start, cursor, replacement));
    } else if !inserting.is_empty() {
        let replacement = inserting.join("");
        instructions.push((cursor, cursor, replacement));
    }
    instructions
}

struct TaffyTreeIterator<'a, T> {
    taffy: &'a TaffyTree<T>,
    stack: Vec<<TaffyTree as TraversePartialTree>::ChildIter<'a>>,
}

impl<'a, T> TaffyTreeIterator<'a, T> {
    fn new(taffy: &'a TaffyTree<T>, root: NodeId) -> Self {
        Self {
            taffy,
            stack: vec![taffy.child_ids(root)],
        }
    }
}

impl<'a, T> Iterator for TaffyTreeIterator<'a, T> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last_iter = self.stack.last_mut()?;
            if let Some(next_node) = last_iter.next() {
                self.stack.push(self.taffy.child_ids(next_node));
                return Some(next_node);
            }
            self.stack.pop();
        }
    }
}

pub fn collect_text_blocks(doc: &mut TaffyTree<TuiNodeContext>, root_id: NodeId) {
    let mut textbox_line = HashMap::<NodeId, String>::new();
    for node in TaffyTreeIterator::new(doc, root_id) {
        if let Some(TuiNodeContext::TextBox(ctx)) = doc.get_node_context(node) {
            if !doc.dirty(node).unwrap() {
                continue;
            };
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

    for (k, v) in textbox_line {
        match doc.get_node_context_mut(k) {
            Some(TuiNodeContext::TextBox(ctx)) => {
                ctx.cache_text_block = v;
            }
            _ => {
                panic!("should not reach");
            }
        }
    }
}

pub struct Canvas {
    width: usize,
    height: usize,
    buffer: Vec<Vec<char>>,
    faces: Vec<(usize, usize, ManagedGlobalRef)>,
}

impl Canvas {
    /// Create a new canvas filled with spaces
    pub fn new(width: usize, height: usize) -> Self {
        Canvas {
            width,
            height,
            buffer: vec![vec![' '; width]; height],
            faces: Vec::new(),
        }
    }

    pub fn draw(
        &mut self,
        doc: &TaffyTree<TuiNodeContext>,
        parent_x: usize,
        parent_y: usize,
        id: NodeId,
    ) {
        let layout = doc.layout(id).unwrap();
        let x = parent_x + layout.location.x as usize;
        let y = parent_y + layout.location.y as usize;
        let maybe_ctx = doc.get_node_context(id);
        if let Some(ctx) = maybe_ctx {
            let face_ref = match ctx {
                TuiNodeContext::Box(ctx) => ctx.face.clone(),
                TuiNodeContext::TextBox(ctx) => ctx.face.clone(),
                _ => None,
            };
            if let Some(face_ref) = face_ref {
                let width = layout.size.width as usize;
                let height = layout.size.height as usize;
                let mut i_y = y;
                while i_y < height + y {
                    self.faces.push((
                        self.to_position(x, i_y),
                        self.to_position(x + width, i_y),
                        face_ref.clone(),
                    ));
                    i_y += 1;
                }
            };
        }
        self.draw_border(parent_x, parent_y, layout);
        if let Some(TuiNodeContext::TextBox(ctx)) = maybe_ctx {
            let text_content = &ctx.cache_text_block;
            let width: usize = layout.content_box_width() as usize;
            let wrapped_text = textwrap::wrap(text_content, width);
            let content_x = parent_x + layout.content_box_x() as usize;
            let content_y = parent_y + layout.content_box_y() as usize;
            self.draw_text(content_x, content_y, &wrapped_text);
            self.draw_text_face(
                doc,
                ctx.text_tree_root,
                &wrapped_text,
                parent_x + layout.content_box_x() as usize,
                parent_y + layout.content_box_y() as usize,
                &mut 0,
                &mut 0,
                &mut Vec::<ManagedGlobalRef>::new(),
            )
        } else {
            for child_id in doc.children(id).unwrap() {
                self.draw(doc, x, y, child_id);
            }
        }
    }

    /// Draw a multi-line string at the specified position
    pub fn draw_text(&mut self, x: usize, y: usize, lines: &Vec<Cow<'_, str>>) {
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
    #[inline]
    pub fn to_position(&self, x: usize, y: usize) -> usize {
        // + 1 for position starts from 1
        // self.width + 1 for trailling line break
        x + y * (self.width + 1) + 1
    }

    pub fn draw_text_face(
        &mut self,
        doc: &TaffyTree<TuiNodeContext>,
        node_id: NodeId,
        wrapped_text: &Vec<Cow<'_, str>>,
        x: usize,
        y: usize,
        cursor_x: &mut usize,
        cursor_y: &mut usize,
        face_stack: &mut Vec<ManagedGlobalRef>,
    ) {
        if let Some(ctx) = doc.get_node_context(node_id) {
            match ctx {
                TuiNodeContext::TextLeaf(ctx) => {
                    let mut begin = *cursor_x;
                    let mut maybe_trailling_whitespaces = 0;

                    for char in ctx.text.chars() {
                        // skip heading whitespace
                        if *cursor_x == 0 && char == ' ' {
                            continue;
                        }
                        if char == ' ' {
                            maybe_trailling_whitespaces += 1;
                        } else if char != '\n' {
                            maybe_trailling_whitespaces = 0;
                        }

                        if char == '\n' {
                            *cursor_x -= maybe_trailling_whitespaces;
                            if *cursor_x > 0 {
                                let absolute_y = y + *cursor_y;
                                for face in face_stack.iter() {
                                    self.faces.push((
                                        self.to_position(x + begin, absolute_y),
                                        self.to_position(x + *cursor_x, absolute_y),
                                        face.clone(),
                                    ));
                                }
                            }
                            *cursor_x = 0;
                            *cursor_y += 1;
                            maybe_trailling_whitespaces = 0;
                            begin = 0;
                        } else {
                            *cursor_x += 1;
                            if *cursor_x - maybe_trailling_whitespaces
                                > wrapped_text[*cursor_y].chars().count()
                            {
                                let absolute_y = y + *cursor_y;
                                for face in face_stack.iter() {
                                    self.faces.push((
                                        self.to_position(x + begin, absolute_y),
                                        self.to_position(x + *cursor_x, absolute_y),
                                        face.clone(),
                                    ));
                                }
                                *cursor_x = 0;
                                *cursor_y += 1;
                                maybe_trailling_whitespaces = 0;
                                begin = 0;
                            }
                        };
                    }
                }
                TuiNodeContext::Text(ctx) => {
                    let maybe_face = ctx.face.clone();
                    let has_face = maybe_face.is_some();
                    let begin_x = *cursor_x;
                    let begin_y = *cursor_y;
                    if let Some(face) = maybe_face {
                        face_stack.push(face);
                    }

                    for child_id in doc.children(node_id).unwrap() {
                        self.draw_text_face(
                            doc,
                            child_id,
                            wrapped_text,
                            x,
                            y,
                            cursor_x,
                            cursor_y,
                            face_stack,
                        );
                    }

                    if has_face {
                        let face = face_stack.pop().unwrap();
                        self.faces.push((
                            self.to_position(
                                if *cursor_y == begin_y { x + begin_x } else { x },
                                y + *cursor_y,
                            ),
                            self.to_position(x + *cursor_x, y + *cursor_y),
                            face,
                        ));
                    }
                }
                _ => {
                    for child_id in doc.children(node_id).unwrap() {
                        self.draw_text_face(
                            doc,
                            child_id,
                            wrapped_text,
                            x,
                            y,
                            cursor_x,
                            cursor_y,
                            face_stack,
                        );
                    }
                }
            }
        }
    }

    pub fn draw_border(&mut self, parent_x: usize, parent_y: usize, layout: &Layout) {
        let content_left = layout.content_box_x() as usize + parent_x;
        let content_top = layout.content_box_y() as usize + parent_y;
        let content_right = content_left + (layout.content_box_width() as usize); //.saturating_sub(1);
        let content_bottom = content_top + (layout.content_box_height() as usize); //.saturating_sub(1);
        let content_left = content_left - layout.padding.left as usize;
        let content_top = content_top - layout.padding.top as usize;
        let content_right = content_right + layout.padding.right as usize;
        let content_bottom = content_bottom + layout.padding.bottom as usize;
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
    should_redraw: bool,
    // result_cache: String,
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
        let mut should_redraw = false;
        let mut event_manager = TuiEventManager::new();
        // let mutation_writter =
        ROOT_COMPONENT.set(&root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut doc,
                // root_id,
                state: &mut dioxus_state,
                event_manager: &mut event_manager,
                should_redraw: &mut should_redraw,
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
            should_redraw,
            // result_cache: String::new(),
            // size: Size::MAX_CONTENT,
        }
    }

    pub fn set_width(&mut self, width: usize) {
        // self.size.width = AvailableSpace::from_length(width);
        self.should_redraw = true;
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

    pub fn render(
        &mut self,
    ) -> Option<(
        // Vec<(usize, usize, String)>,
        String,
        Vec<(usize, usize, ManagedGlobalRef)>,
    )> {
        ROOT_COMPONENT.set(&self.root_component_ref, || {
            let mut mutation_writer = MutationWriter {
                doc: &mut self.doc,
                // root_id: self.root_id,
                state: &mut self.dioxus_state,
                event_manager: &mut self.event_manager,
                should_redraw: &mut self.should_redraw,
            };
            self.vdom.render_immediate(&mut mutation_writer);
            mutation_writer.should_redraw
        });

        if !self.should_redraw {
            return None;
        }

        collect_text_blocks(&mut self.doc, self.root_id);

        self.doc
            .compute_layout_with_measure(
                self.root_id,
                Size::MAX_CONTENT,
                |known_dimensions, available_space, _node_id, node_context, _style| {
                    if let Some(TuiNodeContext::TextBox(ctx)) =  node_context {
                        #[cfg(feature = "tracing")]
                        tracing::debug!(
                            "[measure_text_block] known_dimensions:{:?} available_space:{:?} text_block:{:?}",
                            known_dimensions,
                            available_space,
                            ctx.cache_text_block
                        );
                        let res = measure_text_block(known_dimensions, available_space, &ctx.cache_text_block);
                        #[cfg(feature = "tracing")]
                        tracing::debug!(
                            "[measure_text_block] result:{:?}",
                            res
                        );
                        res
                    }
                    else {
                        Size::ZERO
                    }
                },
            )
            .unwrap();

        let layout = self.doc.layout(self.root_id).unwrap();
        let mut canvas = Canvas::new(layout.size.width as usize, layout.size.height as usize);

        canvas.draw(&self.doc, 0, 0, self.root_id);
        let result = canvas.to_string();
        // let instructions = edit_instruction(&self.result_cache, &result);
        // self.result_cache = result;
        self.should_redraw = false;
        return Some((result, canvas.faces));
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
