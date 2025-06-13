use crate::managed_global_ref::ManagedGlobalRef;
use crate::rendering_context::{TuiNodeContext, get_absolut_location};
use dioxus_core::{ElementId, Runtime};
use std::collections::BTreeSet;
use std::rc::Rc;
use taffy::prelude::{NodeId, TaffyTree};

pub struct TuiEventManager {
    hover: BTreeSet<ElementId>,
    leave: BTreeSet<ElementId>,
    focus: BTreeSet<ElementId>,
    blur: BTreeSet<ElementId>,
    click: BTreeSet<ElementId>,
    // change: BTreeSet<ElementId>,
    hovering: BTreeSet<ElementId>,
    focusing: BTreeSet<ElementId>,
}

// fn position_in_subarea(
//     position_main: usize, // position in main area
//     width_main: usize,    // width of main area
//     x_sub: usize,         // x offset of subarea
//     y_sub: usize,         // y offset of subarea
//     width_sub: usize,     // width of subarea
// ) -> Option<usize> {
//     // Convert position n to (x, y) coordinates in main area
//     let x = position_main % width_main;
//     let y = position_main / width_main;
//
//     // Convert to relative coordinates within subarea
//     let relative_x = x.checked_sub(x_sub)?;
//     let relative_y = y.checked_sub(y_sub)?;
//
//     // Convert back to position within subarea
//     Some(relative_y * width_sub + relative_x)
// }

fn get_text_container_id(doc: &TaffyTree<TuiNodeContext>, node_id: NodeId) -> Option<NodeId> {
    let mut current_id = node_id;
    while let Some(parent_id) = doc.parent(current_id) {
        if let Some(TuiNodeContext::TextTreeRoot(root_ctx)) = doc.get_node_context(parent_id) {
            return Some(root_ctx.container);
        }
        current_id = parent_id;
    }
    return None;
}

fn is_point_in_node(
    doc: &TaffyTree<TuiNodeContext>,
    root_id: NodeId,
    node_id: NodeId,
    x: usize,
    y: usize,
) -> bool {
    if let Some(TuiNodeContext::Text(ctx)) = doc.get_node_context(node_id) {
        if let Some(text_container_id) = get_text_container_id(doc, node_id) {
            if let Ok(layout) = doc.layout(text_container_id) {
                if let Some((global_box_x, global_box_y)) =
                    get_absolut_location(doc, root_id, text_container_id)
                {
                    let box_x = layout.location.x as usize;
                    let box_y = layout.location.y as usize;
                    let content_x = layout.content_box_x() as usize;
                    let content_y = layout.content_box_y() as usize;
                    let content_w = layout.content_box_width() as usize;
                    let content_h = layout.content_box_height() as usize;
                    let global_content_x = global_box_x + content_x - box_x;
                    let global_content_y = global_box_y + content_y - box_y;
                    if x >= global_content_x
                        && x < global_content_x + content_w
                        && y >= global_content_y
                        && y < global_content_y + content_h
                    {
                        let local_position =
                            (x - global_content_x) + (y - global_content_y) * content_w;
                        for (begin, end) in &ctx.target_ranges {
                            if local_position >= *begin && local_position < *end {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    } else {
        if let Ok(layout) = doc.layout(node_id) {
            let box_w = layout.size.width as usize;
            let box_h = layout.size.height as usize;
            if let Some((box_x, box_y)) = get_absolut_location(doc, root_id, node_id) {
                return x >= box_x && x < box_x + box_w && y >= box_y && y < box_y + box_h;
            }
        }
    }
    return false;
}

fn create_event(
    event_payload: ManagedGlobalRef,
    propagates: bool,
) -> dioxus_core::Event<dyn std::any::Any> {
    dioxus_core::Event::new(std::rc::Rc::new(event_payload), propagates)
}

impl TuiEventManager {
    pub fn new() -> Self {
        Self {
            hover: BTreeSet::new(),
            leave: BTreeSet::new(),
            focus: BTreeSet::new(),
            blur: BTreeSet::new(),
            // change: BTreeSet::new(),
            hovering: BTreeSet::new(),
            click: BTreeSet::new(),
            focusing: BTreeSet::new(),
        }
    }

    pub fn remove_element(&mut self, id: ElementId) {
        self.hover.remove(&id);
        self.leave.remove(&id);
        self.focus.remove(&id);
        self.blur.remove(&id);
        self.click.remove(&id);
        self.hovering.remove(&id);
        self.focusing.remove(&id);
    }

    pub fn create_event_listener(&mut self, name: &'static str, id: ElementId) {
        match name {
            "hover" => {
                self.hover.insert(id);
            }
            "leave" => {
                self.leave.insert(id);
            }
            "focus" => {
                self.focus.insert(id);
            }
            "blur" => {
                self.blur.insert(id);
            }
            // "change" => {
            //     self.change.insert(id);
            // }
            "click" => {
                self.click.insert(id);
            }
            _ => panic!("Unsupported event name: {}", name),
        }
    }

    pub fn remove_event_listener(&mut self, name: &'static str, id: ElementId) {
        match name {
            "hover" => {
                self.hover.remove(&id);
            }
            "leave" => {
                self.leave.remove(&id);
            }
            "focus" => {
                self.focus.remove(&id);
            }
            "blur" => {
                self.blur.remove(&id);
            }
            // "change" => {
            //     self.change.remove(&id);
            // }
            "click" => {
                self.click.remove(&id);
            }
            _ => panic!("Unsupported event name: {}", name),
        }
    }

    pub fn handle_cursor_event(
        &mut self,
        event_name: String,
        runtime: Rc<Runtime>,
        doc: &TaffyTree<TuiNodeContext>,
        node_id_mapping: &Vec<Option<NodeId>>,
        cursor_pos: (usize, usize),
        event_payload: ManagedGlobalRef,
    ) {
        let (x, y) = cursor_pos;
        let root_id = node_id_mapping[0].unwrap();
        // Check all elements with cursor_move listeners
        match event_name.as_ref() {
            "move" => {
                for element_id in BTreeSet::union(&self.hover, &self.leave) {
                    if let Some(node_id) = node_id_mapping[element_id.0] {
                        if is_point_in_node(doc, root_id, node_id, x, y) {
                            if self.hovering.insert(*element_id) {
                                runtime.handle_event(
                                    "hover",
                                    create_event(event_payload.clone(), false),
                                    *element_id,
                                );
                            }
                        } else {
                            if self.hovering.remove(element_id) {
                                runtime.handle_event(
                                    "leave",
                                    create_event(event_payload.clone(), false),
                                    *element_id,
                                );
                            }
                        }
                    }
                }
            }
            "click" => {
                for element_id in BTreeSet::union(&self.focus, &self.blur) {
                    if let Some(node_id) = node_id_mapping[element_id.0] {
                        if is_point_in_node(doc, root_id, node_id, x, y) {
                            if self.focusing.insert(*element_id) {
                                runtime.handle_event(
                                    "focus",
                                    create_event(event_payload.clone(), false),
                                    *element_id,
                                );
                            }
                        } else {
                            if self.focusing.remove(element_id) {
                                runtime.handle_event(
                                    "blur",
                                    create_event(event_payload.clone(), false),
                                    *element_id,
                                );
                            }
                        }
                    }
                }
                for element_id in self.click.iter().rev() {
                    if let Some(node_id) = node_id_mapping[element_id.0] {
                        if is_point_in_node(doc, root_id, node_id, x, y) {
                            runtime.handle_event(
                                "click",
                                create_event(event_payload.clone(), true),
                                *element_id,
                            );
                            break;
                        }
                    }
                }
            }
            _ => {
                panic!("unknown event {}", event_name);
            }
        }
    }
}
