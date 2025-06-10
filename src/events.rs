use crate::managed_global_ref::ManagedGlobalRef;
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

fn is_point_in_node<T>(
    doc: &TaffyTree<T>,
    root_id: NodeId,
    node_id: NodeId,
    x: usize,
    y: usize,
) -> bool {
    if let Ok(layout) = doc.layout(node_id) {
        let mut box_x = layout.location.x as usize;
        let mut box_y = layout.location.y as usize;
        let box_w = layout.size.width as usize;
        let box_h = layout.size.height as usize;
        let mut current_ancestor = node_id;
        while let Some(next_ancestor) = doc.parent(current_ancestor) {
            current_ancestor = next_ancestor;
            let layout = doc.layout(current_ancestor).unwrap();
            box_x += layout.location.x as usize;
            box_y += layout.location.y as usize;
        }
        // not mounted if root_id != current_ancestor
        root_id == current_ancestor
            && x >= box_x
            && x < box_x + box_w
            && y >= box_y
            && y < box_y + box_h
    } else {
        false
    }
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

    pub fn handle_cursor_event<T>(
        &mut self,
        event_name: String,
        runtime: Rc<Runtime>,
        doc: &TaffyTree<T>,
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
