use eframe::egui;
use std::collections::HashMap;

fn main() -> Result<(), eframe::Error> {
    let options = Default::default();
    eframe::run_native(
        "Working Draggable Nodes",
        options,
        Box::new(|cc| Box::new(App::new(cc))),
    )
}

struct App {
    nodes: HashMap<egui::Id, Node>,
    selected_nodes: Vec<egui::Id>,
    primary_dragged_node: Option<egui::Id>,
    selection_rect: Option<SelectionRect>,
}

struct SelectionRect {
    start: egui::Pos2,
    end: egui::Pos2,
}

struct Node {
    position: egui::Pos2,
    title: String,
    selected: bool,
}

impl App {
    fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let ctx = &cc.egui_ctx;
        ctx.set_fonts(egui::FontDefinitions::default());
        let mut nodes = HashMap::new();

        // Create a few test nodes
        for i in 0..5 {
            let id = egui::Id::new(format!("node_{}", i));
            let position =
                egui::Pos2::new(100.0 + i as f32 * 150.0, 100.0 + (i % 2) as f32 * 100.0);
            let title = format!("Node {}", i);
            nodes.insert(
                id,
                Node {
                    position,
                    title,
                    selected: false,
                },
            );
        }

        Self {
            nodes,
            selected_nodes: Vec::new(),
            primary_dragged_node: None,
            selection_rect: None,
        }
    }

    fn draw_node(&mut self, ctx: &egui::Context, id: egui::Id) {
        // Get node data
        let node = match self.nodes.get(&id) {
            Some(n) => n,
            None => return,
        };

        let is_selected = node.selected;

        // Prepare window frame
        let mut frame = egui::Frame::window(&ctx.style());
        if is_selected {
            frame.stroke = ctx.style().visuals.selection.stroke;
        }

        // Draw the node window
        let position = node.position; // Get position immutably

        egui::Window::new(&node.title)
            .id(id)
            .frame(frame)
            .fixed_pos(position)
            .movable(true)
            .title_bar(true)
            .resizable(false)
            .show(ctx, |ui| {
                ui.set_min_size(egui::Vec2::new(120.0, 80.0));
                ui.label(format!("ID: {}", id.short_debug_format()));
                ui.label(format!("Position: {:?}", position));
                ui.label(format!("Selected: {}", is_selected));
            })
            .map(|response| {
                let window_response = response.response;
                let ctrl_down = ctx.input(|i| i.modifiers.ctrl);

                // Handle clicks for selection
                if window_response.clicked() {
                    if ctrl_down {
                        // Toggle selection with Ctrl
                        let node_data = self.nodes.get_mut(&id).unwrap();
                        node_data.selected = !node_data.selected;

                        if node_data.selected {
                            self.selected_nodes.push(id);
                        } else {
                            self.selected_nodes.retain(|&n| n != id);
                        }
                    } else {
                        // Select single node without Ctrl
                        for (node_id, node) in self.nodes.iter_mut() {
                            node.selected = *node_id == id;
                        }
                        self.selected_nodes.clear();
                        self.selected_nodes.push(id);
                    }
                }

                // Handle dragging
                if window_response.dragged() {
                    let drag_delta = window_response.drag_delta();

                    // If this node is selected, move all selected nodes
                    if is_selected {
                        // Move all selected nodes
                        for &selected_id in &self.selected_nodes {
                            if let Some(node) = self.nodes.get_mut(&selected_id) {
                                node.position += drag_delta;
                            }
                        }
                    } else {
                        // If not selected, just move this node
                        let node_data = self.nodes.get_mut(&id).unwrap();
                        node_data.position += drag_delta;
                    }

                    if self.primary_dragged_node.is_none() && is_selected {
                        self.primary_dragged_node = Some(id);
                    }
                }

                // Handle drag stop
                if window_response.drag_stopped() {
                    if self.primary_dragged_node == Some(id) {
                        self.primary_dragged_node = None;
                    }
                }
            });
    }
}

impl eframe::App for App {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show(ctx, |ui| {
            let available_rect = ui.available_rect_before_wrap();

            // Background
            ui.painter().rect_filled(
                available_rect,
                0.0,
                ui.style().visuals.window_fill.linear_multiply(0.8),
            );

            let pointer = ui.input(|i| i.pointer.clone());
            let pointer_pos = pointer.hover_pos().unwrap_or_default();

            // Handle selection rectangle - start selection on background click
            if self.selection_rect.is_none()
                && pointer.any_pressed()
                && !ctx.is_using_pointer()
                && available_rect.contains(pointer_pos)
            {
                // Check if clicked on a node
                let mut clicked_on_node = false;
                for id in self.nodes.keys() {
                    if let Some(response) = ctx.read_response(*id) {
                        if response.contains_pointer() {
                            clicked_on_node = true;
                            break;
                        }
                    }
                }

                if !clicked_on_node {
                    // Start selection rectangle
                    self.selection_rect = Some(SelectionRect {
                        start: pointer_pos,
                        end: pointer_pos,
                    });

                    // Clear selection if not holding Ctrl
                    if !ui.input(|i| i.modifiers.ctrl) {
                        for (_, node) in self.nodes.iter_mut() {
                            node.selected = false;
                        }
                        self.selected_nodes.clear();
                    }
                }
            }

            // Update selection rectangle
            if let Some(ref mut rect) = self.selection_rect {
                if pointer.any_down() {
                    rect.end = pointer_pos;
                } else {
                    // Finalize selection if mouse is released
                    let selection_rect = egui::Rect::from_two_pos(rect.start, rect.end);

                    // Select all nodes in the rectangle
                    for (id, node) in self.nodes.iter_mut() {
                        let node_rect = egui::Rect::from_center_size(
                            node.position,
                            egui::Vec2::new(120.0, 80.0),
                        );

                        if selection_rect.intersects(node_rect) {
                            node.selected = true;
                            if !self.selected_nodes.contains(id) {
                                self.selected_nodes.push(*id);
                            }
                        }
                    }

                    // Clear selection rectangle
                    self.selection_rect = None;
                }
            }

            // Draw selection rectangle if active
            if let Some(rect) = &self.selection_rect {
                let rect_visual = egui::Rect::from_two_pos(rect.start, rect.end);
                ui.painter().rect(
                    rect_visual,
                    0.0,
                    ui.style().visuals.selection.bg_fill,
                    ui.style().visuals.selection.stroke,
                );
            }

            // Draw all nodes
            let node_ids: Vec<egui::Id> = self.nodes.keys().copied().collect();
            for id in node_ids {
                self.draw_node(ctx, id);
            }

            // Draw debug info
            egui::Window::new("Debug Info")
                .fixed_pos(egui::Pos2::new(10.0, 400.0))
                .show(ctx, |ui| {
                    ui.label(format!("Selected nodes: {}", self.selected_nodes.len()));
                    ui.label(format!("Primary dragged: {:?}", self.primary_dragged_node));
                    ui.label(format!(
                        "Selection rect: {:?}",
                        self.selection_rect.is_some()
                    ));

                    ui.separator();
                    for (id, node) in &self.nodes {
                        ui.label(format!(
                            "Node {:?}: pos={:?}, selected={}",
                            id.short_debug_format(),
                            node.position,
                            node.selected
                        ));
                    }
                });
        });
    }
}
