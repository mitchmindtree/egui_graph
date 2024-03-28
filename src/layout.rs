//! Items related to automated layout of nodes.

use crate::Layout;
use egui::Direction;
use layout::{
    adt::dag::NodeHandle,
    core::{base::Orientation, geometry::Point, style::*},
    std_shapes::shapes::*,
    topo::{layout::VisualGraph, placer::Placer},
};
use std::collections::HashMap;

/// The layout of a graph.
pub struct GraphLayout {
    handles: HashMap<egui::Id, NodeHandle>,
    ids: Vec<(NodeHandle, egui::Id)>,
    vg: VisualGraph,
    edge_map: HashMap<(NodeHandle, NodeHandle), usize>,
}

impl GraphLayout {
    /// The layout of the nodes.
    pub fn nodes(&self) -> HashMap<egui::Id, egui::Pos2> {
        // `layout-rs` lays out nodes down and to the right starting from the middle.
        // We find the bounding box and shift the nodes up and to the right in order
        // to centre them under the default `camera` position.
        let mut min = None;
        let mut max = None;
        let mut layout = Layout::new();
        for &(handle, id) in &self.ids {
            let pos = self.vg.pos(handle);
            let with_halo = false;
            let (tl, br) = pos.bbox(with_halo);
            let pos = [tl.x as f32, tl.y as f32].into();
            layout.insert(id, pos);
            let [x, y] = min.get_or_insert([tl.x, tl.y]);
            *x = x.min(tl.x);
            *y = y.min(tl.y);
            let [x, y] = max.get_or_insert([br.x, br.y]);
            *x = x.max(br.x);
            *y = y.max(br.y);
        }
        // if let (Some(min), Some(max)) = (min, max) {
        //     let half_w = ((max[0] - min[0]) / 2.0) as f32;
        //     let half_h = ((max[1] - min[1]) / 2.0) as f32;
        //     for p in layout.values_mut() {
        //         p.x -= half_w;
        //         p.y -= half_h;
        //     }
        // }
        layout
    }

    /// The layout of the edge from node `a` to `b`.
    pub fn edge_path(&self, a: egui::Id, b: egui::Id) -> Vec<(egui::Pos2, egui::Pos2)> {
        let (ha, hb) = match (self.handles.get(&a), self.handles.get(&b)) {
            (Some(&ha), Some(&hb)) => (ha, hb),
            _ => return vec![],
        };
        let edge_ix = match self.edge_map.get(&(ha, hb)) {
            Some(&ix) => ix,
            None => return vec![],
        };
        let (arrow, handles) = self
            .vg
            .iter_edges()
            .nth(edge_ix)
            .expect("no edge for index");
        let elems: Vec<_> = handles
            .iter()
            .map(|&h| self.vg.element(h).clone())
            .collect();
        fn to_egui_pos2(p: Point) -> egui::Pos2 {
            egui::Pos2::new(p.x as f32, p.y as f32)
        }
        let force = 30.0;
        layout::std_shapes::render::generate_curve_for_elements(&elems, arrow, force)
            .into_iter()
            .map(|(a, b)| (to_egui_pos2(a), to_egui_pos2(b)))
            .collect()
    }
}

fn edge_map(vg: &VisualGraph) -> HashMap<(NodeHandle, NodeHandle), usize> {
    vg.iter_edges()
        .enumerate()
        .map(|(i, (_arr, v))| {
            // assert_eq!(v.len(), 2, "expected edge to have 2 nodes");
            ((v[0], v[v.len() - 1]), i)
        })
        .collect()
}

/// Constructs a layout for a directed graph based on given edges and node sizes.
///
/// Returns a `Layout` object representing the positions of nodes in the graph.
pub fn layout(
    nodes: impl IntoIterator<Item = (egui::Id, egui::Vec2)>,
    edges: impl IntoIterator<Item = (egui::Id, egui::Id)>,
    flow: egui::Direction,
) -> GraphLayout {
    let orientation = match flow {
        Direction::LeftToRight | Direction::RightToLeft => Orientation::LeftToRight,
        Direction::TopDown | Direction::BottomUp => Orientation::TopToBottom,
    };
    let mut vg = VisualGraph::new(orientation);

    let mut handles = HashMap::new();
    let mut ids = Vec::new();
    for (id, size) in nodes {
        let shape = ShapeKind::new_box("");
        let style = StyleAttr::simple();
        let size = Point::new(size.x.into(), size.y.into());
        let node = Element::create(shape, style, orientation, size);
        let handle = vg.add_node(node);
        handles.insert(id, handle);
        ids.push((handle, id));
    }

    for (a, b) in edges {
        let edge = Arrow::default();
        let na = handles[&a];
        let nb = handles[&b];
        vg.add_edge(edge, na, nb);
    }

    vg.to_valid_dag();
    vg.split_text_edges();
    let disable_opts = false;
    vg.split_long_edges(disable_opts);
    Placer::new(&mut vg).layout(true);

    let edge_map = edge_map(&vg);

    GraphLayout {
        handles,
        ids,
        vg,
        edge_map,
    }
}
