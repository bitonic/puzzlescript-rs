use crate::math::*;
use std::ops::{Add, AddAssign};
use winit::dpi::LogicalSize;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LayoutOrigin {
  TopLeft,
  TopRight,
  BottomLeft,
  BottomRight,
  Center,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct LayoutPosition {
  pub origin: LayoutOrigin,
  pub position: Vector2<f64>,
}

impl LayoutPosition {
  pub fn new(origin: LayoutOrigin, x: f64, y: f64) -> LayoutPosition {
    LayoutPosition {
      origin,
      position: vec2(x, y),
    }
  }

  pub fn to_clipping_coordinates(&self, window_size: &LogicalSize) -> Vector2<f64> {
    let scale = layout_scale(window_size);

    let scaled_x = self.position[0] * scale[0];
    let scaled_y = self.position[1] * scale[1];

    let (x, y) = match self.origin {
      LayoutOrigin::TopLeft => (scaled_x - 1.0, 1.0 - scaled_y),
      LayoutOrigin::TopRight => (1.0 - scaled_x, 1.0 - scaled_y),
      LayoutOrigin::BottomLeft => (scaled_x - 1.0, scaled_y - 1.0),
      LayoutOrigin::BottomRight => (1.0 - scaled_x, scaled_y - 1.0),
      LayoutOrigin::Center => (scaled_x, scaled_y),
    };

    vec2(x, y)
  }

  pub fn from_clipping_coordinates(
    window_size: &LogicalSize,
    coords: &Vector2<f64>,
    origin: LayoutOrigin,
  ) -> LayoutPosition {
    let scale = layout_scale(window_size);

    let (x, y) = (coords[0], coords[1]);
    let (scaled_x, scaled_y) = match origin {
      LayoutOrigin::TopLeft => (1.0 + x, 1.0 - y),
      LayoutOrigin::TopRight => (1.0 - x, 1.0 - y),
      LayoutOrigin::BottomLeft => (1.0 + x, 1.0 + y),
      LayoutOrigin::BottomRight => (1.0 - x, 1.0 + y),
      LayoutOrigin::Center => (x, y),
    };

    LayoutPosition {
      position: vec2(scaled_x / scale[0], scaled_y / scale[1]),
      origin,
    }
  }

  pub fn modify_origin(
    &self,
    window_size: &LogicalSize,
    new_origin: LayoutOrigin,
  ) -> LayoutPosition {
    LayoutPosition::from_clipping_coordinates(
      window_size,
      &self.to_clipping_coordinates(window_size),
      new_origin,
    )
  }
}

impl Add<Vector2<f64>> for LayoutPosition {
  type Output = LayoutPosition;

  fn add(self, rhs: Vector2<f64>) -> LayoutPosition {
    LayoutPosition {
      origin: self.origin,
      position: self.position + rhs,
    }
  }
}

impl AddAssign<Vector2<f64>> for LayoutPosition {
  fn add_assign(&mut self, other: Vector2<f64>) {
    self.position = self.position + other;
  }
}

pub fn layout_scale(window_size: &LogicalSize) -> Vector2<f64> {
  vec2(2.0 / window_size.width, 2.0 / window_size.height)
}

#[cfg(test)]
mod tests {
  use super::*;
  use winit::dpi::LogicalSize;

  const SAME_SIZE: LogicalSize = LogicalSize {
    width: 2.0,
    height: 2.0,
  };
  const DOUBLE_WIDTH: LogicalSize = LogicalSize {
    width: 4.0,
    height: 2.0,
  };
  const DOUBLE_HEIGHT: LogicalSize = LogicalSize {
    width: 2.0,
    height: 4.0,
  };

  fn assert_roundtrip(
    window_size: &LogicalSize,
    layout_pos: &LayoutPosition,
    coords: &Vector2<f64>,
  ) {
    assert_eq!(&layout_pos.to_clipping_coordinates(window_size), coords);
    assert_eq!(
      layout_pos,
      &LayoutPosition::from_clipping_coordinates(
        window_size,
        &layout_pos.to_clipping_coordinates(window_size),
        layout_pos.origin
      )
    );
  }

  fn tests(cases: &[(LogicalSize, LayoutPosition, Vector2<f64>)]) {
    for case in cases.iter() {
      assert_roundtrip(&case.0, &case.1, &case.2);
    }
  }

  #[test]
  fn top_left() {
    fn pos(x: f64, y: f64) -> LayoutPosition {
      LayoutPosition::new(LayoutOrigin::TopLeft, x, y)
    }

    tests(&[
      (SAME_SIZE, pos(0.0, 0.0), vec2(-1.0, 1.0)),
      (DOUBLE_WIDTH, pos(0.0, 0.0), vec2(-1.0, 1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 0.0), vec2(-1.0, 1.0)),
      (SAME_SIZE, pos(0.0, 2.0), vec2(-1.0, -1.0)),
      (DOUBLE_WIDTH, pos(0.0, 2.0), vec2(-1.0, -1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 2.0), vec2(-1.0, 0.0)),
      (SAME_SIZE, pos(2.0, 0.0), vec2(1.0, 1.0)),
      (DOUBLE_WIDTH, pos(2.0, 0.0), vec2(0.0, 1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 0.0), vec2(1.0, 1.0)),
      (SAME_SIZE, pos(2.0, 2.0), vec2(1.0, -1.0)),
      (DOUBLE_WIDTH, pos(2.0, 2.0), vec2(0.0, -1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 2.0), vec2(1.0, 0.0)),
      (SAME_SIZE, pos(1.0, 1.0), vec2(0.0, 0.0)),
      (DOUBLE_WIDTH, pos(1.0, 1.0), vec2(-0.5, 0.0)),
      (DOUBLE_HEIGHT, pos(1.0, 1.0), vec2(0.0, 0.5)),
    ]);
  }

  #[test]
  fn top_right() {
    fn pos(x: f64, y: f64) -> LayoutPosition {
      LayoutPosition::new(LayoutOrigin::TopRight, x, y)
    }

    tests(&[
      (SAME_SIZE, pos(0.0, 0.0), vec2(1.0, 1.0)),
      (DOUBLE_WIDTH, pos(0.0, 0.0), vec2(1.0, 1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 0.0), vec2(1.0, 1.0)),
      (SAME_SIZE, pos(0.0, 2.0), vec2(1.0, -1.0)),
      (DOUBLE_WIDTH, pos(0.0, 2.0), vec2(1.0, -1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 2.0), vec2(1.0, 0.0)),
      (SAME_SIZE, pos(2.0, 0.0), vec2(-1.0, 1.0)),
      (DOUBLE_WIDTH, pos(2.0, 0.0), vec2(0.0, 1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 0.0), vec2(-1.0, 1.0)),
      (SAME_SIZE, pos(2.0, 2.0), vec2(-1.0, -1.0)),
      (DOUBLE_WIDTH, pos(2.0, 2.0), vec2(0.0, -1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 2.0), vec2(-1.0, 0.0)),
      (SAME_SIZE, pos(1.0, 1.0), vec2(0.0, 0.0)),
      (DOUBLE_WIDTH, pos(1.0, 1.0), vec2(0.5, 0.0)),
      (DOUBLE_HEIGHT, pos(1.0, 1.0), vec2(0.0, 0.5)),
    ]);
  }

  #[test]
  fn bottom_left() {
    fn pos(x: f64, y: f64) -> LayoutPosition {
      LayoutPosition::new(LayoutOrigin::BottomLeft, x, y)
    }

    tests(&[
      (SAME_SIZE, pos(0.0, 0.0), vec2(-1.0, -1.0)),
      (DOUBLE_WIDTH, pos(0.0, 0.0), vec2(-1.0, -1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 0.0), vec2(-1.0, -1.0)),
      (SAME_SIZE, pos(0.0, 2.0), vec2(-1.0, 1.0)),
      (DOUBLE_WIDTH, pos(0.0, 2.0), vec2(-1.0, 1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 2.0), vec2(-1.0, 0.0)),
      (SAME_SIZE, pos(2.0, 0.0), vec2(1.0, -1.0)),
      (DOUBLE_WIDTH, pos(2.0, 0.0), vec2(0.0, -1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 0.0), vec2(1.0, -1.0)),
      (SAME_SIZE, pos(2.0, 2.0), vec2(1.0, 1.0)),
      (DOUBLE_WIDTH, pos(2.0, 2.0), vec2(0.0, 1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 2.0), vec2(1.0, 0.0)),
      (SAME_SIZE, pos(1.0, 1.0), vec2(0.0, 0.0)),
      (DOUBLE_WIDTH, pos(1.0, 1.0), vec2(-0.5, 0.0)),
      (DOUBLE_HEIGHT, pos(1.0, 1.0), vec2(0.0, -0.5)),
    ]);
  }

  #[test]
  fn bottom_right() {
    fn pos(x: f64, y: f64) -> LayoutPosition {
      LayoutPosition::new(LayoutOrigin::BottomRight, x, y)
    }

    tests(&[
      (SAME_SIZE, pos(0.0, 0.0), vec2(1.0, -1.0)),
      (DOUBLE_WIDTH, pos(0.0, 0.0), vec2(1.0, -1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 0.0), vec2(1.0, -1.0)),
      (SAME_SIZE, pos(0.0, 2.0), vec2(1.0, 1.0)),
      (DOUBLE_WIDTH, pos(0.0, 2.0), vec2(1.0, 1.0)),
      (DOUBLE_HEIGHT, pos(0.0, 2.0), vec2(1.0, 0.0)),
      (SAME_SIZE, pos(2.0, 0.0), vec2(-1.0, -1.0)),
      (DOUBLE_WIDTH, pos(2.0, 0.0), vec2(0.0, -1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 0.0), vec2(-1.0, -1.0)),
      (SAME_SIZE, pos(2.0, 2.0), vec2(-1.0, 1.0)),
      (DOUBLE_WIDTH, pos(2.0, 2.0), vec2(0.0, 1.0)),
      (DOUBLE_HEIGHT, pos(2.0, 2.0), vec2(-1.0, 0.0)),
      (SAME_SIZE, pos(1.0, 1.0), vec2(0.0, 0.0)),
      (DOUBLE_WIDTH, pos(1.0, 1.0), vec2(0.5, 0.0)),
      (DOUBLE_HEIGHT, pos(1.0, 1.0), vec2(0.0, -0.5)),
    ]);
  }
}
