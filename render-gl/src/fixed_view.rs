use crate::math::*;

/// A fixed view -- coordinates start from top left between the size specified
pub struct FixedView {
  pub size: Vector2<f32>,
}

impl FixedView {
  #[rustfmt::skip]
  pub fn to_matrix(&self, aspect_ratio: f32) -> Matrix3<f32> {
    let target_aspect_ratio = self.size[0] / self.size[1];

    if target_aspect_ratio > aspect_ratio {
      // black bars top and bottom
      let actual_width = self.size[0];
      let actual_height = self.size[0] / aspect_ratio;
      let scale_x = 2.0 / actual_width;
      let scale_y = 2.0 / actual_height;

      Matrix3::<f32>::new(
        scale_x,   0.0,     -((actual_width / 2.0) * scale_x),
        0.0,      -scale_y,  (self.size[1] / 2.0) * scale_y,
        0.0,       0.0,      1.0,
      )
    } else {
      // black bars left and right
      let actual_width = self.size[1] * aspect_ratio;
      let actual_height = self.size[1];
      let scale_x = 2.0 / actual_width;
      let scale_y = 2.0 / actual_height;

      Matrix3::<f32>::new(
        scale_x,  0.0,     -((self.size[0] / 2.0) * scale_x),
        0.0,     -scale_y, -((actual_height / 2.0) * scale_y) + 2.0,
        0.0,      0.0,      1.0,
      )
    }
  }
}
