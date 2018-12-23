use crate::math::*;

/// A view to center the view around a point while seeing a given area.
#[derive(Debug, Clone, Copy)]
pub struct AreaView {
    pub center: Vector2<f32>,
    pub area: f32,
}

impl AreaView {
    pub fn to_matrix(&self, aspect_ratio: f32) -> Matrix3<f32> {
        let size_x = self.width(aspect_ratio);
        let size_y = self.height(aspect_ratio);
        let scale_x = 2.0 / size_x;
        let scale_y = 2.0 / size_y;
        let trans_x = -(self.center[0] * scale_x);
        let trans_y = -(self.center[1] * scale_y);

        #[rustfmt::skip]
      Matrix3::<f32>::new(
      scale_x, 0.0,      trans_x,
      0.0,     -scale_y, trans_y,
      0.0,     0.0,      1.0
    )
    }

    pub fn width(&self, aspect_ratio: f32) -> f32 {
        (aspect_ratio * self.area).sqrt()
    }

    pub fn height(&self, aspect_ratio: f32) -> f32 {
        (self.area / aspect_ratio).sqrt()
    }
}
