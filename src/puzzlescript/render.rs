use crate::math::*;
use crate::puzzlescript::colors::*;
use crate::puzzlescript::game::*;
use crate::render::*;
use failure::Error;
use gleam::gl;
use glutin::dpi;
use std::collections::HashMap;
use std::rc::Rc;

/// 1x1, each of those fits in a cell
pub struct ObjectSprite<'gl> {
    geometry: Geometry<'gl>,
}

#[derive(Clone)]
pub struct ObjectShader<'gl> {
    shader: Rc<Shader<'gl>>,
}

impl<'gl> ObjectShader<'gl> {
    pub fn new(gl: &'gl gl::Gl) -> Result<ObjectShader<'gl>, Error> {
        Ok(ObjectShader {
            shader: Rc::new(Shader::new(
                gl,
                include_str!("../../shaders/colored.vert"),
                include_str!("../../shaders/colored.frag"),
            )?),
        })
    }
}

impl<'gl> ObjectSprite<'gl> {
    pub fn new(
        gl: &'gl gl::Gl,
        shader: ObjectShader<'gl>,
        palette: ColorPalette,
        object: &Object,
    ) -> Result<ObjectSprite<'gl>, Error> {
        match object {
            Object::Empty(pzl_color) => {
                let color = to_float_color(palette, pzl_color);
                let geometry = Geometry::new(
                    gl,
                    shader.shader,
                    #[rustfmt::skip]
          &GeometrySpec::Colored(&[
            ColoredVertex { pos: vec2(0.0, 0.0), color },
            ColoredVertex { pos: vec2(0.0, 1.0), color },
            ColoredVertex { pos: vec2(1.0, 0.0), color },
            ColoredVertex { pos: vec2(1.0, 1.0), color },
          ]),
                    &[Triangle(0, 1, 2), Triangle(1, 2, 3)],
                )?;
                Ok(ObjectSprite { geometry })
            }
            Object::Normal(squares) => {
                let mut vertices = Vec::new();
                let mut triangles = Vec::new();

                for row in 0..5 {
                    #[rustfmt::skip]
          for col in 0..5 {
            let color = to_float_color(palette, &squares[(row, col)]);
            let x = col as f32 * 0.2;
            let y = row as f32 * 0.2;

            let current_vertex = vertices.len();

            vertices.push(ColoredVertex { pos: vec2(x,       y      ), color });
            vertices.push(ColoredVertex { pos: vec2(x,       y + 0.2), color });
            vertices.push(ColoredVertex { pos: vec2(x + 0.2, y      ), color });
            vertices.push(ColoredVertex { pos: vec2(x + 0.2, y + 0.2), color });

            triangles.push(Triangle(current_vertex,   current_vertex+1, current_vertex+2));
            triangles.push(Triangle(current_vertex+1, current_vertex+2, current_vertex+3));
          }
                }

                Ok(ObjectSprite {
                    geometry: Geometry::new(
                        gl,
                        shader.shader,
                        &GeometrySpec::Colored(&vertices),
                        &triangles,
                    )?,
                })
            }
        }
    }

    pub fn draw(&self, aspect_ratio: f32, view: &FixedView, pos: &Vector2<usize>) {
        #[rustfmt::skip]
    let model = Matrix3::new(
      1.0, 0.0, pos[0] as f32,
      0.0, 1.0, pos[1] as f32,
      0.0, 0.0, 1.0,
    );
        self.geometry.draw(&model, &view.to_matrix(aspect_ratio))
    }
}

pub fn draw_level<'gl>(
    window_size: &dpi::LogicalSize,
    color_palette: ColorPalette,
    foreground_color: &Color,
    font: &mut Face<'gl>,
    object_sprites: &HashMap<ObjectName, ObjectSprite<'gl>>,
    collision_layers: &[CollisionLayer],
    level: &Level,
) -> Result<(), Error> {
    let aspect_ratio = window_size.width as f32 / window_size.height as f32;

    match level {
        Level::Message(msg) => font.draw(
            window_size,
            msg,
            &to_float_color(color_palette, foreground_color)
                .fixed_rows::<U3>(0)
                .clone_owned(),
            &LayoutPosition::new(LayoutOrigin::TopLeft, 40.0, 60.0),
            window_size.width - 80.0,
        ),

        Level::Stage { stage, background } => {
            let view = FixedView {
                size: vec2(stage.ncols() as f32, stage.nrows() as f32),
            };

            for row in 0..stage.nrows() {
                for col in 0..stage.ncols() {
                    let pos = vec2(col, row);
                    let cell = &stage[(row, col)];
                    // do not draw anything beneath the background layer
                    let mut before_background = true;
                    for collision_layer in collision_layers {
                        match collision_layer {
                            CollisionLayer::Background => {
                                match background {
                                    None => (), // the main loop clears the color
                                    Some(bgr) => {
                                        object_sprites[bgr].draw(aspect_ratio, &view, &pos)
                                    }
                                }
                                before_background = false;
                            }
                            CollisionLayer::Normal(objects) => {
                                if !before_background {
                                    for object in objects {
                                        if cell.contains_key(object) {
                                            object_sprites[object].draw(aspect_ratio, &view, &pos);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            Ok(())
        }
    }
}
