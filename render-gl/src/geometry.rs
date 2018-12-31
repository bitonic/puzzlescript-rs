use crate::math::*;
use failure::Error;
use gleam::gl;
use std::mem::size_of;
use std::rc::Rc;

use crate::shader::*;
use crate::texture::*;

/// stores three indices for the vertices of the triangles in some
#[derive(Debug, Clone, Copy)]
pub struct Triangle(pub usize, pub usize, pub usize);

#[derive(Debug, Clone, Copy)]
pub struct TexturedVertex {
  pub pos: Vector2<f32>,
  pub tex: Vector2<f32>,
}

#[derive(Debug, Clone, Copy)]
pub struct ColoredVertex {
  pub pos: Vector2<f32>,
  pub color: Vector4<f32>,
}

enum GeometrySpecInternal<'gl> {
  #[allow(dead_code)]
  Bare,
  Colored,
  Textured(Rc<Texture<'gl>>),
}

/// Extremely simple Geometry -- just textured vertices
pub struct Geometry<'gl> {
  gl: &'gl gl::Gl,
  spec: GeometrySpecInternal<'gl>,
  shader: Rc<Shader<'gl>>,
  vao: u32,
  vbo: u32,
  ebo: u32,
  num_indices: usize,
  model_location: u32,
  view_location: u32,
}

pub enum GeometrySpec<'gl, 'a> {
  #[allow(dead_code)]
  Bare(&'a [Vector2<f32>]),
  Colored(&'a [ColoredVertex]),
  #[allow(dead_code)]
  Textured(Rc<Texture<'gl>>, &'a [TexturedVertex]),
}

impl<'gl> Geometry<'gl> {
  /// The shader must have:
  ///
  /// * A `mat3` `"model"` model matrix;
  /// * A `mat3` `"view"` view matrix;
  /// * A `"pos"` `vec2` input;
  /// * A `"tex_coord"` input if `GeometrySpec::Textured`.
  /// * A `"color"` input if `GeometrySpec::Colored`.
  #[allow(clippy::new_ret_no_self)]
  pub fn new<'a>(
    gl: &'gl gl::Gl,
    shader: Rc<Shader<'gl>>,
    spec: &GeometrySpec<'gl, 'a>,
    triangles: &[Triangle],
  ) -> Result<Geometry<'gl>, Error> {
    let mut vertices: Vec<f32> = Vec::new();
    match spec {
      GeometrySpec::Bare(vertices_2d) => {
        for pos in vertices_2d.iter() {
          vertices.push(pos[0]);
          vertices.push(pos[1]);
        }
      }
      GeometrySpec::Textured(_texture, vertices_2d) => {
        for TexturedVertex { pos, tex } in vertices_2d.iter() {
          vertices.push(pos[0]);
          vertices.push(pos[1]);
          vertices.push(tex[0]);
          vertices.push(tex[1]);
        }
      }
      GeometrySpec::Colored(vertices_2d) => {
        for ColoredVertex { pos, color } in vertices_2d.iter() {
          vertices.push(pos[0]);
          vertices.push(pos[1]);
          vertices.push(color[0]);
          vertices.push(color[1]);
          vertices.push(color[2]);
          vertices.push(color[3]);
        }
      }
    }

    let mut indices: Vec<u32> = Vec::with_capacity(triangles.len());
    for triangle in triangles.iter() {
      indices.push(triangle.0 as u32);
      indices.push(triangle.1 as u32);
      indices.push(triangle.2 as u32);
    }

    let vertices_slice: &[f32] = &vertices;
    let indices_slice: &[u32] = &indices;

    let vao = gl.gen_vertex_arrays(1)[0];
    let vbo = gl.gen_buffers(1)[0];
    let ebo = gl.gen_buffers(1)[0];

    gl.bind_vertex_array(vao);

    // bind vertices
    gl.bind_buffer(gl::ARRAY_BUFFER, vbo);
    gl::buffer_data(gl, gl::ARRAY_BUFFER, vertices_slice, gl::STATIC_DRAW);

    // bind indices
    gl.bind_buffer(gl::ELEMENT_ARRAY_BUFFER, ebo);
    gl::buffer_data(gl, gl::ELEMENT_ARRAY_BUFFER, indices_slice, gl::STATIC_DRAW);

    match spec {
      GeometrySpec::Bare(_) => {
        let stride = 2 * size_of::<f32>() as i32;

        // position attribute
        let pos_location = shader.attrib_location("pos")?;
        gl.vertex_attrib_pointer(pos_location, 2, gl::FLOAT, false, stride, 0);
        gl.enable_vertex_attrib_array(pos_location);
      }

      GeometrySpec::Textured(_, _) => {
        let stride = 4 * size_of::<f32>() as i32;

        // position attribute
        let pos_location = shader.attrib_location("pos")?;
        gl.vertex_attrib_pointer(pos_location, 2, gl::FLOAT, false, stride, 0);
        gl.enable_vertex_attrib_array(pos_location);

        // tex coord attribute
        let tex_coord_location = shader.attrib_location("tex_coord")?;
        gl.vertex_attrib_pointer(
          tex_coord_location,
          2,
          gl::FLOAT,
          false,
          stride,
          (size_of::<f32>() * 2) as u32,
        );
        gl.enable_vertex_attrib_array(tex_coord_location);
      }

      GeometrySpec::Colored(_) => {
        let stride = 6 * size_of::<f32>() as i32;

        // position attribute
        let pos_location = shader.attrib_location("pos")?;
        gl.vertex_attrib_pointer(pos_location, 2, gl::FLOAT, false, stride, 0);
        gl.enable_vertex_attrib_array(pos_location);

        // color attribute
        let color_location = shader.attrib_location("color")?;
        gl.vertex_attrib_pointer(
          color_location,
          4,
          gl::FLOAT,
          false,
          stride,
          (size_of::<f32>() * 2) as u32,
        );
        gl.enable_vertex_attrib_array(color_location);
      }
    }

    gl.bind_buffer(gl::ARRAY_BUFFER, 0);
    gl.bind_vertex_array(0);

    let model_location = shader.uniform_location("model")?;
    let view_location = shader.uniform_location("view")?;
    Ok(Geometry {
      gl,
      spec: match spec {
        GeometrySpec::Bare(_) => GeometrySpecInternal::Bare,
        GeometrySpec::Textured(texture, _) => GeometrySpecInternal::Textured(texture.clone()),
        GeometrySpec::Colored(_) => GeometrySpecInternal::Colored,
      },
      shader,
      vao,
      vbo,
      ebo,
      num_indices: indices.len(),
      model_location,
      view_location,
    })
  }

  pub fn draw(&self, model: &Matrix3<f32>, view: &Matrix3<f32>) {
    match self.spec {
      GeometrySpecInternal::Bare => (),
      GeometrySpecInternal::Textured(ref texture) => texture.bind(),
      GeometrySpecInternal::Colored => (),
    }
    self.shader.use_();
    self
      .gl
      .uniform_matrix_3fv(self.model_location as i32, false, model.as_slice());
    self
      .gl
      .uniform_matrix_3fv(self.view_location as i32, false, view.as_slice());
    self.gl.bind_vertex_array(self.vao);
    self
      .gl
      .draw_elements(gl::TRIANGLES, self.num_indices as i32, gl::UNSIGNED_INT, 0);
  }
}

impl<'gl> Drop for Geometry<'gl> {
  fn drop(&mut self) {
    self.gl.delete_vertex_arrays(&[self.vao]);
    self.gl.delete_buffers(&[self.vbo]);
    self.gl.delete_buffers(&[self.ebo]);
  }
}
