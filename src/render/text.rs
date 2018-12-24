use crate::math::*;
use crate::render::layout::*;
use crate::render::shader::*;
use crate::render::texture::*;
use failure::Error;
use gleam::gl;
use glutin::dpi::LogicalSize;
use std::collections::HashMap;
use std::mem::size_of;
use std::ptr;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct GlyphMetrics {
  index: u32,
  // we store all these sizes in logical size -- e.g. logical pixels rather than
  // physical pixels.
  size: Vector2<f64>,
  bearing: Vector2<f64>,
  advance: f64,
}

pub struct Glyph<'gl> {
  texture: Texture<'gl>,
  metrics: GlyphMetrics,
}

impl<'gl> Glyph<'gl> {
  fn new(
    gl: &'gl gl::Gl,
    face: &freetype::Face,
    hidpi_factor: f64,
    ch: char,
  ) -> Result<Glyph<'gl>, Error> {
    let index = face.get_char_index(ch as usize);
    face.load_glyph(index, freetype::face::LoadFlag::RENDER)?;
    let glyph = face.glyph();
    let bitmap = glyph.bitmap();
    let bytes = bitmap.buffer().to_vec();
    let img: image::GrayImage =
      image::ImageBuffer::from_raw(bitmap.width() as u32, bitmap.rows() as u32, bytes).unwrap();
    let texture = Texture::new(
      gl,
      &[
        TextureParameter::WrapS(TextureWrap::ClampToEdge),
        TextureParameter::WrapT(TextureWrap::ClampToEdge),
        TextureParameter::MinFilter(TextureMinFilter::Nearest),
        TextureParameter::MagFilter(TextureMagFilter::Nearest),
      ],
      &image::DynamicImage::ImageLuma8(img),
      false,
    )?;
    Ok(Glyph {
      texture,
      metrics: GlyphMetrics {
        index,
        size: vec2(
          bitmap.width() as f64 / hidpi_factor,
          bitmap.rows() as f64 / hidpi_factor,
        ),
        bearing: vec2(
          glyph.bitmap_left() as f64 / hidpi_factor,
          glyph.bitmap_top() as f64 / hidpi_factor,
        ),
        advance: (glyph.advance().x as f64 / 2_f64.powi(6)) / hidpi_factor,
      },
    })
  }
}

struct GlyphCache<'gl> {
  map: HashMap<char, Glyph<'gl>>,
}

impl<'gl> GlyphCache<'gl> {
  fn get_glyph(
    &mut self,
    gl: &'gl gl::Gl,
    face: &freetype::Face,
    hidpi_factor: f64,
    ch: char,
  ) -> Result<&Glyph<'gl>, Error> {
    Ok(
      self
        .map
        .entry(ch)
        .or_insert(Glyph::new(gl, face, hidpi_factor, ch)?),
    )
  }
}

pub struct Face<'gl> {
  gl: &'gl gl::Gl,
  face: freetype::Face,
  glyph_cache: GlyphCache<'gl>,
  vao: u32,
  vbo: u32,
  shader: Shader<'gl>,
  text_color_location: u32,
  hidpi_factor: f64,
}

impl<'gl> Face<'gl> {
  pub fn new(
    gl: &'gl gl::Gl,
    face_bytes: Rc<Vec<u8>>,
    hidpi_factor: f64,
    pixel_height: u32,
  ) -> Result<Face<'gl>, Error> {
    // setup font
    let freetype_lib = freetype::Library::init()?;

    let face = freetype_lib.new_memory_face(face_bytes, 0)?;
    face.set_pixel_sizes(0, (pixel_height as f64 * hidpi_factor) as u32)?;

    let glyph_cache = GlyphCache {
      map: HashMap::new(),
    };

    // setup shader
    let shader = Shader::new(
      gl,
      include_str!("../../shaders/glyph.vert"),
      include_str!("../../shaders/glyph.frag"),
    )?;
    let text_color_location = shader.uniform_location("text_color")?;

    // setup buffers
    let vao = gl.gen_vertex_arrays(1)[0];
    let vbo = gl.gen_buffers(1)[0];

    gl.bind_vertex_array(vao);
    gl.bind_buffer(gl::ARRAY_BUFFER, vbo);
    gl.bind_buffer(gl::ARRAY_BUFFER, vbo);
    gl.buffer_data_untyped(
      gl::ARRAY_BUFFER,
      (size_of::<f32>() * 6 * 4) as isize,
      ptr::null(),
      gl::DYNAMIC_DRAW,
    );

    gl.enable_vertex_attrib_array(0);
    gl.vertex_attrib_pointer(0, 4, gl::FLOAT, false, (4 * size_of::<f32>()) as i32, 0);
    gl.bind_buffer(gl::ARRAY_BUFFER, 0);
    gl.bind_vertex_array(0);

    Ok(Face {
      gl,
      face,
      glyph_cache,
      vao,
      vbo,
      shader,
      text_color_location,
      hidpi_factor,
    })
  }

  fn get_glyph(&mut self, ch: char) -> Result<&Glyph<'gl>, Error> {
    self
      .glyph_cache
      .get_glyph(self.gl, &self.face, self.hidpi_factor, ch)
  }

  /// Note: the position refers to the "origin" as in this picture
  /// <https://www.freetype.org/freetype2/docs/glyphs/metrics.png>
  #[rustfmt::skip]
  pub fn draw(
    &mut self,
    window_size: &LogicalSize,
    text: &str,
    color: &Vector3<f32>,
    layout_pos: &LayoutPosition,
    max_width: f64,
  ) -> Result<(), Error> {
    let words = text.split_whitespace();

    let vao = self.vao;
    let vbo = self.vbo;

    self.shader.use_();
    self.gl.uniform_3f(
      self.text_color_location as i32,
      color[0],
      color[1],
      color[2],
    );
    self.gl.active_texture(gl::TEXTURE0);
    self.gl.bind_vertex_array(vao);

    let scale = layout_scale(window_size);

    let line_height =
      (self.face.size_metrics().unwrap().height as f64 / 2_f64.powi(6)) / self.hidpi_factor;
    let space_width = self.get_glyph(' ')?.metrics.advance;

    let mut layout_pos = layout_pos.modify_origin(window_size, LayoutOrigin::TopLeft);
    let start_layout_x = layout_pos.position[0];
    let mut mb_prev_ch: Option<u32> = None;
    let mut beginning_of_line = true;

    for word in words {
      // first calculate the word size and see if we're within the required
      // width
      let mut word_width = 0.0;
      for ch in word.chars() {
        word_width += self.get_glyph(ch)?.metrics.advance;
      }
      // never enter a new line if it's the first word we're rendering
      if !beginning_of_line
        && (layout_pos.position[0] + (space_width + word_width) - start_layout_x > max_width)
      {
        layout_pos.position[0] = start_layout_x;
        layout_pos.position[1] += line_height;
        beginning_of_line = true;
      }
      if !beginning_of_line {
        // space from previous word if we're not on a new line
        layout_pos += vec2(space_width, 0.0);
      }

      // then, actually write the word.
      for ch in word.chars() {
        // this trick is just so that `self` is not borrowed as mutable in the
        // rest of the loop iteration.
        let glyph_metrics = {
          let glyph = self.get_glyph(ch)?;
          glyph.texture.bind();
          glyph.metrics
        };

        match mb_prev_ch {
          None => (),
          Some(prev_ch) => {
            let kerning = self.face.get_kerning(
              prev_ch,
              glyph_metrics.index,
              freetype::face::KerningMode::KerningDefault,
            )?;
            layout_pos += vec2(kerning.x as f64 / 2_f64.powi(6), 0.0);
          }
        }

        let pos = (layout_pos
          + vec2(
            glyph_metrics.bearing[0],
            glyph_metrics.size[1] - glyph_metrics.bearing[1],
          ))
        .to_clipping_coordinates(window_size);
        let xpos = pos[0] as f32;
        let ypos = pos[1] as f32;

        let w = (glyph_metrics.size[0] * scale[0]) as f32;
        let h = (glyph_metrics.size[1] * scale[1]) as f32;

        let vertices: [[f32; 4]; 6] = [
          [xpos,     ypos + h,  0.0, 0.0],
          [xpos,     ypos,      0.0, 1.0],
          [xpos + w, ypos,      1.0, 1.0],

          [xpos,     ypos + h,  0.0, 0.0],
          [xpos + w, ypos,      1.0, 1.0],
          [xpos + w, ypos + h,  1.0, 0.0],
        ];

        self.gl.bind_buffer(gl::ARRAY_BUFFER, vbo);
        gl::buffer_sub_data(self.gl, gl::ARRAY_BUFFER, 0, &vertices);
        self.gl.bind_buffer(gl::ARRAY_BUFFER, 0);
        self.gl.draw_arrays(gl::TRIANGLES, 0, 6);
        layout_pos += vec2(glyph_metrics.advance, 0.0);
        mb_prev_ch = Some(glyph_metrics.index);
        beginning_of_line = false;
      }
    }

    self.gl.bind_vertex_array(0);
    self.gl.bind_texture(gl::TEXTURE_2D, 0);
    Ok(())
  }
}
