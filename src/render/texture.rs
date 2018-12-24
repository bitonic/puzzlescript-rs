use failure::{Error, Fail};
use gleam::gl;
use image::{DynamicImage, GenericImageView};

#[derive(Debug, Clone, Copy)]
pub enum TextureWrap {
  Repeat,
  ClampToEdge,
}

impl TextureWrap {
  fn param(self) -> i32 {
    let p = match self {
      TextureWrap::Repeat => gl::REPEAT,
      TextureWrap::ClampToEdge => gl::CLAMP_TO_EDGE,
    };
    p as i32
  }
}

#[derive(Debug, Clone, Copy)]
pub enum TextureMinFilter {
  Nearest,
  Linear,
  NearestMipmapNearest,
  LinearMipmapNearest,
  NearestMipmapLinear,
  LinearMipmapLinear,
}

impl TextureMinFilter {
  fn param(self) -> i32 {
    let p = match self {
      TextureMinFilter::Nearest => gl::NEAREST,
      TextureMinFilter::Linear => gl::LINEAR,
      TextureMinFilter::NearestMipmapNearest => gl::NEAREST_MIPMAP_NEAREST,
      TextureMinFilter::LinearMipmapNearest => gl::LINEAR_MIPMAP_NEAREST,
      TextureMinFilter::NearestMipmapLinear => gl::NEAREST_MIPMAP_LINEAR,
      TextureMinFilter::LinearMipmapLinear => gl::LINEAR_MIPMAP_LINEAR,
    };
    p as i32
  }
}

#[derive(Debug, Clone, Copy)]
pub enum TextureMagFilter {
  Nearest,
  Linear,
}

impl TextureMagFilter {
  fn param(self) -> i32 {
    let p = match self {
      TextureMagFilter::Nearest => gl::NEAREST,
      TextureMagFilter::Linear => gl::LINEAR,
    };
    p as i32
  }
}

#[derive(Debug, Clone, Copy)]
pub enum TextureParameter {
  WrapS(TextureWrap),
  WrapT(TextureWrap),
  MinFilter(TextureMinFilter),
  MagFilter(TextureMagFilter),
}

impl TextureParameter {
  fn apply(self, gl: &gl::Gl) {
    match self {
      TextureParameter::WrapS(wrap) => {
        gl.tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, wrap.param())
      }
      TextureParameter::WrapT(wrap) => {
        gl.tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, wrap.param())
      }
      TextureParameter::MinFilter(min_filter) => {
        gl.tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, min_filter.param())
      }
      TextureParameter::MagFilter(mag_filter) => {
        gl.tex_parameter_i(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, mag_filter.param())
      }
    }
  }
}

pub struct Texture<'gl> {
  gl: &'gl gl::Gl,
  texture: u32,
}

#[derive(Debug, Fail)]
#[fail(display = "Unsupported image color: {:?}", color)]
pub struct TextureUnsupportedImageColor {
  pub color: image::ColorType,
}

impl<'gl> Texture<'gl> {
  /// Supported images:
  /// * Gray(8), gets stuffed in the red channel;
  /// * GrayA(8), the gray gets stuffed in the red channel and the alpha in the
  ///   green channel;
  /// * RGB(8)
  /// * RGBA(8)
  ///
  /// The input image is not touched before being turned into a texture, so you
  /// probably need to `DynamicImage.flipv` to put it in OpenGL texture
  /// coordinates.
  #[allow(clippy::new_ret_no_self)]
  pub fn new(
    gl: &'gl gl::Gl,
    params: &[TextureParameter],
    image: &DynamicImage,
    generate_mipmap: bool,
  ) -> Result<Texture<'gl>, Error> {
    let format = match image.color() {
      image::ColorType::Gray(8) => Ok(gl::RED),
      image::ColorType::GrayA(8) => Ok(gl::RG),
      image::ColorType::RGB(8) => Ok(gl::RGB),
      image::ColorType::RGBA(8) => Ok(gl::RGBA),
      c => Err(TextureUnsupportedImageColor { color: c }),
    }?;
    let width = image.dimensions().0 as i32;
    let height = image.dimensions().1 as i32;
    let img_bytes = image.raw_pixels();

    let tex: u32 = gl.gen_textures(1)[0];
    gl.bind_texture(gl::TEXTURE_2D, tex);

    for param in params.iter() {
      param.apply(gl);
    }

    gl.tex_image_2d(
      gl::TEXTURE_2D,
      0,
      format as i32,
      width,
      height,
      0,
      format,
      gl::UNSIGNED_BYTE,
      // space character has no length
      if img_bytes.is_empty() {
        None
      } else {
        Some(&img_bytes)
      },
    );
    if generate_mipmap {
      gl.generate_mipmap(gl::TEXTURE_2D);
    }
    Ok(Texture { gl, texture: tex })
  }

  /// does _not_ check for errors -- the caller should.
  pub fn bind(&self) {
    // TODO consider not binding if the current texture is already active --
    // binding textures is sort of slow
    self.gl.bind_texture(gl::TEXTURE_2D, self.texture);
  }
}

impl<'gl> Drop for Texture<'gl> {
  fn drop(&mut self) {
    self.gl.delete_textures(&[self.texture]);
  }
}
