use failure::{Error, Fail};
use gleam::gl;

pub struct Shader<'gl> {
  gl: &'gl gl::Gl,
  program: u32,
}

#[derive(Debug, Fail)]
#[fail(display = "Missing shader attribute {}", name)]
pub struct MissingShaderAttribute {
  pub name: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Missing shader uniform {}", name)]
pub struct MissingShaderUniform {
  pub name: &'static str,
}

#[derive(Debug, Fail)]
#[fail(display = "Shader compile error: {}", info_log)]
pub struct ShaderCompileError {
  pub info_log: String,
}

impl<'gl> Shader<'gl> {
  fn check_shader_compile(gl: &gl::Gl, shader: u32) -> Result<(), Error> {
    // see <https://www.khronos.org/opengl/wiki/Example/GLSL_Shader_Compile_Error_Testing>
    let mut is_compiled: Vec<i32> = vec![-1, 1];
    unsafe {
      gl.get_shader_iv(shader, gl::COMPILE_STATUS, &mut is_compiled);
    }
    if is_compiled[0] == gl::FALSE as i32 {
      let info_log = gl.get_shader_info_log(shader);
      Err(ShaderCompileError { info_log })?
    } else {
      Ok(())
    }
  }

  pub fn new(gl: &'gl gl::Gl, vertex_src: &str, fragment_src: &str) -> Result<Shader<'gl>, Error> {
    let program: u32;
    let vertex: u32;
    let fragment: u32;

    vertex = gl.create_shader(gl::VERTEX_SHADER);
    gl.shader_source(vertex, &[vertex_src.as_bytes()]);
    gl.compile_shader(vertex);
    Shader::check_shader_compile(gl, vertex)?;

    fragment = gl.create_shader(gl::FRAGMENT_SHADER);
    gl.shader_source(fragment, &[fragment_src.as_bytes()]);
    gl.compile_shader(fragment);
    Shader::check_shader_compile(gl, fragment)?;

    program = gl.create_program();
    gl.attach_shader(program, vertex);
    gl.attach_shader(program, fragment);
    gl.link_program(program);

    gl.delete_shader(vertex);
    gl.delete_shader(fragment);

    Ok(Shader {
      gl,
      program: program,
    })
  }

  /// does _not_ check for errors -- the caller should.
  pub fn use_(&self) {
    // TODO consider not binding if the current shader is already active --
    // binding shaders is slow
    self.gl.use_program(self.program);
  }

  pub fn attrib_location(&self, attrib: &'static str) -> Result<u32, Error> {
    let loc: i32;
    loc = self.gl.get_attrib_location(self.program, attrib);
    if loc >= 0 {
      Ok(loc as u32)
    } else {
      Err(MissingShaderAttribute { name: attrib })?
    }
  }

  pub fn uniform_location(&self, uniform: &'static str) -> Result<u32, Error> {
    let loc: i32;
    loc = self.gl.get_uniform_location(self.program, uniform);
    if loc >= 0 {
      Ok(loc as u32)
    } else {
      Err(MissingShaderUniform { name: uniform })?
    }
  }

  pub fn list_uniforms(&self) -> Result<Vec<String>, Error> {
    let mut count: Vec<i32> = vec![-1, 1];
    unsafe {
      self
        .gl
        .get_program_iv(self.program, gl::ACTIVE_UNIFORMS, &mut count);
    }

    let mut names: Vec<String> = Vec::new();
    for ix in 0..count[0] {
      let (_, _, name) = self.gl.get_active_uniform(self.program, ix as u32);
      names.push(name);
    }

    Ok(names)
  }

  pub fn list_attributes(&self) -> Result<Vec<String>, Error> {
    let mut count: Vec<i32> = vec![-1, 1];
    unsafe {
      self
        .gl
        .get_program_iv(self.program, gl::ACTIVE_ATTRIBUTES, &mut count);
    }

    let mut names: Vec<String> = Vec::new();
    for ix in 0..count[0] {
      let (_, _, name) = self.gl.get_active_attrib(self.program, ix as u32);
      names.push(name);
    }

    Ok(names)
  }
}

impl<'gl> Drop for Shader<'gl> {
  fn drop(&mut self) {
    self.gl.delete_program(self.program);
  }
}
