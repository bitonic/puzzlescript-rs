use failure::Error;
use gleam::gl;
use glutin::dpi::LogicalSize;
use glutin::{
  Api, ContextBuilder, Event, EventsLoop, GlContext, GlRequest, GlWindow, WindowBuilder,
  WindowEvent,
};
use std::rc::Rc;

/// We assume that we have at least a 720p space to draw.
const MIN_WINDOW_WIDTH: u32 = 1280;
const MIN_WINDOW_HEIGHT: u32 = 720;

pub fn init(
  title: &str,
  set_min_window_size: bool,
) -> Result<(EventsLoop, GlWindow, Rc<gl::Gl>), Error> {
  let events_loop = EventsLoop::new();
  let window = WindowBuilder::new().with_title(title);
  // 3.3 just because that's what <https://learnopengl.com> uses
  let context = ContextBuilder::new()
    .with_gl(GlRequest::Specific(Api::OpenGl, (3, 3)))
    .with_vsync(true);
  // `unwrap` rather than floating the errors upstream since we can't go further
  // if we can't create a gl window anyway and because it does not implement
  // `Sync`.
  let gl_window = GlWindow::new(window, context, &events_loop).unwrap();

  // We set the minimum size to 720p physical for ease of testing
  if set_min_window_size {
    let min_dims = LogicalSize::new(
      f64::from(MIN_WINDOW_WIDTH) / gl_window.get_hidpi_factor(),
      f64::from(MIN_WINDOW_HEIGHT) / gl_window.get_hidpi_factor(),
    );
    gl_window.set_inner_size(min_dims);
    gl_window.set_min_dimensions(Some(min_dims));
  }

  unsafe {
    gl_window.make_current()?;
  }
  let gl_unchecked;
  unsafe {
    gl_unchecked = gl::GlFns::load_with(|symbol| gl_window.get_proc_address(symbol) as *const _);
  }
  let gl = gl::ErrorCheckingGl::wrap(gl_unchecked);

  // enable blending (for fonts)
  gl.enable(gl::BLEND);
  gl.blend_func(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);
  // all the image data we pass to OpenGL is tightly packed
  gl.pixel_store_i(gl::UNPACK_ALIGNMENT, 1);

  Ok((events_loop, gl_window, gl))
}

/// Automatically handles resize events. Forwards the event untouched
/// to the callback (including resize events).
pub fn handle_resize_events(gl_window: &GlWindow, gl: &gl::Gl, event: &Event) {
  if let Event::WindowEvent {
    event: WindowEvent::Resized(window_size),
    ..
  } = event
  {
    let hidpi_factor = gl_window.get_hidpi_factor();
    let phys_size = window_size.to_physical(hidpi_factor);
    gl_window.resize(phys_size);

    gl.viewport(0, 0, phys_size.width as i32, phys_size.height as i32);
  }
}

pub fn collect_events(events_loop: &mut EventsLoop) -> Vec<Event> {
  let mut events = Vec::<Event>::new();
  events_loop.poll_events(|event| events.push(event));
  events
}

pub fn clear(gl: &gl::Gl) {
  gl.clear(gl::COLOR_BUFFER_BIT);
}
