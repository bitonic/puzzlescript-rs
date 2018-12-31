use failure::{Error, Fail};
use gleam::gl;
use glutin::*;
use serde_json;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::Ordering;
use std::time::SystemTime;
use structopt::StructOpt;

use puzzlescript_core::colors::*;
use puzzlescript_core::state::*;
use puzzlescript_core::*;
use puzzlescript_render_gl as render;

mod window;

#[derive(Debug, StructOpt)]
struct Opts {
  #[structopt(parse(from_os_str))]
  input: PathBuf,
  #[structopt(long = "save-to", parse(from_os_str))]
  save_to: Option<PathBuf>,
  #[structopt(long = "restore-from", parse(from_os_str))]
  restore_from: Option<PathBuf>,
  #[structopt(long = "start-from-level")]
  start_from_level: Option<usize>,
  #[structopt(long = "record-to", parse(from_os_str))]
  record_to: Option<PathBuf>,
  #[structopt(long = "replay-from", parse(from_os_str))]
  replay_from: Option<PathBuf>,
}

struct SizeState<'gl> {
  window_size: dpi::LogicalSize,
  font: render::text::Face<'gl>,
}

impl<'gl> SizeState<'gl> {
  #[allow(clippy::new_ret_no_self)]
  fn new(gl_window: &glutin::GlWindow, gl: &'gl gl::Gl) -> Result<SizeState<'gl>, Error> {
    let hidpi_factor = gl_window.get_hidpi_factor();
    // TODO do not reload the font unless hidpi changed
    Ok(SizeState {
      window_size: gl_window.get_inner_size().unwrap(),
      font: render::text::Face::new(
        &*gl,
        Rc::new(include_bytes!("../assets/fonts/IM-Fell-Double-Pica-Pro-Italic.otf").to_vec()),
        hidpi_factor,
        40,
      )?,
    })
  }
}

#[derive(Debug, Fail)]
#[fail(display = "Puzzlescript execution error: {}", msg)]
struct PuzzlescriptError {
  msg: String,
}

fn save_state<'a, W>(state: &State<'a>, writer: W) -> serde_json::Result<()>
where
  W: std::io::Write,
{
  serde_json::to_writer(writer, &state.save())
}

fn restore_state<'a, R>(game: &'a game::Game, reader: R) -> serde_json::Result<State<'a>>
where
  R: std::io::Read,
{
  let serialized_state: SerializableState = serde_json::from_reader(reader)?;
  Ok(State::restore(game, serialized_state))
}

fn run(opts: Opts) -> Result<(), Error> {
  let src: String = fs::read_to_string(opts.input)?;
  let game = {
    let ast = parser::parse(&src)?;
    compiler::compile(&ast)?
  };

  VERBOSE_LOGGING.store(game.prelude.verbose_logging, Ordering::Relaxed);
  DEBUG_LOGGING.store(game.prelude.debug, Ordering::Relaxed);

  debug_log!(
    "{} rules",
    game
      .rules
      .iter()
      .fold(0, |count, rule_group| count + rule_group.rules.len())
  );
  debug_log!();
  for rule_group in game.rules.iter() {
    debug_log!("{}", rule_group);
  }
  debug_log!("late rules:");
  for rule_group in game.late_rules.iter() {
    debug_log!("{}", rule_group);
  }

  let title = match &game.prelude.title {
    None => &"PuzzleScript game",
    Some(ref title) => title.as_str(),
  };
  let (mut events_loop, gl_window, gl) = window::init(title, false)?;

  let object_shader = render::ObjectShader::new(&*gl)?;

  let object_sprites = {
    let mut object_sprites = HashMap::new();
    for (object_name, object) in game.objects.iter() {
      object_sprites.insert(
        object_name.clone(),
        render::ObjectSprite::new(
          &*gl,
          object_shader.clone(),
          game.prelude.color_palette,
          object,
        )?,
      );
    }
    object_sprites
  };

  let background_color = game.prelude.background_color;
  let foreground_color = game.prelude.text_color;
  let background_color_f32 = to_float_color(game.prelude.color_palette, background_color);
  gl.clear_color(
    background_color_f32.r(),
    background_color_f32.g(),
    background_color_f32.b(),
    background_color_f32.a(),
  );

  let mut size_state = SizeState::new(&gl_window, &*gl)?;
  let conflicting_options = |msg: &'static str| {
    Err(PuzzlescriptError {
      msg: format!("Conflicting options: {}", msg),
    })
  };
  let (mut game_state, mut commands_recording) =
    match (opts.start_from_level, opts.restore_from, opts.replay_from) {
      (Some(_), Some(_), _) => conflicting_options("start-from-level and restore-from")?,
      (Some(_), _, Some(_)) => conflicting_options("start-from-level and replay-from")?,
      (_, Some(_), Some(_)) => conflicting_options("restore-from and replay-from")?,
      (mb_start_level, None, None) => (State::new(&game, mb_start_level), Vec::new()),
      (None, Some(ref restore_from), None) => {
        let restore_from_file = File::open(restore_from)?;
        (restore_state(&game, restore_from_file)?, Vec::new())
      }
      (None, None, Some(ref replay_from)) => {
        let replay_from_file = File::open(replay_from)?;
        let commands: Vec<Command> = serde_json::from_reader(replay_from_file)?;
        (State::replay(&game, &commands), commands)
      }
    };
  let mut prev_frame_time = SystemTime::now();

  'running: loop {
    let dt = prev_frame_time.elapsed()?;
    prev_frame_time = SystemTime::now();

    let mut mb_command = None;
    let mut save = false;
    let events = window::collect_events(&mut events_loop);
    for event in events.iter() {
      window::handle_resize_events(&gl_window, &*gl, &event);
      if let Event::WindowEvent { event, .. } = event {
        match event {
          WindowEvent::CloseRequested => {
            match opts.record_to {
              None => (),
              Some(record_to) => {
                let record_to_file = File::create(record_to)?;
                serde_json::to_writer(record_to_file, &commands_recording)?;
              }
            }
            break 'running;
          }
          WindowEvent::Resized(_window_size) => size_state = SizeState::new(&gl_window, &*gl)?,
          WindowEvent::KeyboardInput { input, .. } => {
            if let Some(virtual_key) = input.virtual_keycode {
              if let ElementState::Pressed = input.state {
                if virtual_key == VirtualKeyCode::Space
                  || virtual_key == VirtualKeyCode::Return
                  || virtual_key == VirtualKeyCode::X
                {
                  mb_command = Some(Command::Action);
                }
                if virtual_key == VirtualKeyCode::Up || virtual_key == VirtualKeyCode::W {
                  mb_command = Some(Command::Up);
                }
                if virtual_key == VirtualKeyCode::Down || virtual_key == VirtualKeyCode::S {
                  mb_command = Some(Command::Down);
                }
                if virtual_key == VirtualKeyCode::Left || virtual_key == VirtualKeyCode::A {
                  mb_command = Some(Command::Left);
                }
                if virtual_key == VirtualKeyCode::Right || virtual_key == VirtualKeyCode::D {
                  mb_command = Some(Command::Right);
                }
                if virtual_key == VirtualKeyCode::Z {
                  mb_command = Some(Command::Undo);
                }
                if virtual_key == VirtualKeyCode::R {
                  mb_command = Some(Command::Restart);
                }
                if virtual_key == VirtualKeyCode::P {
                  save = true;
                }
              }
            }
          }
          _ => (),
        }
      }
    }

    window::clear(&*gl);

    if save {
      match opts.save_to {
        None => {
          eprintln!("You asked me to save the game, but you did not provide a path to save it to!")
        }
        Some(ref save_to) => {
          let save_to_file = File::create(save_to)?;
          save_state(&game_state, save_to_file)?;
          eprintln!("State saved to {:?}", save_to);
        }
      }
    }

    match mb_command {
      None => (),
      Some(command) => commands_recording.push(command),
    }

    game_state.update(dt, mb_command);

    render::draw_level(
      &size_state.window_size,
      game.prelude.color_palette,
      foreground_color,
      &mut size_state.font,
      &object_sprites,
      &game.collision_layers,
      &game_state.to_draw(),
    )?;

    gl_window.swap_buffers()?;
  }

  Ok(())
}

fn main() {
  run(Opts::from_args()).unwrap()
}
