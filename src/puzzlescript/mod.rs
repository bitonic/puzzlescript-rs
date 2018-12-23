use crate::render::*;
use failure::Error;
use gleam::gl;
use glutin::*;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::atomic::Ordering;
use std::time::SystemTime;
use structopt::StructOpt;

#[macro_use]
pub mod logging;

pub mod ast;
pub mod colors;
pub mod compiler;
pub mod engine;
pub mod game;
pub mod parser;
pub mod render;
pub mod state;

use self::colors::*;
use self::logging::*;
use self::state::*;

#[derive(Debug, StructOpt)]
pub struct Opts {
    #[structopt(parse(from_os_str))]
    input: PathBuf,
    #[structopt(long = "start-from-level")]
    start_from_level: Option<usize>,
}

struct SizeState<'gl> {
    window_size: dpi::LogicalSize,
    font: Face<'gl>,
}

impl<'gl> SizeState<'gl> {
    pub fn new(gl_window: &glutin::GlWindow, gl: &'gl gl::Gl) -> Result<SizeState<'gl>, Error> {
        let hidpi_factor = gl_window.get_hidpi_factor();
        // TODO do not reload the font unless hidpi changed
        Ok(SizeState {
            window_size: gl_window.get_inner_size().unwrap(),
            font: Face::new(
                &*gl,
                Rc::new(
                    include_bytes!("../../assets/fonts/IM-Fell-Double-Pica-Pro-Italic.otf")
                        .to_vec(),
                ),
                hidpi_factor,
                40,
            )?,
        })
    }
}

pub fn run(opts: Opts) -> Result<(), Error> {
    let src: String = fs::read_to_string(opts.input)?;
    let game = {
        let ast = parser::parse(&src)?;
        compiler::compile(&ast)?
    };

    VERBOSE_LOGGING.store(game.prelude.verbose_logging, Ordering::Relaxed);
    DEBUG_LOGGING.store(game.prelude.debug, Ordering::Relaxed);

    debug_log!(
        "{} rules",
        game.rules
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

    let object_shader = self::render::ObjectShader::new(&*gl)?;

    let object_sprites = {
        let mut object_sprites = HashMap::new();
        for (object_name, object) in game.objects.iter() {
            object_sprites.insert(
                object_name.clone(),
                self::render::ObjectSprite::new(
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
    let background_color_f32 = to_float_color(game.prelude.color_palette, &background_color);
    gl.clear_color(
        background_color_f32[0],
        background_color_f32[1],
        background_color_f32[2],
        background_color_f32[3],
    );

    let mut size_state = SizeState::new(&gl_window, &*gl)?;
    let mut game_state = State::new(&game, opts.start_from_level);
    let mut prev_frame_time = SystemTime::now();

    'running: loop {
        let dt = prev_frame_time.elapsed()?;
        prev_frame_time = SystemTime::now();

        let mut mb_command = None;
        let events = window::collect_events(&mut events_loop);
        for event in events.iter() {
            window::handle_resize_events(&gl_window, &*gl, &event);
            match event {
                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::CloseRequested => break 'running,
                    WindowEvent::Resized(_window_size) => {
                        size_state = SizeState::new(&gl_window, &*gl)?
                    }
                    WindowEvent::KeyboardInput { input, .. } => match input.virtual_keycode {
                        Some(virtual_key) => match input.state {
                            ElementState::Released => (),
                            ElementState::Pressed => {
                                if virtual_key == VirtualKeyCode::Space
                                    || virtual_key == VirtualKeyCode::X
                                {
                                    mb_command = Some(Command::Action);
                                }
                                if virtual_key == VirtualKeyCode::Up
                                    || virtual_key == VirtualKeyCode::W
                                {
                                    mb_command = Some(Command::Up);
                                }
                                if virtual_key == VirtualKeyCode::Down
                                    || virtual_key == VirtualKeyCode::S
                                {
                                    mb_command = Some(Command::Down);
                                }
                                if virtual_key == VirtualKeyCode::Left
                                    || virtual_key == VirtualKeyCode::A
                                {
                                    mb_command = Some(Command::Left);
                                }
                                if virtual_key == VirtualKeyCode::Right
                                    || virtual_key == VirtualKeyCode::D
                                {
                                    mb_command = Some(Command::Right);
                                }
                                if virtual_key == VirtualKeyCode::Z {
                                    mb_command = Some(Command::Undo);
                                }
                                if virtual_key == VirtualKeyCode::R {
                                    mb_command = Some(Command::Restart);
                                }
                            }
                        },
                        _ => (),
                    },
                    _ => (),
                },
                _ => (),
            }
        }

        window::clear(&*gl);

        game_state.update(dt, mb_command);

        self::render::draw_level(
            &size_state.window_size,
            game.prelude.color_palette,
            &foreground_color,
            &mut size_state.font,
            &object_sprites,
            &game.collision_layers,
            game_state.to_draw(),
        )?;

        gl_window.swap_buffers()?;
    }

    Ok(())
}
