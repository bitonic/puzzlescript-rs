use crate::puzzlescript::engine;
use crate::puzzlescript::game::*;
use lazy_static::*;
use std::rc::Rc;
use std::time::Duration;
use std::time::SystemTime;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
struct LevelState {
  /// what level of the game we're in
  level_number: usize,
  /// note that this might be modified -- it's not the level as-is from
  /// `Game`.
  level: Level,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Command {
  Up,
  Down,
  Left,
  Right,
  Action,
  Undo,
  Restart,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
enum Status {
  Playing,
  /// we flash the completed level for a bit after a win
  FlashWonLevel {
    level: Level,
    start: Duration,
  },
  /// we finished the game
  Won {
    /// the last level to show
    level: Level,
  },
  IntraLevelMessage {
    message: Rc<str>,
  },
}

/// flash win screen for half a second
lazy_static! {
  static ref FLASH_WON_LEVEL: Duration = Duration::new(0, 500_000_000);
}

pub struct State<'a> {
  game: &'a Game,
  history: Vec<LevelState>,
  /// how long since the game started
  time: Duration,
  status: Status,
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
struct SerializedState {
  history: Vec<LevelState>,
  time: Duration,
  status: Status,
}

/*
fn duration_to_seconds(dt: Duration) -> f64 {
  dt.as_secs() as f64 * (dt.subsec_nanos() as f64 / 1_000_000 as f64)
}
*/

impl<'a> State<'a> {
  // TODO remove the `Level`, which requires some pretty gratuitous
  // cloning. right now it does not happen to be a problem, but it
  // might become one if the level structure changes.
  pub fn to_draw(&self) -> Level {
    match self.status {
      Status::FlashWonLevel { ref level, .. } => level.clone(),
      Status::Playing => self.last_state().level.clone(),
      Status::Won { ref level } => level.clone(),
      Status::IntraLevelMessage { ref message } => Level::Message(message.clone()),
    }
  }

  fn last_state(&self) -> &LevelState {
    &self.history.last().unwrap()
  }

  fn push_level(&mut self, level_number: usize) {
    let level = match self.game.levels[level_number] {
      Level::Stage {
        ref stage,
        ref background,
      } if self.game.prelude.run_rules_on_level_start => {
        verbose_log!("Executing rules on level start");
        let new_stage = match engine::advance(self.game, &stage, None) {
          engine::Advance::Nothing => stage.clone(),
          engine::Advance::Active(new_stage, _sounds) => new_stage,
          engine::Advance::Won(_, _) => {
            panic!("TODO Won when executing rules at beginning of level!")
          }
          engine::Advance::Restart => {
            panic!("TODO Restart when executing rules at beginning of level!")
          }
          engine::Advance::Message(_, _, _) => {
            panic!("TODO Won when executing rules at beginning of level!")
          }
        };
        Level::Stage {
          stage: new_stage,
          background: background.clone(),
        }
      }
      ref level => level.clone(),
    };
    self.history.push(LevelState {
      level_number,
      level,
    })
  }

  fn reset_and_push_level(&mut self, level: usize) {
    self.history.clear();
    self.push_level(level);
  }

  fn has_next_level(&self) -> bool {
    self.last_state().level_number < self.game.levels.len() - 1
  }

  fn move_(&mut self, movement: Movement) {
    let last_state = self.last_state();
    let has_next_level = self.has_next_level();
    match &last_state.level {
      Level::Message(_) => {
        if movement == Movement::Action {
          if has_next_level {
            self.reset_and_push_level(last_state.level_number + 1);
          } else {
            self.status = Status::Won {
              level: last_state.level.clone(),
            };
          }
        }
      }
      Level::Stage { stage, background } => {
        let next_level = |new_stage| Level::Stage {
          background: background.clone(),
          stage: new_stage,
        };
        // then do the thing
        match engine::advance(&self.game, &stage, Some(movement)) {
          engine::Advance::Won(new_stage, _sounds) =>
          // if we've won, change the status to winning
          {
            if has_next_level {
              self.status = Status::FlashWonLevel {
                level: next_level(new_stage),
                start: self.time,
              }
            } else {
              self.status = Status::Won {
                level: next_level(new_stage),
              };
            }
          }
          engine::Advance::Restart => {
            // do not reset when restarting -- we want to be able to
            // undo beyond the restart
            self.push_level(last_state.level_number);
          }
          engine::Advance::Active(new_stage, _sounds) =>
          // otherwise keep going
          {
            self.history.push(LevelState {
              level_number: last_state.level_number,
              level: next_level(new_stage),
            })
          }
          engine::Advance::Message(_sounds, message, new_stage) =>
          // if we need to display a message we still need to advance
          // the state
          {
            self.history.push(LevelState {
              level_number: last_state.level_number,
              level: next_level(new_stage),
            });
            self.status = Status::IntraLevelMessage { message };
          }
          engine::Advance::Nothing => (),
        }
      }
    }
  }

  pub fn new(game: &'a Game, starting_level: Option<usize>) -> State<'a> {
    let mut state = State {
      history: vec![],
      game,
      time: Duration::new(0, 0),
      status: Status::Playing,
    };
    state.push_level(starting_level.unwrap_or(0));
    state
  }

  pub fn update(&mut self, dt: Duration, mb_command: Option<Command>) {
    self.time += dt;

    if let Status::FlashWonLevel { start, .. } = self.status {
      if self.time - start > *FLASH_WON_LEVEL && self.has_next_level() {
        self.status = Status::Playing;
        self.reset_and_push_level(self.last_state().level_number + 1);
      }
    }

    // if we're _not_ winning, execute command
    match self.status {
      Status::FlashWonLevel { .. } => (),
      Status::Won { .. } => (),
      Status::IntraLevelMessage { .. } => match mb_command {
        None => (),
        Some(Command::Action) => self.status = Status::Playing,
        Some(_) => (),
      },
      Status::Playing => match mb_command {
        None => (),
        Some(command) => {
          let command_start_time = SystemTime::now();
          verbose_log!("====");
          verbose_log!("# Applying command {:?}", command);
          match command {
            Command::Up => self.move_(Movement::Up),
            Command::Down => self.move_(Movement::Down),
            Command::Left => self.move_(Movement::Left),
            Command::Right => self.move_(Movement::Right),
            Command::Action => self.move_(Movement::Action),
            Command::Undo => {
              if self.history.len() > 1 {
                self.history.pop();
              }
            }
            Command::Restart => {
              // do not reset when restarting -- we want to be able to
              // undo beyond the restart
              self.push_level(self.last_state().level_number);
            }
          }
          let command_time = command_start_time.elapsed().unwrap();
          let command_time_millis =
            command_time.as_secs() as f64 * 1000.0 + f64::from(command_time.subsec_millis());
          debug_log!("# Command cleared in {}ms", command_time_millis);
        }
      },
    }
  }

  pub fn save<W>(&self, writer: W) -> serde_json::Result<()>
  where
    W: std::io::Write,
  {
    serde_json::to_writer(
      writer,
      &SerializedState {
        history: self.history.clone(),
        time: self.time,
        status: self.status.clone(),
      },
    )
  }

  pub fn restore<R>(game: &'a Game, reader: R) -> serde_json::Result<State<'a>>
  where
    R: std::io::Read,
  {
    let serialized_state: SerializedState = serde_json::from_reader(reader)?;
    Ok(State {
      game,
      history: serialized_state.history,
      time: serialized_state.time,
      status: serialized_state.status,
    })
  }

  pub fn replay(game: &'a Game, commands: &[Command]) -> State<'a> {
    let mut st = State::new(game, None);
    for command in commands.iter() {
      st.update(Duration::new(0, 0), Some(*command));
      // immediately go forward when winning
      if let Status::FlashWonLevel { .. } = st.status {
        st.status = Status::Playing;
        st.reset_and_push_level(st.last_state().level_number + 1);
      }
    }
    st
  }
}

#[cfg(test)]
mod tests {
  use crate::puzzlescript::compiler;
  use crate::puzzlescript::parser;
  use crate::puzzlescript::state::*;

  fn won_test(pzl_src: &str, solution_str: &str) {
    let ast = parser::parse(pzl_src).unwrap();
    let game = compiler::compile(&ast).unwrap();
    let commands: Vec<Command> = serde_json::from_str(solution_str).unwrap();
    let state = State::replay(&game, &commands);
    match state.status {
      Status::Won { .. } => (),
      status => panic!("Expecting Won, got {:?}", status),
    }
  }

  #[test]
  fn tutorial_basic() {
    won_test(
      include_str!("../../puzzlescripts/tutorial/basic.pzl"),
      include_str!("../../puzzlescripts/tutorial/basic.solution"),
    );
  }

  #[test]
  fn tutorial_match_3() {
    won_test(
      include_str!("../../puzzlescripts/tutorial/match_3.pzl"),
      include_str!("../../puzzlescripts/tutorial/match_3.solution"),
    );
  }

  #[test]
  fn elementary_block_faker() {
    won_test(
      include_str!("../../puzzlescripts/elementary/block_faker.pzl"),
      include_str!("../../puzzlescripts/elementary/block_faker.solution"),
    );
  }

  #[test]
  fn elementary_by_your_side() {
    won_test(
      include_str!("../../puzzlescripts/elementary/by_your_side.pzl"),
      include_str!("../../puzzlescripts/elementary/by_your_side.solution"),
    );
  }

  #[test]
  fn elementary_kettle() {
    won_test(
      include_str!("../../puzzlescripts/elementary/kettle.pzl"),
      include_str!("../../puzzlescripts/elementary/kettle.solution"),
    );
  }

  #[test]
  fn elementary_neko() {
    won_test(
      include_str!("../../puzzlescripts/elementary/neko.pzl"),
      include_str!("../../puzzlescripts/elementary/neko.solution"),
    );
  }

  #[test]
  fn elementary_microban() {
    won_test(
      include_str!("../../puzzlescripts/elementary/microban.pzl"),
      include_str!("../../puzzlescripts/elementary/microban.solution"),
    );
  }

  #[test]
  fn heroes_of_sokoban_1() {
    won_test(
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_1.pzl"),
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_1.solution"),
    );
  }

  #[test]
  fn heroes_of_sokoban_2() {
    won_test(
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_2.pzl"),
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_2.solution"),
    );
  }

  #[test]
  fn heroes_of_sokoban_3() {
    won_test(
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_3.pzl"),
      include_str!("../../puzzlescripts/third_party/heroes_of_sokoban_3.solution"),
    );
  }
}
