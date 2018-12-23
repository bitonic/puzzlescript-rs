use crate::puzzlescript::engine;
use crate::puzzlescript::game::*;
use lazy_static::*;
use std::time::Duration;

#[derive(Debug, PartialEq, Eq, Clone)]
struct LevelState {
    /// what level of the game we're in
    level_number: usize,
    /// note that this might be modified -- it's not the level as-is from
    /// `Game`.
    level: Level,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Command {
    Up,
    Down,
    Left,
    Right,
    Action,
    Undo,
    Restart,
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

/*
fn duration_to_seconds(dt: Duration) -> f64 {
  dt.as_secs() as f64 * (dt.subsec_nanos() as f64 / 1_000_000 as f64)
}
*/

impl<'a> State<'a> {
    pub fn to_draw(&self) -> &Level {
        match self.status {
            Status::FlashWonLevel { ref level, .. } => level,
            Status::Playing => &self.last_state().level,
            Status::Won { ref level } => level,
        }
    }

    fn last_state(&self) -> &LevelState {
        &self.history.last().unwrap()
    }

    fn push_level(&mut self, level: usize) {
        self.history.push(LevelState {
            level_number: level,
            level: self.game.levels[level].clone(),
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
                let mut new_stage = stage.clone();
                // otherwise, create a new stage and apply movement
                for cell in new_stage.iter_mut() {
                    for player in self.game.players.iter() {
                        if cell.contains_key(player) {
                            cell[player] = movement;
                        }
                    }
                }
                // then do the thing
                match engine::advance(&self.game, &mut new_stage) {
                    engine::Advance::Won =>
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
                    engine::Advance::Active =>
                    // otherwise keep going
                    {
                        self.history.push(LevelState {
                            level_number: last_state.level_number,
                            level: next_level(new_stage),
                        })
                    }
                    engine::Advance::Nothing => (),
                }
            }
        }
    }

    pub fn new(game: &'a Game, starting_level: Option<usize>) -> State<'a> {
        let level = starting_level.unwrap_or(0);
        State {
            history: vec![LevelState {
                level_number: level,
                level: game.levels[level].clone(),
            }],
            game,
            time: Duration::new(0, 0),
            status: Status::Playing,
        }
    }

    pub fn update(&mut self, dt: Duration, mb_command: Option<Command>) {
        self.time += dt;

        match self.status {
            Status::FlashWonLevel { start, .. } => {
                if self.time - start > *FLASH_WON_LEVEL {
                    if self.has_next_level() {
                        self.status = Status::Playing;
                        self.reset_and_push_level(self.last_state().level_number + 1);
                    }
                }
            }
            Status::Playing => (),
            Status::Won { .. } => (),
        }

        // if we're _not_ winning, execute command
        match self.status {
            Status::FlashWonLevel { .. } => (),
            Status::Won { .. } => (),
            Status::Playing => match mb_command {
                None => (),
                Some(command) => match command {
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
                },
            },
        }
    }
}
