// TODO change from Normal / Ellipsis to separate LHS and RHS
use crate::grid::*;
use crate::puzzlescript::colors::*;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Prelude {
  pub title: Option<String>,
  pub author: Option<String>,
  pub text_color: Color,
  /// in seconds
  pub key_repeat_interval: f64,
  pub homepage: Option<String>,
  pub require_player_movement: bool,
  pub background_color: Color,
  pub color_palette: ColorPalette,
  pub noundo: bool,
  pub run_rules_on_level_start: bool,
  pub again_interval: Option<f64>,
  pub norepeat_action: bool,
  pub debug: bool,
  pub verbose_logging: bool,
}

pub type ObjectName = Rc<str>;
pub type PropertyName = Rc<str>;
pub type AggregateName = Rc<str>;
pub type EntityName = Rc<str>;

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
  Empty(Color),
  Normal(Grid<Color>),
}

impl Object {
  pub fn normal(lines: &[Vec<Color>]) -> Object {
    Object::Normal(Grid::generate(5, 5, |(row, col)| lines[row][col]))
  }

  pub fn empty(color: Color) -> Object {
    Object::Empty(color)
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum EntityQualifier {
  ArrowRight,
  ArrowLeft,
  ArrowUp,
  ArrowDown,
  Up,
  Down,
  Left,
  Right,
  Moving,
  Stationary,
  Orthogonal,
  Action,
  No,
  RandomDir,
  Random,
  Perpendicular,
  Parallel,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RuleDirection {
  Up,
  Down,
  Left,
  Right,
  Horizontal,
  Vertical,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct MatchEntity {
  pub qualifier: Option<EntityQualifier>,
  pub entity: EntityName,
}

/// From 0 to 10
pub type SoundFx = u8;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CellMatcher<RHS> {
  Ellipsis,
  Entities(Vec<MatchEntity>, RHS),
}

pub type Matcher<RHS> = Vec<CellMatcher<RHS>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuleCommand {
  Message(Rc<str>),
  Sound(SoundFx),
  Cancel,
  Restart,
  Again,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuleBody {
  Normal(Vec<Matcher<Vec<MatchEntity>>>),
  NoConsequence(Vec<Matcher<()>>), // just used for sounds and commands
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Rule {
  pub line_number: usize, // for debugging
  pub direction: Option<RuleDirection>,
  pub body: RuleBody,
  pub commands: Vec<RuleCommand>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WinCondition {
  No(EntityName),
  NoOn(EntityName, EntityName),
  AllOn(EntityName, EntityName),
  Some(EntityName),
  SomeOn(EntityName, EntityName),
}

pub type LegendName = Rc<str>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LegendBody<A: Hash + Eq> {
  /// 'and' separated
  Aggregate(HashSet<A>),
  /// 'or' separated
  Property(HashSet<A>),
  /// just another name for another entity.
  Alias(EntityName),
}

pub type LevelStage = Grid<EntityName>;

#[derive(Debug, PartialEq, Clone)]
pub enum Level {
  Stage(LevelStage),
  Message(Rc<str>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RuleGroup {
  pub late: bool,
  pub random: bool,
  pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Entities {
  pub objects: HashMap<ObjectName, Object>,
  pub legend: HashMap<LegendName, LegendBody<EntityName>>,
  /// The order the legends were defined by. We store it so that we do not have
  /// to topologically sort them later in the compiler.
  pub legend_order: Vec<EntityName>,
}

pub type CollisionLayers = Vec<Vec<EntityName>>;

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
  pub prelude: Prelude,
  pub entities: Entities,
  pub collision_layers: CollisionLayers,
  pub rules: Vec<RuleGroup>,
  pub win_conditions: Vec<WinCondition>,
  pub levels: Vec<Level>,
}

impl Ast {
  pub fn empty() -> Ast {
    Ast {
      prelude: Prelude {
        title: None,
        author: None,
        text_color: Color::Named(NamedColor::White),
        key_repeat_interval: 0.1,
        homepage: None,
        require_player_movement: false,
        background_color: Color::Named(NamedColor::Black),
        color_palette: ColorPalette::Arne,
        noundo: false,
        run_rules_on_level_start: false,
        again_interval: None,
        norepeat_action: false,
        debug: false,
        verbose_logging: false,
      },
      entities: Entities {
        objects: HashMap::new(),
        legend: HashMap::new(),
        legend_order: Vec::new(),
      },
      collision_layers: Vec::new(),
      rules: Vec::new(),
      win_conditions: Vec::new(),
      levels: Vec::new(),
    }
  }
}
