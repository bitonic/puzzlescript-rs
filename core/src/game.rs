use crate::ast;
use crate::grid::*;
use im_rc::hashmap as im_hashmap;
use serde_derive::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

pub use self::ast::{Object, ObjectName, Prelude, PropertyName, RuleCommand, SoundFx};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CollisionLayer {
  Background,
  Normal(Vec<ObjectName>), // vec to preserve drawing ordering
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum RuleDirection {
  Down,
  Up,
  Right,
  Left,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Qualifier {
  /// On the LHS, match anything. On the RHS:
  ///
  /// * For objects, keep whatever movement you already had in the cell
  ///   for that object, if any.
  /// * For properties, carry over the movement you already had in the
  ///   cell for the object the property refers to when the rule fires.
  Passthrough,
  Stationary,
  Up,
  Down,
  Left,
  Right,
  Action,
  RandomDir,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct QualifiedObject {
  pub object: ObjectName,
  pub qualifier: Qualifier,
}

/// Some unique identifier for an occurrence of a property on the LHS of a rule
pub type PropertyBinder = usize;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum EntityMatcher {
  /// match a single object.
  Object(QualifiedObject),
  /// match any of these objects
  Property {
    property: PropertyName,
    qualifier: Qualifier,
    binder: PropertyBinder,
  },
  /// make sure none of theseare in the cell
  No(HashSet<ObjectName>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CellModifier {
  /// insert the given object
  Object(QualifiedObject),
  /// insert the given property
  Property {
    qualifier: Qualifier,
    binder: PropertyBinder,
    property: PropertyName, // just for debugging / pretty printing
  },
  /// remove all these objects
  No(HashSet<ObjectName>),
}

pub type Objects<Object> = Rc<Vec<Object>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CellMatcher<RHS> {
  Ellipsis,
  Objects(Objects<EntityMatcher>, RHS),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct RHSCell {
  /// if present, place one of these objects at random
  pub random: Option<HashSet<ObjectName>>,
  pub normal: Objects<CellModifier>,
}

/// the `|` separated cell matchers, e.g. in `[ A | B | C ]` `A`, `B`, and `C`
/// are the cell matchers.
pub type Matcher<RHS> = Vec<CellMatcher<RHS>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuleBody {
  Normal(Vec<Matcher<RHSCell>>),
  NoConsequence(Vec<Matcher<()>>), // just used for sounds and commands
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Rule {
  pub line_number: usize,
  pub direction: RuleDirection,
  pub body: RuleBody,
  pub commands: Vec<RuleCommand>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum WinCondition {
  No(HashSet<ObjectName>),
  /// None of the ones on the left on none of the ones on the right
  NoOn(HashSet<ObjectName>, HashSet<ObjectName>),
  /// None of the ones on the left on any of the ones on the right
  AllOn(HashSet<ObjectName>, HashSet<ObjectName>),
  /// At least one of the ones listed
  Some(HashSet<ObjectName>),
  /// At least one of the ones on the left on any of those on the right
  SomeOn(HashSet<ObjectName>, HashSet<ObjectName>),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub enum Movement {
  Up,
  Down,
  Left,
  Right,
  Action,
  Stationary,
}

pub type Cell = im_hashmap::HashMap<ObjectName, Movement>;
pub type Stage = Grid<Cell>;

#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize)]
pub enum Level {
  Stage {
    stage: Stage,
    background: Option<ObjectName>,
  },
  Message(Rc<str>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RuleGroup {
  pub random: bool,
  pub rules: Vec<Rule>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Game {
  pub prelude: Prelude,
  pub objects: HashMap<ObjectName, Object>,
  pub properties: HashMap<PropertyName, HashSet<ObjectName>>,
  /// These must cover _all_ the objects.
  pub collision_layers: Vec<CollisionLayer>,
  pub rules: Vec<RuleGroup>,
  pub late_rules: Vec<RuleGroup>,
  /// All of these must be satisfied
  pub win_conditions: Vec<WinCondition>,
  pub levels: Vec<Level>,
  pub players: HashSet<ObjectName>,
}

// Displaying things
// --------------------------------------------------------------------

impl Display for RuleDirection {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      RuleDirection::Down => write!(f, "DOWN"),
      RuleDirection::Up => write!(f, "UP"),
      RuleDirection::Right => write!(f, "RIGHT"),
      RuleDirection::Left => write!(f, "LEFT"),
    }
  }
}

impl Display for Qualifier {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      Qualifier::Stationary => write!(f, "STATIONARY"),
      Qualifier::Up => write!(f, "UP"),
      Qualifier::Down => write!(f, "DOWN"),
      Qualifier::Left => write!(f, "LEFT"),
      Qualifier::Right => write!(f, "RIGHT"),
      Qualifier::Action => write!(f, "ACTION"),
      Qualifier::RandomDir => write!(f, "RANDOMDIR"),
      Qualifier::Passthrough => write!(f, "PASSTHROUGH"),
    }
  }
}

impl Display for QualifiedObject {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{} ", self.qualifier)?;
    write!(f, "{}", self.object)
  }
}

impl Display for EntityMatcher {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      EntityMatcher::Object(object) => object.fmt(f),
      EntityMatcher::Property {
        property,
        qualifier,
        binder,
      } => write!(f, "{} {}<{}>", qualifier, property, binder),
      EntityMatcher::No(objects) => {
        let mut first = true;
        for object in objects {
          if !first {
            write!(f, " ")?
          }
          first = false;
          write!(f, "NO {}", object)?;
        }
        Ok(())
      }
    }
  }
}

impl Display for CellModifier {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match self {
      CellModifier::Object(object) => object.fmt(f),
      CellModifier::Property {
        qualifier,
        binder,
        property,
      } => write!(f, "{} {}<{}>", qualifier, property, binder),
      CellModifier::No(objects) => {
        let mut first = true;
        for object in objects {
          if !first {
            write!(f, " ")?;
          }
          first = false;
          write!(f, "NO {}", object)?;
        }
        Ok(())
      }
    }
  }
}

fn display_objects<A>(f: &mut Formatter, objects: &Objects<A>) -> fmt::Result
where
  A: Display,
{
  let mut first = true;
  for object in objects.iter() {
    if !first {
      write!(f, " ")?;
    }
    first = false;
    object.fmt(f)?;
  }
  Ok(())
}

fn display_cell_matcher_lhs<RHS>(
  f: &mut Formatter,
  cell_matcher: &CellMatcher<RHS>,
) -> fmt::Result {
  match cell_matcher {
    CellMatcher::Ellipsis => write!(f, "..."),
    CellMatcher::Objects(objects, _) => display_objects(f, objects),
  }
}

impl Display for RHSCell {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    match &self.random {
      None => (),
      Some(ref objects) => {
        for object in objects {
          write!(f, "RANDOM {} ", object)?;
        }
      }
    }
    display_objects(f, &self.normal)
  }
}

fn display_cell_matcher_rhs<RHS>(f: &mut Formatter, cell_matcher: &CellMatcher<RHS>) -> fmt::Result
where
  RHS: Display,
{
  match cell_matcher {
    CellMatcher::Ellipsis => write!(f, "..."),
    CellMatcher::Objects(_, rhs) => rhs.fmt(f),
  }
}

fn display_cell_matchers<'a, A: 'static, F, I>(
  f: &mut Formatter,
  display: F,
  cell_matchers: I,
) -> fmt::Result
where
  I: Iterator<Item = &'a A>,
  F: Fn(&mut Formatter, &A) -> fmt::Result,
{
  let mut first = true;
  for cell_matcher in cell_matchers {
    if !first {
      write!(f, " | ")?;
    }
    first = false;
    display(f, cell_matcher)?;
  }
  Ok(())
}

fn display_matchers<'a, A: 'static, F, I>(f: &mut Formatter, display: F, matchers: I) -> fmt::Result
where
  I: Iterator<Item = &'a A>,
  F: Fn(&mut Formatter, &A) -> fmt::Result,
{
  let mut first = true;
  for matcher in matchers {
    if !first {
      write!(f, " ")?;
    }
    write!(f, "[ ")?;
    first = false;
    display(f, matcher)?;
    write!(f, " ]")?;
  }
  Ok(())
}

impl Display for Rule {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    write!(f, "{}: {} ", self.line_number, self.direction)?;
    match &self.body {
      RuleBody::NoConsequence(matchers) => display_matchers(
        f,
        |f, cell_matchers| display_cell_matchers(f, display_cell_matcher_lhs, cell_matchers.iter()),
        matchers.iter(),
      )?,
      RuleBody::Normal(matchers) => {
        display_matchers(
          f,
          |f, cell_matchers| {
            display_cell_matchers(f, display_cell_matcher_lhs, cell_matchers.iter())
          },
          matchers.iter(),
        )?;
        write!(f, " -> ")?;
        display_matchers(
          f,
          |f, cell_matchers| {
            display_cell_matchers(f, display_cell_matcher_rhs, cell_matchers.iter())
          },
          matchers.iter(),
        )?;
      }
    }
    Ok(())
  }
}

impl Display for RuleGroup {
  fn fmt(&self, f: &mut Formatter) -> fmt::Result {
    for rule in self.rules.iter() {
      rule.fmt(f)?;
      writeln!(f)?;
    }
    Ok(())
  }
}
