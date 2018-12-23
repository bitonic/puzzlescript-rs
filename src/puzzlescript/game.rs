use crate::puzzlescript::ast;
use crate::puzzlescript::grid::*;
use im_rc::hashmap as im_hashmap;
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
    No,
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
pub enum LHSEntity {
    /// match a single object.
    Object(QualifiedObject),
    /// match any of these objects
    Property {
        property: PropertyName,
        qualifier: Qualifier,
        binder: PropertyBinder,
    },
}
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RHSEntity {
    Object(QualifiedObject),
    Property {
        /// if 'None', carries the movement of the property on the left
        qualifier: Qualifier,
        binder: PropertyBinder,
        property: PropertyName, // just for debugging / pretty printing
    },
    Random(Vec<Vec<ObjectName>>),
}

pub type Objects<Object> = Rc<Vec<Object>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CellMatcher<RHS> {
    Ellipsis,
    Objects(Objects<LHSEntity>, RHS),
}

/// the `|` separated cell matchers, e.g. in `[ A | B | C ]` `A`, `B`, and `C`
/// are the cell matchers.
pub type Matcher<RHS> = Vec<CellMatcher<RHS>>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum RuleBody {
    Normal(Vec<Matcher<Objects<RHSEntity>>>),
    NoConsequence(Vec<Matcher<()>>), // just used for sounds and commands
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Rule {
    pub line_number: usize,
    pub direction: RuleDirection,
    pub body: RuleBody,
    pub sounds: Vec<SoundFx>,
    pub command: Option<RuleCommand>,
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Eq, Clone)]
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
            Qualifier::No => write!(f, "NO"),
            Qualifier::RandomDir => write!(f, "RANDOMDIR"),
            Qualifier::Passthrough => write!(f, "PASSTHROUGH"),
        }
    }
}

impl Display for QualifiedObject {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        if self.qualifier != Qualifier::Passthrough {
            write!(f, "{} ", self.qualifier)?;
        }
        write!(f, "{}", self.object)
    }
}

impl Display for LHSEntity {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            LHSEntity::Object(object) => object.fmt(f),
            LHSEntity::Property {
                property,
                qualifier,
                binder,
            } => write!(f, "{} {}<{}>", qualifier, property, binder),
        }
    }
}

impl Display for RHSEntity {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            RHSEntity::Object(object) => object.fmt(f),
            RHSEntity::Random(_) => write!(f, "TODO RHS random"),
            RHSEntity::Property {
                qualifier,
                binder,
                property,
            } => write!(f, "{} {}<{}>", qualifier, property, binder),
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

fn display_cell_matcher_rhs<RHS>(
    f: &mut Formatter,
    cell_matcher: &CellMatcher<Objects<RHS>>,
) -> fmt::Result
where
    RHS: Display,
{
    match cell_matcher {
        CellMatcher::Ellipsis => write!(f, "..."),
        CellMatcher::Objects(_, objects) => display_objects(f, objects),
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
                |f, cell_matchers| {
                    display_cell_matchers(f, display_cell_matcher_lhs, cell_matchers.iter())
                },
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
