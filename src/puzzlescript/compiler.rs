// TODO remove panics, use error instead
// TODO handle `no` better, it's very different from the other qualifiers and
// we need to special cases in various places...
use crate::grid::*;
use crate::puzzlescript::ast;
use crate::puzzlescript::game::*;
use failure::Fail;
use im_rc::hashmap as im_hashmap;
use std::collections::hash_map;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone)]
enum LegendBody {
  Aggregate(HashSet<ObjectName>),
  Property(HashSet<ObjectName>),
}

struct CompileState<'a> {
  ast: &'a ast::Ast,
  /// Legends, but with all the names fully resolved to objects. note that,
  /// for convenience, we place all the objects as aliases of themselves in
  /// this map.
  legend: HashMap<ast::EntityName, LegendBody>,
  backgrounds: HashSet<ObjectName>,
  game: Game,
}

type ErrorMsg = String;

/*
fn cross_product<A, B>(xs: &Vec<A>, ys: &Vec<B>) -> Vec<(A, B)>
where
  A: Clone,
  B: Clone,
{
  let mut result = Vec::with_capacity(xs.len() * ys.len());

  for x in xs {
    for y in ys {
      result.push((x.clone(), y.clone()));
    }
  }

  result
}
*/

#[allow(clippy::ptr_arg)]
fn big_cross_product_op<A, B, F>(empty: B, op: F, xss: &Vec<Vec<A>>) -> Vec<B>
where
  A: Clone,
  B: Clone,
  F: Fn(&mut B, &A),
{
  let n = xss.iter().fold(1, |n, xs| n * xs.len());
  let mut result: Vec<B> = vec![empty; n];

  // in how much we need to fit each iteration of a single set of numbers
  let mut iteration_size = n;

  for xs in xss {
    let repeat_inner = iteration_size / xs.len();
    let repeat_outer = n / iteration_size;

    let mut row = 0;
    for _ in 0..repeat_outer {
      for x in xs.iter() {
        for _ in 0..repeat_inner {
          op(&mut result[row], x);
          row += 1;
        }
      }
    }

    iteration_size = repeat_inner;
  }

  result
}

#[allow(clippy::ptr_arg)]
fn big_cross_product<A>(xss: &Vec<Vec<A>>) -> Vec<Vec<A>>
where
  A: Clone,
{
  big_cross_product_op(Vec::new(), |row, el| row.push(el.clone()), xss)
}

fn rule_is_symmetric(_rule: &ast::Rule) -> bool {
  false // TODO
}

fn simplify_rule_direction(rule: &ast::Rule) -> Vec<ast::Rule> {
  let symmetric = rule_is_symmetric(rule);

  let with_direction_and_body = |direction, body| ast::Rule {
    direction: Some(direction),
    body,
    commands: rule.commands.clone(),
    line_number: rule.line_number,
  };

  let mut rules = Vec::new();

  match rule.direction {
    None => {
      rules.push(with_direction_and_body(
        ast::RuleDirection::Down,
        rule.body.clone(),
      ));
      rules.push(with_direction_and_body(
        ast::RuleDirection::Right,
        rule.body.clone(),
      ));
      if !symmetric {
        rules.push(with_direction_and_body(
          ast::RuleDirection::Up,
          rule.body.clone(),
        ));
        rules.push(with_direction_and_body(
          ast::RuleDirection::Left,
          rule.body.clone(),
        ));
      }
    }
    Some(direction) => match direction {
      ast::RuleDirection::Down => rules.push(rule.clone()),
      ast::RuleDirection::Right => rules.push(rule.clone()),
      ast::RuleDirection::Up => rules.push(rule.clone()),
      ast::RuleDirection::Left => rules.push(rule.clone()),
      ast::RuleDirection::Horizontal => {
        rules.push(with_direction_and_body(
          ast::RuleDirection::Right,
          rule.body.clone(),
        ));
        if !symmetric {
          rules.push(with_direction_and_body(
            ast::RuleDirection::Left,
            rule.body.clone(),
          ));
        }
      }
      ast::RuleDirection::Vertical => {
        rules.push(with_direction_and_body(
          ast::RuleDirection::Down,
          rule.body.clone(),
        ));
        if !symmetric {
          rules.push(with_direction_and_body(
            ast::RuleDirection::Up,
            rule.body.clone(),
          ));
        }
      }
    },
  }

  rules
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Ambiguity<A> {
  Ambiguous,
  Unique(A),
}
use self::Ambiguity::*;

#[derive(Debug, PartialEq, Eq, Clone)]
enum LHSEntityInfo {
  Property {
    original_qualifier: Option<ast::EntityQualifier>,
    derived_qualifier: Qualifier,
    binder: usize,
  },
  /// we treat aggregates and aliases the same
  Aggregate {
    original_qualifier: Option<ast::EntityQualifier>,
    derived_matchers: Objects<QualifiedObject>,
  },
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct LHSQualifierInfo {
  entity: ast::EntityName,
  entity_info: Rc<LHSEntityInfo>,
}

/// Store necessary information for the RHS. We go a bit overboard and store
/// quite a bit more information than necessary. If we encounter something
/// more than once, we consider that something "ambiguous".
#[derive(Clone, PartialEq, Eq, Debug)]
struct LHSInfo {
  /// All entities encountered on the LHS
  entities: im_hashmap::HashMap<ast::EntityName, Ambiguity<Rc<LHSEntityInfo>>>,
  /// All qualifiers encountered on the LHS
  qualifiers: im_hashmap::HashMap<ast::EntityQualifier, Ambiguity<LHSQualifierInfo>>,
}

impl LHSInfo {
  fn new() -> LHSInfo {
    LHSInfo {
      entities: im_hashmap::HashMap::new(),
      qualifiers: im_hashmap::HashMap::new(),
    }
  }

  fn insert_entity(&mut self, entity: &ast::EntityName, entity_info: &Rc<LHSEntityInfo>) {
    match self.entities.entry(entity.clone()) {
      im_hashmap::Entry::Vacant(vacant) => {
        vacant.insert(Unique(entity_info.clone()));
      }
      im_hashmap::Entry::Occupied(mut occupied) => {
        occupied.insert(Ambiguous);
      }
    }
  }

  fn insert_qualifier(
    &mut self,
    qualifier: ast::EntityQualifier,
    entity: &ast::EntityName,
    entity_info: &Rc<LHSEntityInfo>,
  ) {
    match self.qualifiers.entry(qualifier) {
      im_hashmap::Entry::Vacant(vacant) => {
        let info = LHSQualifierInfo {
          entity: entity.clone(),
          entity_info: entity_info.clone(),
        };
        vacant.insert(Unique(info));
      }
      im_hashmap::Entry::Occupied(mut occupied) => {
        occupied.insert(Ambiguous);
      }
    }
  }

  fn union(&mut self, other: &LHSInfo) {
    for (entity, mb_entity_info) in other.entities.iter() {
      match mb_entity_info {
        Ambiguous => {
          self.entities.insert(entity.clone(), Ambiguous);
        }
        Unique(entity_info) => self.insert_entity(entity, entity_info),
      }
    }

    for (qualifier, mb_entity) in other.qualifiers.iter() {
      match mb_entity {
        Ambiguous => {
          self.qualifiers.insert(*qualifier, Ambiguous);
        }
        Unique(qualifier_info) => self.insert_qualifier(
          *qualifier,
          &qualifier_info.entity,
          &qualifier_info.entity_info,
        ),
      }
    }
  }

  fn get_entity(&self, entity: &ast::EntityName) -> Ambiguity<Rc<LHSEntityInfo>> {
    match self.entities.get(entity) {
      None => Ambiguous,
      Some(x) => x.clone(),
    }
  }

  fn get_qualifier(&self, qualifier: ast::EntityQualifier) -> Ambiguity<LHSQualifierInfo> {
    match self.qualifiers.get(&qualifier) {
      None => Ambiguous,
      Some(x) => x.clone(),
    }
  }
}

#[allow(clippy::ptr_arg)]
fn big_cross_product_lhs_info<A>(xss: &Vec<Vec<(LHSInfo, A)>>) -> Vec<(LHSInfo, Vec<A>)>
where
  A: Clone,
{
  big_cross_product_op(
    (LHSInfo::new(), Vec::new()),
    |(lhs_info, match_objects), (new_lhs_info, match_object)| {
      lhs_info.union(&new_lhs_info);
      match_objects.push(match_object.clone());
    },
    xss,
  )
}

fn expand_qualifier(
  rule_direction: RuleDirection,
  mb_qualifier: Option<ast::EntityQualifier>,
) -> Vec<Qualifier> {
  use self::Qualifier::*;

  match mb_qualifier {
    None => vec![Passthrough],
    Some(qualifier) => match qualifier {
      ast::EntityQualifier::ArrowRight => vec![match rule_direction {
        RuleDirection::Down => Down,
        RuleDirection::Right => Right,
        RuleDirection::Up => Up,
        RuleDirection::Left => Left,
      }],
      ast::EntityQualifier::ArrowLeft => vec![match rule_direction {
        RuleDirection::Down => Up,
        RuleDirection::Up => Down,
        RuleDirection::Right => Left,
        RuleDirection::Left => Right,
      }],
      ast::EntityQualifier::ArrowUp => vec![match rule_direction {
        RuleDirection::Down => Left,
        RuleDirection::Up => Right,
        RuleDirection::Right => Up,
        RuleDirection::Left => Down,
      }],
      ast::EntityQualifier::ArrowDown => vec![match rule_direction {
        RuleDirection::Down => Right,
        RuleDirection::Up => Left,
        RuleDirection::Right => Down,
        RuleDirection::Left => Up,
      }],
      ast::EntityQualifier::Up => vec![Up],
      ast::EntityQualifier::Down => vec![Down],
      ast::EntityQualifier::Left => vec![Left],
      ast::EntityQualifier::Right => vec![Right],
      ast::EntityQualifier::Moving => vec![Up, Down, Left, Right, Action],
      ast::EntityQualifier::Orthogonal => vec![Up, Down, Left, Right, Action],
      ast::EntityQualifier::Stationary => vec![Stationary],
      ast::EntityQualifier::Action => vec![Action],
      ast::EntityQualifier::No => vec![No],
      ast::EntityQualifier::Random => panic!("Got random in LHS"),
      ast::EntityQualifier::RandomDir => vec![RandomDir],
      ast::EntityQualifier::Perpendicular => match rule_direction {
        RuleDirection::Down => vec![Left, Right],
        RuleDirection::Up => vec![Left, Right],
        RuleDirection::Right => vec![Up, Down],
        RuleDirection::Left => vec![Up, Down],
      },
      ast::EntityQualifier::Parallel => match rule_direction {
        RuleDirection::Down => vec![Up, Down],
        RuleDirection::Up => vec![Up, Down],
        RuleDirection::Right => vec![Left, Right],
        RuleDirection::Left => vec![Left, Right],
      },
    },
  }
}

fn resolve_qualifier(
  rule_direction: RuleDirection,
  mb_qualifier: Option<ast::EntityQualifier>,
) -> Option<Qualifier> {
  use self::Qualifier::*;

  match mb_qualifier {
    None => Some(Passthrough),
    Some(qualifier) => match qualifier {
      ast::EntityQualifier::ArrowRight => match rule_direction {
        RuleDirection::Down => Some(Down),
        RuleDirection::Up => Some(Up),
        RuleDirection::Right => Some(Right),
        RuleDirection::Left => Some(Left),
      },
      ast::EntityQualifier::ArrowLeft => match rule_direction {
        RuleDirection::Down => Some(Up),
        RuleDirection::Up => Some(Down),
        RuleDirection::Right => Some(Left),
        RuleDirection::Left => Some(Right),
      },
      ast::EntityQualifier::ArrowUp => match rule_direction {
        RuleDirection::Down => Some(Right),
        RuleDirection::Up => Some(Left),
        RuleDirection::Right => Some(Up),
        RuleDirection::Left => Some(Down),
      },
      ast::EntityQualifier::ArrowDown => match rule_direction {
        RuleDirection::Down => Some(Left),
        RuleDirection::Up => Some(Right),
        RuleDirection::Right => Some(Down),
        RuleDirection::Left => Some(Up),
      },
      ast::EntityQualifier::Up => Some(Up),
      ast::EntityQualifier::Down => Some(Down),
      ast::EntityQualifier::Left => Some(Left),
      ast::EntityQualifier::Right => Some(Right),
      ast::EntityQualifier::Moving => None,
      ast::EntityQualifier::Orthogonal => None,
      ast::EntityQualifier::Stationary => Some(Stationary),
      ast::EntityQualifier::Action => Some(Action),
      ast::EntityQualifier::No => Some(No),
      ast::EntityQualifier::RandomDir => Some(RandomDir),
      ast::EntityQualifier::Random => None,
      ast::EntityQualifier::Perpendicular => None,
      ast::EntityQualifier::Parallel => None,
    },
  }
}

impl<'a> CompileState<'a> {
  fn process_legends(&mut self) -> Result<(), ErrorMsg> {
    for legend_name in self.ast.entities.legend_order.iter() {
      match &self.ast.entities.legend[legend_name] {
        // remember, for comfort we store the objects as aliases of themselves
        ast::LegendBody::Alias(target) => {
          self
            .legend
            .insert(legend_name.clone(), self.legend[target].clone());
          match self.legend[target] {
            LegendBody::Aggregate(_) => (),
            // if it's an alias to property, add this to the properties as well
            LegendBody::Property(_) => {
              self
                .game
                .properties
                .insert(legend_name.clone(), self.game.properties[target].clone());
            }
          }
        }

        // if it's an aggregate, check that it only refers to objects and other
        // aggregates.
        ast::LegendBody::Aggregate(immediate_targets) => {
          let mut targets = HashSet::new();
          for target in immediate_targets {
            match &self.legend[target] {
              LegendBody::Aggregate(objects) => {
                for object in objects {
                  targets.insert(object.clone());
                }
              }
              LegendBody::Property(_) => {
                return Err(format!(
                  "Aggregate {} contains a reference to property {}",
                  legend_name, target
                ));
              }
            }
          }
          self
            .legend
            .insert(legend_name.clone(), LegendBody::Aggregate(targets));
        }

        // if it's a property, check that it only refers to objects and other
        // properties
        ast::LegendBody::Property(immediate_targets) => {
          let mut targets = HashSet::new();
          for target in immediate_targets {
            match &self.legend[target] {
              LegendBody::Property(objects) => {
                for object in objects {
                  targets.insert(object.clone());
                }
              }
              LegendBody::Aggregate(objects) if objects.len() == 1 => {
                targets.insert(objects.iter().next().unwrap().clone());
              }
              LegendBody::Aggregate(_) => {
                return Err(format!(
                  "Property {} contains a reference to aggregate {}",
                  legend_name, target
                ));
              }
            }
          }
          self
            .game
            .properties
            .insert(legend_name.clone(), targets.clone());
          self
            .legend
            .insert(legend_name.clone(), LegendBody::Property(targets));
        }
      }
    }
    Ok(())
  }

  fn process_collision_layers(&mut self) -> Result<(), ErrorMsg> {
    let mut considered_objects: HashSet<&ObjectName> = HashSet::new();

    let mut collision_layers = Vec::new();
    let mut background_found = false;

    'running: for collision_layer in self.ast.collision_layers.iter() {
      let mut objects: Vec<&ObjectName> = Vec::new();
      for entity in collision_layer.iter() {
        if entity == &Rc::from("background") {
          if background_found {
            return Err("Multiple 'background' collision layers!".to_string());
          }
          if collision_layer.len() == 1 {
            collision_layers.push(CollisionLayer::Background);
            background_found = true;
            continue 'running;
          } else {
            return Err(format!(
              "Collision layer 'background' must appear on its own, but I found it along {:?}",
              collision_layers
            ));
          }
        } else {
          match &self.legend[entity] {
            LegendBody::Aggregate(aggregate_objects) if aggregate_objects.len() == 1 => {
              objects.push(aggregate_objects.iter().next().unwrap())
            }
            LegendBody::Aggregate(_) => {
              return Err(format!("Aggregate {} appears in collision layer", entity));
            }
            LegendBody::Property(property_objects) => {
              for object in property_objects {
                objects.push(object)
              }
            }
          }
        }
      }
      // check that we haven't met things _at the end_, since objects can appear
      // multiple times in the same layer.
      //
      // also, dedup the vector as you go.
      let mut seen_objects: HashSet<&ObjectName> = HashSet::new();
      let mut deduped_objects = Vec::new();
      for object in objects.into_iter() {
        if !seen_objects.contains(&object) {
          if considered_objects.contains(&object) {
            return Err(format!(
              "Object {} appears twice in collision layers!",
              object
            ));
          }
          considered_objects.insert(object);
          seen_objects.insert(object);
          deduped_objects.push(object.clone());
        }
      }
      collision_layers.push(CollisionLayer::Normal(deduped_objects))
    }

    if !background_found {
      return Err("Could not find 'background' in collision layers".to_string())?;
    }

    self.game.collision_layers = collision_layers;

    Ok(())
  }

  fn expand_lhs_cell_entity(
    &self,
    property_counter: &mut usize,
    rule_direction: RuleDirection,
    match_entity: &ast::MatchEntity,
  ) -> Vec<(LHSInfo, Objects<LHSEntity>)> {
    let with_qualifier = |lhs_info: &mut LHSInfo,
                          mb_qualifier: Option<ast::EntityQualifier>,
                          entity_info: &Rc<LHSEntityInfo>| {
      mb_qualifier.into_iter().for_each(|qualifier| {
        lhs_info.insert_qualifier(qualifier, &match_entity.entity, entity_info)
      });
    };
    match &self.legend[&match_entity.entity] {
      LegendBody::Aggregate(objects) => expand_qualifier(rule_direction, match_entity.qualifier)
        .into_iter()
        .map(|qualifier| {
          let match_objects: Objects<QualifiedObject> = Rc::new(
            objects
              .iter()
              .map(|object| QualifiedObject {
                qualifier,
                object: object.clone(),
              })
              .collect(),
          );
          let entity_info = Rc::new(LHSEntityInfo::Aggregate {
            original_qualifier: match_entity.qualifier,
            derived_matchers: match_objects.clone(),
          });
          let mut lhs_info = LHSInfo::new();
          // if the qualifier is No, we do not "remember it", since the match
          // is negative
          if match_entity.qualifier != Some(ast::EntityQualifier::No) {
            with_qualifier(&mut lhs_info, match_entity.qualifier, &entity_info);
            lhs_info.insert_entity(&match_entity.entity, &entity_info);
          }
          (
            lhs_info,
            Rc::new(
              match_objects
                .iter()
                .cloned()
                .map(LHSEntity::Object)
                .collect(),
            ),
          )
        })
        .collect(),
      LegendBody::Property(_) => {
        let mut result = Vec::new();
        for qualifier in expand_qualifier(rule_direction, match_entity.qualifier) {
          let mut lhs_info = LHSInfo::new();
          let binder = *property_counter;
          *property_counter += 1;
          let entity_info = Rc::new(LHSEntityInfo::Property {
            original_qualifier: match_entity.qualifier,
            derived_qualifier: qualifier,
            binder,
          });
          // if the qualifier is No, we do not "remember it", since the match
          // is negative
          if match_entity.qualifier != Some(ast::EntityQualifier::No) {
            lhs_info.insert_entity(&match_entity.entity, &entity_info);
            with_qualifier(&mut lhs_info, match_entity.qualifier, &entity_info);
          }
          result.push((
            lhs_info,
            Rc::new(vec![LHSEntity::Property {
              qualifier,
              binder,
              property: match_entity.entity.clone(),
            }]),
          ));
        }
        result
      }
    }
  }

  fn expand_lhs_cell_entities(
    &self,
    property_counter: &mut usize,
    rule_direction: RuleDirection,
    match_entities: &[ast::MatchEntity],
  ) -> Vec<(LHSInfo, Vec<LHSEntity>)> {
    big_cross_product_op(
      (LHSInfo::new(), Vec::new()),
      |(lhs_info, match_objects), (new_lhs_info, new_match_objects)| {
        lhs_info.union(&new_lhs_info);
        for match_object in new_match_objects.iter() {
          match_objects.push(match_object.clone());
        }
      },
      &match_entities
        .iter()
        .map(|match_entity| {
          self.expand_lhs_cell_entity(property_counter, rule_direction, match_entity)
        })
        .collect(),
    )
  }

  fn expand_no_consequence_rule_body(
    &self,
    property_counter: &mut usize,
    rule_direction: RuleDirection,
    matchers: &[ast::Matcher<()>],
  ) -> Vec<Vec<Matcher<()>>> {
    big_cross_product(
      &matchers
        .iter()
        .map(|matcher| {
          big_cross_product(
            &matcher
              .iter()
              .map(|cell_matcher| match cell_matcher {
                ast::CellMatcher::Ellipsis => vec![CellMatcher::Ellipsis],
                ast::CellMatcher::Entities(match_entities, ()) => self
                  .expand_lhs_cell_entities(property_counter, rule_direction, match_entities)
                  .into_iter()
                  .map(|x| CellMatcher::Objects(Rc::new(x.1), ()))
                  .collect(),
              })
              .collect(),
          )
        })
        .collect(),
    )
  }

  fn process_rhs_entities(
    &self,
    rule_direction: RuleDirection,
    overall_lhs_info: &LHSInfo,
    cell_lhs_info: &LHSInfo,
    entities: &[ast::MatchEntity],
  ) -> Objects<RHSEntity> {
    Rc::new(
      entities
        .iter()
        .flat_map(|match_entity| {
          if match_entity.qualifier == Some(ast::EntityQualifier::Random) {
            vec![match &self.legend[&match_entity.entity] {
              LegendBody::Aggregate(objects) if objects.len() == 1 => {
                RHSEntity::Random(objects.clone())
              }
              LegendBody::Aggregate(_) => {
                panic!("TODO random compound aggregate {:?}", match_entity.entity)
              }
              LegendBody::Property(objects) => {
                RHSEntity::Random(objects.clone())
              }
            }]
          } else {
            // when dealing with an aggregate...
            match &self.legend[&match_entity.entity] {
              LegendBody::Aggregate(objects) => {
                // first see if we can resolve the qualifier immediately
                match resolve_qualifier(rule_direction, match_entity.qualifier) {
                  // in which case just use it for all the objects -- with one
                  // caveat: passthroughs stay passthroughs only if we do not
                  // have the same object on the LHS, or if the object on the LHS
                  // is a passthrough too. otherwise they are stationary.
                  //
                  // the reasoning is that in a rule like
                  //
                  // ```text
                  // [ > A ] -> [ A ]
                  // ```
                  //
                  // we want the A on the right to change to stationary, not to
                  // carry over the `>`.
                  Some(qualifier) => {
                    let actual_qualifier = if qualifier != Qualifier::Passthrough {
                      qualifier
                    } else {
                      match cell_lhs_info.get_entity(&match_entity.entity) {
                        Ambiguous => Qualifier::Passthrough,
                        Unique(lhs_info) => match &*lhs_info {
                          LHSEntityInfo::Property{..} => panic!("Got property LHS info from aggregate"),
                          LHSEntityInfo::Aggregate{original_qualifier, ..} => match original_qualifier {
                            None => Qualifier::Passthrough,
                            Some(_) => Qualifier::Stationary,
                          }
                        }
                      }
                    };
                    objects
                      .iter()
                      .map(|object| {
                        RHSEntity::Object(QualifiedObject {
                          object: object.clone(),
                          qualifier: actual_qualifier,
                        })
                      })
                      .collect()
                  },
                  // otherwise, if the qualifier is ambiguous, we need to find it
                  // on the LHS somewhere
                  None => {
                    let with_lhs_info = |lhs_info: &LHSEntityInfo| match lhs_info {
                      LHSEntityInfo::Property { .. } => {
                        panic!("Got property LHS info from aggregate")
                      }
                      LHSEntityInfo::Aggregate {
                        original_qualifier,
                        derived_matchers,
                      } => {
                        if original_qualifier == &match_entity.qualifier {
                          Some(
                            derived_matchers
                              .iter()
                              .map(|match_object| {
                                RHSEntity::Object(QualifiedObject {
                                  object: match_object.object.clone(),
                                  qualifier: match_object.qualifier,
                                })
                              })
                              .collect(),
                          )
                        } else {
                          None
                        }
                      }
                    };
                    let mb_from_cell = match cell_lhs_info.get_entity(&match_entity.entity) {
                      Unique(lhs_info) => with_lhs_info(&lhs_info),
                      Ambiguous => None,
                    };
                    let mb_from_overall = || match overall_lhs_info.get_entity(&match_entity.entity)
                    {
                      Unique(lhs_info) => with_lhs_info(&lhs_info),
                      Ambiguous => None,
                    };
                    let mb_from_qualifier =
                      || match overall_lhs_info.get_qualifier(match_entity.qualifier.unwrap()) {
                        Unique(qualifier_info) => {
                          match *qualifier_info.entity_info {
                            LHSEntityInfo::Property {
                              ref derived_qualifier,
                              ..
                            } => Some(
                              objects
                                .iter()
                                .map(|object| {
                                  RHSEntity::Object(QualifiedObject {
                                    object: object.clone(),
                                    qualifier: *derived_qualifier,
                                  })
                                })
                                .collect(),
                            ),
                            LHSEntityInfo::Aggregate {
                              ref derived_matchers,
                              ..
                            } =>
                            // if there is only one object on the left, then just assign its qualifier
                            // to every object on the right
                            {
                              if derived_matchers.len() == 1 {
                                let qualifier = derived_matchers[0].qualifier;
                                Some(
                                  objects
                                    .iter()
                                    .map(|object| {
                                      RHSEntity::Object(QualifiedObject {
                                        object: object.clone(),
                                        qualifier,
                                      })
                                    })
                                    .collect(),
                                )
                              } else {
                                // otherwise, pair things on the right with things on the left.
                                let mut objects_map: HashMap<&ObjectName, &QualifiedObject> =
                                  HashMap::new();
                                for matcher in derived_matchers.iter() {
                                  objects_map.insert(&matcher.object, matcher);
                                }
                                let mut matchers = Vec::new();
                                for object in objects {
                                  match objects_map.get(object) {
                                    None => return None,
                                    Some(match_object) => {
                                      matchers.push(RHSEntity::Object(QualifiedObject {
                                        object: match_object.object.clone(),
                                        qualifier: match_object.qualifier,
                                      }))
                                    }
                                  }
                                }
                                Some(matchers)
                              }
                            }
                          }
                        }
                        Ambiguous => None,
                      };
                    mb_from_cell
                      .or_else(mb_from_overall)
                      .or_else(mb_from_qualifier)
                      .unwrap_or_else(|| {
                        panic!(
                          "Could not disambiguate {:?} for entity {:?} on RHS {:?}",
                          match_entity.qualifier, match_entity.entity, overall_lhs_info
                        )
                      })
                  }
                }
              }
              LegendBody::Property(_) => {
                // otherwise, we need to find out which property on the
                // LHS this property refers to.
                //
                // First define a utility to finish up once we have found
                // something on the LHS:
                let with_lhs_info = |same_cell: bool, lhs_info: &LHSEntityInfo| {
                  match lhs_info {
                    LHSEntityInfo::Aggregate { .. } => panic!("Got LHS aggregate for property"),
                    LHSEntityInfo::Property {
                      original_qualifier,
                      derived_qualifier,
                      binder,
                    } => {
                      let with_qualifier = |qualifier| {
                        vec![RHSEntity::Property {
                          qualifier,
                          binder: *binder,
                          property: match_entity.entity.clone(),
                        }]
                      };
                      let give_up = ||
                        panic!(
                          "Could not resolve ambiguous qualifier {:?} for entity {:?}",
                          match_entity.qualifier,
                          match_entity.entity);
                      match resolve_qualifier(rule_direction, match_entity.qualifier) {
                        // if we can resolve a qualifier immediately, use that one -- but
                        // as explained above, treat passthrough specially
                        Some(qualifier) =>
                          with_qualifier(
                            if
                              same_cell &&
                              qualifier == Qualifier::Passthrough &&
                              *derived_qualifier != Qualifier::Passthrough
                            {
                              Qualifier::Stationary
                            } else {
                              qualifier
                            }
                          ),
                        None => {
                          // otherwise, first try use the one from the LHSInfo
                          if original_qualifier == &match_entity.qualifier {
                            with_qualifier(*derived_qualifier)
                          } else {
                            // if that fails too, try to get it from the qualifier map
                            match overall_lhs_info.get_qualifier(match_entity.qualifier.unwrap()) {
                              Unique(qualifier_info) => {
                                match *qualifier_info.entity_info {
                                  // if the qualifier is attached to a property, just use it
                                  LHSEntityInfo::Property { ref derived_qualifier, .. } =>
                                    with_qualifier(*derived_qualifier),
                                  LHSEntityInfo::Aggregate { ref derived_matchers, .. } =>
                                    // if it's attached to an aggregate, only accept if it's just one object
                                    if derived_matchers.len() == 1 {
                                      with_qualifier(derived_matchers[0].qualifier)
                                    } else {
                                      give_up()
                                    }
                                }
                              }
                              Ambiguous =>
                                give_up(),
                            }
                          }
                        }
                      }
                    }
                  }
                };
                // Then, first check if we have a matching property in the same cell:
                match cell_lhs_info.get_entity(&match_entity.entity) {
                  Unique(lhs_info) => with_lhs_info(true, &lhs_info),
                  Ambiguous => {
                    // Failing that, see if we have it anywhere else in the rule:
                    match overall_lhs_info.get_entity(&match_entity.entity) {
                      Unique(lhs_info) => with_lhs_info(false, &lhs_info),
                      // otherwise we're done
                      Ambiguous => panic!(
                        "Could not find property matching {:?} anywhere on the LHS (cell or overall)",
                        match_entity.entity
                      ),
                    }
                  }
                }
              }
            }
          }
        })
        .collect(),
    )
  }

  fn expand_normal_rule_body(
    &self,
    property_counter: &mut usize,
    rule_direction: RuleDirection,
    matchers: &[ast::Matcher<Vec<ast::MatchEntity>>],
  ) -> Vec<Vec<Matcher<Objects<RHSEntity>>>> {
    // first collect all the LHS properties
    let with_lhs_properties = big_cross_product_lhs_info(
      &matchers
        .iter()
        .map(|matcher| {
          big_cross_product_lhs_info(
            &matcher
              .iter()
              .map(|cell_matcher| match cell_matcher {
                ast::CellMatcher::Ellipsis => vec![(LHSInfo::new(), CellMatcher::Ellipsis)],
                ast::CellMatcher::Entities(lhs_entities, rhs_entities) => self
                  .expand_lhs_cell_entities(property_counter, rule_direction, lhs_entities)
                  .into_iter()
                  .map(|x| {
                    (
                      x.0.clone(),
                      CellMatcher::Objects(Rc::new(x.1), (x.0, rhs_entities)),
                    )
                  })
                  .collect(),
              })
              .collect(),
          )
        })
        .collect(),
    );
    // then process RHS
    with_lhs_properties
      .into_iter()
      .map(|(overall_lhs_info, matchers)| {
        matchers
          .into_iter()
          .map(|matcher| {
            matcher
              .into_iter()
              .map(|cell_matcher| match cell_matcher {
                CellMatcher::Ellipsis => CellMatcher::Ellipsis,
                CellMatcher::Objects(lhs_entities, (cell_lhs_info, rhs_entities)) => {
                  CellMatcher::Objects(
                    lhs_entities,
                    self.process_rhs_entities(
                      rule_direction,
                      &overall_lhs_info,
                      &cell_lhs_info,
                      rhs_entities,
                    ),
                  )
                }
              })
              .collect()
          })
          .collect()
      })
      .collect()
  }

  fn expand_rule_entities(&self, rule: &ast::Rule) -> Vec<Rule> {
    let direction = match rule.direction {
      None => panic!("Unexpected None in rule direction"),
      Some(ast::RuleDirection::Down) => RuleDirection::Down,
      Some(ast::RuleDirection::Up) => RuleDirection::Up,
      Some(ast::RuleDirection::Right) => RuleDirection::Right,
      Some(ast::RuleDirection::Left) => RuleDirection::Left,
      Some(dir) => panic!("Unexpected direction: {:?}", dir),
    };
    let with_body = |body| Rule {
      direction,
      body,
      commands: rule.commands.clone(),
      line_number: rule.line_number,
    };
    let property_counter = &mut 0;
    match &rule.body {
      ast::RuleBody::Normal(matchers) => self
        .expand_normal_rule_body(property_counter, direction, matchers)
        .into_iter()
        .map(|new_matchers| with_body(RuleBody::Normal(new_matchers)))
        .collect(),
      ast::RuleBody::NoConsequence(matchers) => self
        .expand_no_consequence_rule_body(property_counter, direction, matchers)
        .into_iter()
        .map(|new_matchers| with_body(RuleBody::NoConsequence(new_matchers)))
        .collect(),
    }
  }

  fn process_rule(&self, rule0: &ast::Rule) -> Vec<Rule> {
    // reduce all directions to `Down` and `Right`.
    let rules1: Vec<ast::Rule> = simplify_rule_direction(rule0);
    // expand the entities into simple objects
    rules1
      .iter()
      .flat_map(|rule| self.expand_rule_entities(rule))
      .collect()
  }

  fn process_rules(&mut self) -> Result<(), ErrorMsg> {
    for rule_group in self.ast.rules.iter() {
      let mut rules = Vec::new();
      for rule in rule_group.rules.iter() {
        rules.append(&mut self.process_rule(rule));
      }
      let game_rule_group = RuleGroup {
        rules,
        random: rule_group.random,
      };
      if rule_group.late {
        self.game.late_rules.push(game_rule_group);
      } else {
        self.game.rules.push(game_rule_group);
      }
    }
    Ok(())
  }

  fn process_win_conditions(&mut self) -> Result<(), ErrorMsg> {
    let legend = &self.legend;
    let win_conditions = &mut self.game.win_conditions;
    let assert_object_or_property =
      |entity: &ast::EntityName| -> Result<HashSet<ObjectName>, ErrorMsg> {
        match legend[entity] {
          LegendBody::Aggregate(ref objects) if objects.len() == 1 => {
            Ok(objects.iter().take(1).cloned().collect())
          }
          LegendBody::Aggregate(_) => Err(format!(
            "Can only have properties or objects in win conditions, but got aggregate {}",
            entity
          )),
          LegendBody::Property(ref objects) => Ok(objects.clone()),
        }
      };

    for win_condition in self.ast.win_conditions.iter() {
      match win_condition {
        ast::WinCondition::No(entity) => {
          win_conditions.push(WinCondition::No(assert_object_or_property(entity)?));
        }
        ast::WinCondition::NoOn(entity1, entity2) => {
          win_conditions.push(WinCondition::NoOn(
            assert_object_or_property(entity1)?,
            assert_object_or_property(entity2)?,
          ));
        }
        ast::WinCondition::AllOn(entity1, entity2) => {
          win_conditions.push(WinCondition::AllOn(
            assert_object_or_property(entity1)?,
            assert_object_or_property(entity2)?,
          ));
        }
        ast::WinCondition::Some(entity) => {
          win_conditions.push(WinCondition::Some(assert_object_or_property(entity)?));
        }
        ast::WinCondition::SomeOn(entity1, entity2) => {
          win_conditions.push(WinCondition::SomeOn(
            assert_object_or_property(entity1)?,
            assert_object_or_property(entity2)?,
          ));
        }
      }
    }

    Ok(())
  }

  fn process_levels(&mut self) -> Result<(), ErrorMsg> {
    for level in self.ast.levels.iter() {
      match level {
        ast::Level::Message(message) => self.game.levels.push(Level::Message(message.clone())),
        ast::Level::Stage(ast_stage) => {
          let mut stage: Stage = Grid::new(
            ast_stage.nrows(),
            ast_stage.ncols(),
            &im_hashmap::HashMap::new(),
          );
          let mut mb_background: Option<ObjectName> = None;

          for row in 0..ast_stage.nrows() {
            for col in 0..ast_stage.ncols() {
              let entity = &ast_stage[(row, col)];
              match self.legend[entity] {
                LegendBody::Aggregate(ref objects) => {
                  stage[(row, col)] = objects
                    .iter()
                    .cloned()
                    .filter(|object| {
                      let is_background = self.backgrounds.contains(object);
                      if is_background {
                        match mb_background {
                          None => mb_background = Some(object.clone()),
                          // according to stephen the first one wins, and that's it
                          Some(_) => (),
                        }
                        false
                      } else {
                        true
                      }
                    })
                    .map(|object| (object.clone(), Movement::Stationary))
                    .collect()
                }
                LegendBody::Property(_) => Err(format!(
                  "Cannot use property {} in level definition",
                  entity
                ))?,
              }
            }
          }

          self.game.levels.push(Level::Stage {
            stage,
            background: mb_background,
          });
        }
      }
    }

    Ok(())
  }

  fn run(&mut self) -> Result<(), ErrorMsg> {
    self.process_legends()?;

    // look up player
    self.game.players = match self.legend.get("player") {
      None => Err("Game has no player!".to_string())?,
      Some(legend) => match legend {
        LegendBody::Aggregate(objects) => {
          if objects.len() == 1 {
            objects.iter().take(1).cloned().collect()
          } else {
            Err("An aggregate can't be the player!")?
          }
        }
        LegendBody::Property(objects) => objects.clone(),
      },
    };

    // extract background
    let background_name: ast::EntityName = Rc::from("background");
    let backgrounds = match self.legend.entry(background_name.clone()) {
      hash_map::Entry::Occupied(occupied) => {
        let (_, legend_body) = occupied.remove_entry();
        match legend_body {
          LegendBody::Aggregate(ref objects) if objects.len() == 1 => {
            let object = objects.iter().next().unwrap().clone();
            vec![object].into_iter().collect()
          }
          LegendBody::Aggregate(_) => {
            return Err("Background can't be aggregate!".to_string());
          }
          LegendBody::Property(entities) => entities,
        }
      }
      hash_map::Entry::Vacant(_) => HashSet::new(),
    };
    self.backgrounds = backgrounds;

    self.process_collision_layers()?;

    self.process_rules()?;

    self.process_win_conditions()?;

    self.process_levels()?;

    Ok(())
  }
}

#[derive(Debug, Fail)]
#[fail(display = "Compilation error: {}", msg)]
pub struct CompileError {
  pub msg: String,
}

pub fn compile(ast: &ast::Ast) -> Result<Game, CompileError> {
  let game = Game {
    prelude: ast.prelude.clone(),
    objects: ast.entities.objects.clone(),
    properties: HashMap::new(),
    collision_layers: Vec::new(),
    rules: Vec::new(),
    late_rules: Vec::new(),
    win_conditions: Vec::new(),
    levels: Vec::new(),
    players: HashSet::new(),
  };
  let mut legend = HashMap::new();
  for object_name in ast.entities.objects.keys() {
    legend.insert(
      object_name.clone(),
      LegendBody::Aggregate(vec![object_name.clone()].into_iter().collect()),
    );
  }
  let mut st = CompileState {
    ast,
    game,
    legend,
    backgrounds: HashSet::new(),
  };
  match st.run() {
    Ok(()) => Ok(st.game),
    Err(msg) => Err(CompileError { msg }),
  }
}

#[cfg(test)]
mod tests {
  use crate::puzzlescript::compiler::big_cross_product;
  use crate::puzzlescript::compiler::*;
  use crate::puzzlescript::parser;

  #[test]
  fn tutorial_eyeball() {
    let file = include_str!("../../puzzlescripts/tutorial/eyeball.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn tutorial_random_robots() {
    let file = include_str!("../../puzzlescripts/tutorial/random_robots.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn tutorial_random_robots_spawner() {
    let file = include_str!("../../puzzlescripts/tutorial/random_robots_spawner.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn intermediate_constellation_z() {
    let file = include_str!("../../puzzlescripts/intermediate/constellation_z.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn advanced_2d_whale_world() {
    let file = include_str!("../../puzzlescripts/advanced/2d_whale_world.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn third_party_w3rds() {
    let file = include_str!("../../puzzlescripts/third_party/w3rds.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn third_party_kraken() {
    let file = include_str!("../../puzzlescripts/third_party/kraken.pzl");
    let ast = parser::parse(file).unwrap();
    compile(&ast).unwrap();
  }

  #[test]
  fn big_cross_product_empty() {
    let rhs: Vec<Vec<i64>> = vec![vec![]];
    assert_eq!(big_cross_product::<i64>(&vec![]), rhs);
  }

  #[test]
  fn big_cross_product_2_cols() {
    let lhs: Vec<Vec<i64>> = big_cross_product(&vec![vec![1, 2], vec![3, 4, 5]]);
    let rhs: Vec<Vec<i64>> = vec![
      vec![1, 3],
      vec![1, 4],
      vec![1, 5],
      vec![2, 3],
      vec![2, 4],
      vec![2, 5],
    ];
    assert_eq!(lhs, rhs);
  }

  #[test]
  fn big_cross_product_3_cols() {
    let lhs: Vec<Vec<i64>> = big_cross_product(&vec![vec![1, 2], vec![0], vec![3, 4, 5]]);
    let rhs: Vec<Vec<i64>> = vec![
      vec![1, 0, 3],
      vec![1, 0, 4],
      vec![1, 0, 5],
      vec![2, 0, 3],
      vec![2, 0, 4],
      vec![2, 0, 5],
    ];
    assert_eq!(lhs, rhs);
  }

  #[test]
  fn expand_lhs_qualifier_none() {
    assert_eq!(
      expand_qualifier(RuleDirection::Down, None),
      vec![Qualifier::Passthrough]
    );
  }

  #[test]
  fn expand_lhs_qualifier_foward() {
    assert_eq!(
      expand_qualifier(RuleDirection::Down, Some(ast::EntityQualifier::ArrowRight)),
      vec![Qualifier::Down]
    );
    assert_eq!(
      expand_qualifier(RuleDirection::Right, Some(ast::EntityQualifier::ArrowRight)),
      vec![Qualifier::Right]
    );
  }

  #[test]
  fn expand_lhs_qualifier_moving() {
    assert_eq!(
      expand_qualifier(RuleDirection::Down, Some(ast::EntityQualifier::Moving)).sort(),
      vec![
        Qualifier::Down,
        Qualifier::Up,
        Qualifier::Right,
        Qualifier::Left
      ]
      .sort()
    );
  }

  #[test]
  fn cross_product_of_vectors() {
    assert_eq!(
      &big_cross_product(&vec![vec![vec![1 as i32, 2]], vec![vec![3]]]),
      &vec![vec![vec![1 as i32, 2], vec![3]]]
    );
  }
}
