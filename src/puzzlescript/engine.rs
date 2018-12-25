use crate::grid;
use crate::puzzlescript::game::*;
use im_rc::hashmap as im_hashmap;
use std::collections::{HashMap, HashSet};

pub type PropertyBindings = HashMap<PropertyBinder, ObjectName>;

fn match_qualifier(qualifier: Qualifier, movement: Movement) -> bool {
  match (qualifier, movement) {
    (Qualifier::RandomDir, _) => panic!("TODO Qualifier::RandomDir"),
    (Qualifier::Passthrough, _) => true,
    (Qualifier::Stationary, Movement::Stationary) => true,
    (Qualifier::Up, Movement::Up) => true,
    (Qualifier::Down, Movement::Down) => true,
    (Qualifier::Left, Movement::Left) => true,
    (Qualifier::Right, Movement::Right) => true,
    (Qualifier::Action, Movement::Action) => true,
    _ => false,
  }
}

/// `Some` if it could match, with the bound properties (if any).
fn match_cell(
  properties: &HashMap<PropertyName, HashSet<PropertyName>>,
  cell: &im_hashmap::HashMap<ObjectName, Movement>,
  matcher: &Objects<LHSEntity>,
) -> Option<PropertyBindings> {
  let mut bindings: PropertyBindings = HashMap::new();

  'running: for lhs_entity in matcher.iter() {
    match lhs_entity {
      LHSEntity::Object(object) => match cell.get(&object.object) {
        None => {
          if object.qualifier != Qualifier::No {
            return None;
          }
        }
        Some(movement) => {
          if !match_qualifier(object.qualifier, *movement) {
            return None;
          }
        }
      },
      LHSEntity::Property {
        property,
        qualifier,
        binder,
      } => {
        if *qualifier == Qualifier::No {
          for property_object in properties[property].iter() {
            if cell.contains_key(property_object) {
              return None;
            }
          }
        } else {
          for property_object in properties[property].iter() {
            match cell.get(property_object) {
              None => (),
              Some(movement) => {
                if match_qualifier(*qualifier, *movement) {
                  bindings.insert(*binder, property_object.clone());
                  continue 'running; // keep going, we have a match
                }
              }
            }
          }
          return None; // we found nothing
        }
      }
    }
  }

  Some(bindings)
}

/// `None` if 'Qualifier::Passthrough'
fn qualifier_to_movement(qualifier: Qualifier) -> Option<Movement> {
  match qualifier {
    Qualifier::Stationary => Some(Movement::Stationary),
    Qualifier::Up => Some(Movement::Up),
    Qualifier::Down => Some(Movement::Down),
    Qualifier::Left => Some(Movement::Left),
    Qualifier::Right => Some(Movement::Right),
    Qualifier::Action => Some(Movement::Action),
    Qualifier::Passthrough => None,
    Qualifier::No => panic!("Got 'No' in qualifier_to_movement"),
    Qualifier::RandomDir => panic!("TODO 'RandomDir' in qualifier_to_movement"),
  }
}

struct MatchedCells<'a, RHS> {
  cell_matchers: &'a [CellMatcher<RHS>],
  /// in what slice of the stage we matched
  slice_desc: grid::SliceDesc,
  /// at which points of the slice each non-ellipsis matcher matched
  matches: Vec<usize>,
}

impl<'a> MatchedCells<'a, Objects<RHSEntity>> {
  /// returns if anything changed
  pub fn apply(
    &self,
    collision_layers: &CollisionLayersInfo,
    bindings: &PropertyBindings,
    stage: &mut Stage,
  ) -> bool {
    let mut something_changed = false;
    let mut matches_iter = self.matches.iter();
    let mut cells = grid::SliceMut {
      grid: stage,
      desc: self.slice_desc,
    };
    for matcher in self.cell_matchers {
      match matcher {
        CellMatcher::Ellipsis => (),
        CellMatcher::Objects(ref lhs_entities, ref rhs_entities) => {
          let cell_ix = *matches_iter.next().unwrap();
          let old_cell = cells[cell_ix].clone();
          // first remove all the matched LHS entities -- the ones that survive
          // will be added back by the RHS
          for lhs_entity in lhs_entities.iter() {
            match lhs_entity {
              LHSEntity::Object(ref object) => {
                if object.qualifier != Qualifier::No {
                  cells[cell_ix].remove(&object.object);
                }
              }
              LHSEntity::Property {
                binder, qualifier, ..
              } => {
                if qualifier != &Qualifier::No {
                  cells[cell_ix].remove(&bindings[binder]);
                }
              }
            }
          }
          for rhs_entity in rhs_entities.iter() {
            match rhs_entity {
              RHSEntity::Object(ref object) => {
                if object.qualifier != Qualifier::No {
                  let movement = match qualifier_to_movement(object.qualifier) {
                    // if passthrough, just keep whatever is here already, or
                    // stationary
                    None => *old_cell
                      .get(&object.object)
                      .unwrap_or(&Movement::Stationary),
                    Some(movement) => movement,
                  };
                  for other_object in collision_layers.objects_in_same_layer(&object.object) {
                    cells[cell_ix].remove(other_object);
                  }
                  cells[cell_ix].insert(object.object.clone(), movement);
                }
              }
              RHSEntity::Property {
                qualifier, binder, ..
              } => {
                if qualifier != &Qualifier::No {
                  let object = bindings[binder].clone();
                  let movement = match qualifier_to_movement(*qualifier) {
                    None => *old_cell.get(&object).unwrap_or(&Movement::Stationary),
                    Some(movement) => movement,
                  };
                  for other_object in collision_layers.objects_in_same_layer(&object) {
                    cells[cell_ix].remove(other_object);
                  }
                  cells[cell_ix].insert(object, movement);
                }
              }
              RHSEntity::Random(_) => panic!("TODO random"),
            }
          }
          if cells[cell_ix] != old_cell {
            something_changed = true;
          }
        }
      }
    }
    something_changed
  }
}

fn match_cells<'a, 'b, RHS>(
  properties: &HashMap<PropertyName, HashSet<PropertyName>>,
  cells: &grid::Slice<'a, Cell>,
  matchers: &'b [CellMatcher<RHS>],
  begin_at: usize,
) -> Option<(MatchedCells<'b, RHS>, PropertyBindings)> {
  'outer: for start in begin_at..cells.len() {
    // stores where we matched for non-Ellipsis entities
    let mut matches: Vec<usize> = Vec::new();
    let mut cell_cursor = start;
    let mut matcher_cursor = 0;
    let mut after_ellipsis = false;
    let mut bindings = HashMap::new();
    'inner: loop {
      // if we've matched all the matchers, we're done
      if matcher_cursor >= matchers.len() {
        break 'inner;
      }
      // if we're going out of bounds, stop
      if cell_cursor >= cells.len() {
        continue 'outer;
      }
      match matchers[matcher_cursor] {
        CellMatcher::Ellipsis => {
          // we have an ellipsis, just go forward
          matcher_cursor += 1;
          after_ellipsis = true;
        }
        CellMatcher::Objects(ref lhs_objects, _) => {
          match match_cell(properties, &cells[cell_cursor], lhs_objects) {
            None => {
              if after_ellipsis {
                // if we're after an ellipsis, just go to next cell
                cell_cursor += 1;
              } else {
                // otherwise, the rule does not match, continue
                continue 'outer;
              }
            }
            Some(new_bindings) => {
              // the rule matched, advance everything
              matches.push(cell_cursor);
              after_ellipsis = false;
              cell_cursor += 1;
              matcher_cursor += 1;
              bindings.extend(new_bindings);
            }
          }
        }
      }
    }

    // if we made it so far, we've done it
    return Some((
      MatchedCells {
        matches,
        cell_matchers: matchers,
        slice_desc: cells.desc,
      },
      bindings,
    ));
  }

  None
}

struct MatchedCellsCandidate<'a> {
  /// what slice matched
  matched_slice_ix: usize,
  /// we've already checked all the remaining slices
  exhausted: bool,
  matched_cells: MatchedCells<'a, Objects<RHSEntity>>,
  bindings: PropertyBindings,
}

/// returns whether the rule body could be applied. here "could be applied"
/// means:
///
/// * For normal rule bodies, that they matched and that applying them changed
///   something;
/// * For "no consequence" rule bodies, that they matched.
fn apply_rule_body<'a>(
  properties: &HashMap<PropertyName, HashSet<ObjectName>>,
  collision_layers: &CollisionLayersInfo,
  stage: &mut Stage,
  axis: grid::SliceAxis,
  reversed: bool,
  rule_body: &'a RuleBody,
) -> bool {
  let available_slices = match axis {
    grid::SliceAxis::Row => stage.nrows(),
    grid::SliceAxis::Col => stage.ncols(),
  };
  match rule_body {
    RuleBody::NoConsequence(matchers) => {
      // note that for no consequence rules, there is no notion of "changing"
      // the stage or not, so we consider any rule that matches at all as applied.

      // loop through all the matchers
      'no_consequence_matchers: for matcher in matchers.iter() {
        // and for each matcher, loop through each slice.
        for slice_ix in 0..available_slices {
          let cells = grid::Slice {
            desc: grid::SliceDesc {
              axis,
              reversed,
              index: slice_ix,
            },
            grid: stage,
          };
          // if we match, greedily choose this match and go to next matcher.
          // note that we do not need to "backtrack" if subsequent matchers
          // do not match, since matchers can never influence each other.
          //
          // moreover, in "no consequence" matchers we know we are done as
          // soon as something matches -- we do not have to check if we
          // changed something or not.
          match match_cells(properties, &cells, matcher, 0) {
            None => (),
            Some(_) => continue 'no_consequence_matchers,
          }
        }

        // we could not match this matcher in any way. give up.
        return false;
      }

      true
    }
    RuleBody::Normal(matchers) => {
      // for normal matchers we must try all possible configurations until
      // we find on that not only matches, but that it also changes something.
      //
      // however, we can still avoid copying the stage, since as soon as a matching
      // rule changes something we exit.
      //
      // if nothing change once we find a configuration, we go through the matches
      // left-to-right and try to find a new slice that matches.
      //
      // note: matchers may overlap, which makes the semantics of "rule application"
      // quite gnarly. here we ignore the issue (as puzzlescript does as far as i can
      // tell), and we assume throughout the matching that they never influence each
      // other.

      let mut matches: Vec<MatchedCellsCandidate<'a>> = Vec::with_capacity(matchers.len());

      // first, fill in the matches with the first candidates (if any)
      'normal_matchers: for matcher in matchers.iter() {
        for slice_ix in 0..available_slices {
          let cells = grid::Slice {
            desc: grid::SliceDesc {
              axis,
              reversed,
              index: slice_ix,
            },
            grid: stage,
          };
          match match_cells(properties, &cells, matcher, 0) {
            None => (),
            Some((matched_cells, bindings)) => {
              matches.push(MatchedCellsCandidate {
                matched_slice_ix: slice_ix,
                exhausted: slice_ix == available_slices - 1,
                matched_cells,
                bindings,
              });
              continue 'normal_matchers;
            }
          }
        }

        // we could not match this matcher in any way. give up directly.
        return false;
      }

      // now, loop until we find a configuration that changes something, or
      // when you've exhausted all configurations
      loop {
        // first check if the current configuration changes something
        let overall_bindings: PropertyBindings = matches
          .iter()
          .flat_map(|candidate| {
            candidate
              .bindings
              .iter()
              .map(|(binder, object)| (*binder, object.clone()))
          })
          .collect();
        let mut something_changed = false;
        // don't replace with a fold, it's important that they all run
        for candidate in matches.iter() {
          something_changed =
            candidate
              .matched_cells
              .apply(collision_layers, &overall_bindings, stage)
              || something_changed;
        }
        // if something _did_ change, we're done
        if something_changed {
          return true;
        }

        // otherwise, try to find a new configuration
        let mut new_matches = Vec::with_capacity(matches.len());
        let mut new_configuration = false;
        'candidates: for candidate in matches.into_iter() {
          // just keep going if we already found something new or if we've alread
          // looked through all the slices
          if new_configuration || candidate.exhausted {
            new_matches.push(candidate);
          } else {
            for slice_ix in candidate.matched_slice_ix..available_slices {
              let cells = grid::Slice {
                desc: grid::SliceDesc {
                  axis,
                  reversed,
                  index: slice_ix,
                },
                grid: stage,
              };
              // TODO this is broken -- it won't work for ellipsis rules that must
              // be resumed from the ellipsis (because we might match later), not
              // from the beginning.
              let start = if slice_ix == candidate.matched_slice_ix {
                candidate.matched_cells.matches[0] + 1
              } else {
                0
              };
              match match_cells(
                properties,
                &cells,
                candidate.matched_cells.cell_matchers,
                start,
              ) {
                None => (),
                Some((matched_cells, bindings)) => {
                  // we found a new configuration
                  new_configuration = true;
                  new_matches.push(MatchedCellsCandidate {
                    matched_slice_ix: slice_ix,
                    exhausted: slice_ix == available_slices - 1,
                    matched_cells,
                    bindings,
                  });
                  continue 'candidates;
                }
              }
            }

            // we didn't find anything new, just push the old match again --
            // but mark it as exhausted
            new_matches.push(MatchedCellsCandidate {
              matched_slice_ix: candidate.matched_slice_ix,
              exhausted: true,
              matched_cells: candidate.matched_cells,
              bindings: candidate.bindings,
            });
          }
        }
        // if we didn't find anything new, give up
        if !new_configuration {
          return false;
        }
        // otherwise, keep looping with the new matches
        matches = new_matches;
      }
    }
  }
}

fn apply_rule(
  properties: &HashMap<PropertyName, HashSet<ObjectName>>,
  collision_layers: &CollisionLayersInfo,
  stage: &mut Stage,
  commands: &mut Vec<RuleCommand>,
  rule: &Rule,
) -> bool {
  let (axis, reversed) = match rule.direction {
    RuleDirection::Down => (grid::SliceAxis::Col, false),
    RuleDirection::Up => (grid::SliceAxis::Col, true),
    RuleDirection::Right => (grid::SliceAxis::Row, false),
    RuleDirection::Left => (grid::SliceAxis::Row, true),
  };

  let matched = apply_rule_body(
    properties,
    collision_layers,
    stage,
    axis,
    reversed,
    &rule.body,
  );
  if matched {
    verbose_log!("Rule {} {} applied.", rule.line_number, rule.direction);
    for command in rule.commands.clone() {
      commands.push(command.clone());
      // exit early on cancel, since the original puzzlescript allows
      // you to write rules that match forever + cancel or restart
      //
      // TODO Have this to be more structured, go all the way up with
      // an explicit Cancel / Restart rather than a boolean
      if command == RuleCommand::Cancel || command == RuleCommand::Restart {
        return false;
      }
    }
  };
  matched
}

fn apply_rule_group(
  properties: &HashMap<PropertyName, HashSet<ObjectName>>,
  collision_layers: &CollisionLayersInfo,
  stage: &mut Stage,
  commands: &mut Vec<RuleCommand>,
  rule_group: &RuleGroup,
) -> bool {
  if rule_group.random {
    panic!("TODO random");
  }
  let mut keep_going = true;
  let mut any_matched = false;
  while keep_going {
    keep_going = false;
    for rule in rule_group.rules.iter() {
      let matched = apply_rule(properties, collision_layers, stage, commands, rule);
      keep_going = keep_going || matched;
      any_matched = any_matched || matched;
    }
  }
  any_matched
}

fn apply_rule_groups(
  properties: &HashMap<PropertyName, HashSet<ObjectName>>,
  collision_layers: &CollisionLayersInfo,
  stage: &mut Stage,
  commands: &mut Vec<RuleCommand>,
  rule_groups: &[RuleGroup],
) -> bool {
  let mut any_matched = false;
  for rule_group in rule_groups {
    let matched = apply_rule_group(properties, collision_layers, stage, commands, rule_group);
    any_matched = any_matched || matched;
  }
  any_matched
}

fn apply_movement(collision_layers_info: &CollisionLayersInfo, stage: &mut Stage) -> bool {
  let mut something_moved = false;

  let mut move_to_cell = |stage: &mut Stage, object, row, col, target_row, target_col| -> bool {
    // if we're out of bounds, forget it
    if target_row < 0
      || target_row >= stage.nrows() as isize
      || target_col < 0
      || target_col >= stage.ncols() as isize
    {
      // we know we'll never be able to move.
      stage[(row, col)].insert(object, Movement::Stationary);
      return false;
    }
    // if the cell contains anything from the same collision layer
    // as ours, abort
    let our_collision_layer = collision_layers_info.object_to_ix[&object];
    let target_cell = &stage[(target_row as usize, target_col as usize)];
    for other_object in target_cell.keys() {
      if collision_layers_info.object_to_ix[other_object] == our_collision_layer {
        return false;
      }
    }
    // otherwise, remove the object from the source and put it in
    // the target
    something_moved = true;
    stage[(row, col)].remove(&object);
    stage[(target_row as usize, target_col as usize)].insert(object, Movement::Stationary);
    true
  };

  let mut keep_going = true;
  while keep_going {
    keep_going = false;
    for row in 0..stage.nrows() {
      for col in 0..stage.ncols() {
        // TODO can I avoid copying this? If i don't, I hold a borrow to `stage`
        // and it doesn't allow me to borrow it as mutable later.
        let objects: Vec<(ObjectName, Movement)> = stage[(row, col)]
          .iter()
          .map(|(object, movement)| (object.clone(), *movement))
          .collect();
        for (object, movement) in objects {
          match movement {
            Movement::Stationary => (),
            Movement::Up => {
              keep_going =
                move_to_cell(stage, object, row, col, row as isize - 1, col as isize) || keep_going
            }
            Movement::Down => {
              keep_going =
                move_to_cell(stage, object, row, col, row as isize + 1, col as isize) || keep_going
            }
            Movement::Left => {
              keep_going =
                move_to_cell(stage, object, row, col, row as isize, col as isize - 1) || keep_going
            }
            Movement::Right => {
              keep_going =
                move_to_cell(stage, object, row, col, row as isize, col as isize + 1) || keep_going
            }
            Movement::Action => (),
          }
        }
      }
    }
  }

  // clear all remaining movement -- if there are cycles
  // that we werent able to move, or pieces just got stuck
  for cell in stage.iter_mut() {
    for movement in cell.iter_mut() {
      *movement = Movement::Stationary;
    }
  }

  something_moved
}

fn has_movement(stage: &Stage) -> bool {
  for cell in stage.iter() {
    for movement in cell.values() {
      if *movement != Movement::Stationary {
        return true;
      }
    }
  }
  false
}

fn check_win_condition(win_condition: &WinCondition, stage: &Stage) -> bool {
  match win_condition {
    WinCondition::No(objects) => {
      for cell in stage.iter() {
        for object in cell.keys() {
          if objects.contains(object) {
            return false;
          }
        }
      }
      true
    }
    WinCondition::NoOn(left, right) => {
      for cell in stage.iter() {
        for object_left in left.iter() {
          for object_right in right.iter() {
            if cell.contains_key(object_left) && cell.contains_key(object_right) {
              return false;
            }
          }
        }
      }
      true
    }
    WinCondition::AllOn(left, right) => {
      for cell in stage.iter() {
        'left: for object_left in left.iter() {
          if cell.contains_key(object_left) {
            for object_right in right.iter() {
              // if it's on any of the objects, we're done with analizing
              // this object
              if cell.contains_key(object_right) {
                continue 'left;
              }
            }
            return false;
          }
        }
      }
      true
    }
    WinCondition::Some(objects) => {
      for cell in stage.iter() {
        for object in cell.keys() {
          if objects.contains(object) {
            return true;
          }
        }
      }
      false
    }
    WinCondition::SomeOn(left, right) => {
      for cell in stage.iter() {
        for object_left in left.iter() {
          if cell.contains_key(object_left) {
            for object_right in right.iter() {
              // if it's on any of the objects, we're done
              if cell.contains_key(object_right) {
                return true;
              }
            }
          }
        }
      }
      false
    }
  }
}

fn check_win_conditions(win_conditions: &[WinCondition], stage: &Stage) -> bool {
  for win_condition in win_conditions {
    if !check_win_condition(win_condition, stage) {
      return false;
    }
  }
  true
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Advance {
  /// Nothing happened
  Nothing,
  /// Some movement / rules happened
  Active,
  /// We've won the current stage
  Won,
  /// We need to restart the level
  Restart,
}

struct CollisionLayersInfo<'a> {
  layers: &'a [CollisionLayer],
  object_to_ix: HashMap<ObjectName, usize>,
}

impl<'a> CollisionLayersInfo<'a> {
  /// returns including the provided object
  fn objects_in_same_layer(&self, object: &ObjectName) -> &Vec<ObjectName> {
    match self.layers[self.object_to_ix[object]] {
      CollisionLayer::Background => panic!("Unexpected background layer"),
      CollisionLayer::Normal(ref objects) => objects,
    }
  }
}

/// returns whether the winning conditions were satisfied
pub fn advance(game: &Game, stage: &mut Stage) -> Advance {
  // build a map from each object to a collision layer id
  let collision_layers_info = CollisionLayersInfo {
    layers: &game.collision_layers,
    object_to_ix: {
      let mut collision_layers_map: HashMap<ObjectName, usize> = HashMap::new();
      for (ix, collision_layer) in game.collision_layers.iter().enumerate() {
        match collision_layer {
          CollisionLayer::Background => (),
          CollisionLayer::Normal(objects) => {
            for object in objects {
              collision_layers_map.insert(object.clone(), ix);
            }
          }
        }
      }
      collision_layers_map
    },
  };

  let mut active = false;
  let mut commands = Vec::new();
  verbose_log!("# Applying rules");
  active = apply_rule_groups(
    &game.properties,
    &collision_layers_info,
    stage,
    &mut commands,
    &game.rules,
  ) || active;
  verbose_log!("# Applying movement");
  active = apply_movement(&collision_layers_info, stage) || active;
  verbose_log!("# Applying late rules");
  active = apply_rule_groups(
    &game.properties,
    &collision_layers_info,
    stage,
    &mut commands,
    &game.late_rules,
  ) || active;
  if has_movement(stage) {
    panic!("Got movement after late rules!");
  };
  for command in commands {
    match command {
      RuleCommand::Sound(_) => (), // TODO sounds
      RuleCommand::Cancel => return Advance::Nothing,
      RuleCommand::Restart => return Advance::Restart,
      command => panic!("TODO command: {:?}", command),
    }
  }
  let won = check_win_conditions(&game.win_conditions, stage);
  if won {
    Advance::Won
  } else if active {
    Advance::Active
  } else {
    Advance::Nothing
  }
}
