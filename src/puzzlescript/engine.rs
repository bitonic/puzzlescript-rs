use crate::puzzlescript::game::*;
use crate::puzzlescript::grid;
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
    // rule_line_number: usize,
    properties: &HashMap<PropertyName, HashSet<PropertyName>>,
    cell: &im_hashmap::HashMap<ObjectName, Movement>,
    matcher: &Objects<LHSEntity>,
) -> Option<PropertyBindings> {
    let mut bindings: PropertyBindings = HashMap::new();

    'running: for lhs_entity in matcher.iter() {
        match lhs_entity {
            LHSEntity::Object(object) => match cell.get(&object.object) {
                None => {
                    if !(object.qualifier == Qualifier::No) {
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
    matchers: &'a Vec<CellMatcher<RHS>>,
    /// in what slice of the stage we matched
    slice_desc: grid::SliceDesc,
    /// at which points of the slice each non-ellipsis matcher matched
    matches: Vec<usize>,
}

impl<'a> MatchedCells<'a, Objects<RHSEntity>> {
    /// returns if anything changed
    pub fn apply(&self, bindings: &PropertyBindings, stage: &mut Stage) -> bool {
        let mut something_changed = false;
        let mut matches_iter = self.matches.iter();
        let mut cells = grid::SliceMut {
            grid: stage,
            desc: self.slice_desc,
        };
        for matcher in self.matchers {
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
                                    cells[cell_ix].insert(object.object.clone(), movement);
                                }
                            }
                            RHSEntity::Property {
                                qualifier, binder, ..
                            } => {
                                if qualifier != &Qualifier::No {
                                    let object = bindings[binder].clone();
                                    let movement = match qualifier_to_movement(*qualifier) {
                                        None => {
                                            *old_cell.get(&object).unwrap_or(&Movement::Stationary)
                                        }
                                        Some(movement) => movement,
                                    };
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
    // rule_line_number: usize,
    properties: &HashMap<PropertyName, HashSet<PropertyName>>,
    cells: &grid::Slice<'a, Cell>,
    matchers: &'b Vec<CellMatcher<RHS>>,
) -> Option<(MatchedCells<'b, RHS>, PropertyBindings)> {
    let mut min_len = 0;
    for matcher in matchers {
        match matcher {
            CellMatcher::Ellipsis => (),
            CellMatcher::Objects(_, _) => min_len += 1,
        }
    }

    'outer: for start in 0..(cells.len() - min_len) {
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
                    match match_cell(
                        // rule_line_number,
                        properties,
                        &cells[cell_cursor],
                        lhs_objects,
                    ) {
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
                matchers,
                slice_desc: cells.desc,
            },
            bindings,
        ));
    }

    return None;
}

fn apply_rule_body(
    // rule_line_number: usize,
    properties: &HashMap<PropertyName, HashSet<ObjectName>>,
    stage: &mut Stage,
    axis: grid::SliceAxis,
    reversed: bool,
    rule_body: &RuleBody,
) -> bool {
    let max_slice_index = match axis {
        grid::SliceAxis::Row => stage.nrows(),
        grid::SliceAxis::Col => stage.ncols(),
    };
    match rule_body {
        RuleBody::NoConsequence(_matchers) => {
            panic!("TODO RuleBody::NoConsequence");
        }
        RuleBody::Normal(matchers) => {
            let mut matches = Vec::new();
            let mut bindings = HashMap::new();

            // loop through all the matchers
            'matchers: for matcher in matchers.iter() {
                // and for each matcher, loop through each slice.
                for slice_ix in 0..max_slice_index {
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
                    // in other words, if we're trying to match matcher `A` and then
                    // matcher `B`, if matcher `A` matched with some configuration and
                    // matcher `B` does not, matcher `B` will never match regardless
                    // of the configuration we have picked for `A`.
                    match match_cells(/* rule_line_number, */ properties, &cells, matcher) {
                        None => (),
                        Some((match_, match_bindings)) => {
                            bindings.extend(match_bindings);
                            matches.push(match_);
                            continue 'matchers;
                        }
                    }
                }

                // we could not match this matcher in any way. give up.
                return false;
            }

            // every matcher matches -- now apply
            let mut something_changed = false;
            for match_ in matches.iter() {
                something_changed = match_.apply(&bindings, stage) || something_changed;
            }
            something_changed
        }
    }
}

fn apply_rule(
    properties: &HashMap<PropertyName, HashSet<ObjectName>>,
    stage: &mut Stage,
    rule: &Rule,
) -> bool {
    match rule.command {
        None => (),
        Some(_) => panic!("TODO rule command"),
    }

    let (axis, reversed) = match rule.direction {
        RuleDirection::Down => (grid::SliceAxis::Col, false),
        RuleDirection::Up => (grid::SliceAxis::Col, true),
        RuleDirection::Right => (grid::SliceAxis::Row, false),
        RuleDirection::Left => (grid::SliceAxis::Row, true),
    };

    apply_rule_body(
        // rule.line_number,
        properties, stage, axis, reversed, &rule.body,
    )
}

fn apply_rule_group(
    properties: &HashMap<PropertyName, HashSet<ObjectName>>,
    stage: &mut Stage,
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
            let matched = apply_rule(properties, stage, rule);
            keep_going = keep_going || matched;
            any_matched = any_matched || matched;
        }
    }
    any_matched
}

fn apply_rule_groups(
    properties: &HashMap<PropertyName, HashSet<ObjectName>>,
    stage: &mut Stage,
    rule_groups: &[RuleGroup],
) -> bool {
    let mut any_matched = false;
    for rule_group in rule_groups {
        let matched = apply_rule_group(properties, stage, rule_group);
        any_matched = any_matched || matched;
    }
    any_matched
}

fn apply_movement(collision_layers: &[CollisionLayer], stage: &mut Stage) -> bool {
    let mut something_moved = false;

    // build a map from each object to a collision layer id
    let mut collision_layers_map: HashMap<ObjectName, usize> = HashMap::new();
    for (ix, collision_layer) in collision_layers.iter().enumerate() {
        match collision_layer {
            CollisionLayer::Background => (),
            CollisionLayer::Normal(objects) => {
                for object in objects {
                    collision_layers_map.insert(object.clone(), ix);
                }
            }
        }
    }

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
        let our_collision_layer = collision_layers_map[&object];
        let target_cell = &stage[(target_row as usize, target_col as usize)];
        for other_object in target_cell.keys() {
            if collision_layers_map[other_object] == our_collision_layer {
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
                            keep_going = move_to_cell(
                                stage,
                                object,
                                row,
                                col,
                                row as isize - 1,
                                col as isize,
                            ) || keep_going
                        }
                        Movement::Down => {
                            keep_going = move_to_cell(
                                stage,
                                object,
                                row,
                                col,
                                row as isize + 1,
                                col as isize,
                            ) || keep_going
                        }
                        Movement::Left => {
                            keep_going = move_to_cell(
                                stage,
                                object,
                                row,
                                col,
                                row as isize,
                                col as isize - 1,
                            ) || keep_going
                        }
                        Movement::Right => {
                            keep_going = move_to_cell(
                                stage,
                                object,
                                row,
                                col,
                                row as isize,
                                col as isize + 1,
                            ) || keep_going
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
        if check_win_condition(win_condition, stage) {
            return true;
        }
    }
    false
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Advance {
    /// Nothing happened
    Nothing,
    /// Some movement / rules happened
    Active,
    /// We've won the current stage
    Won,
}

/// returns whether the winning conditions were satisfied
pub fn advance(game: &Game, stage: &mut Stage) -> Advance {
    // println!("# Advance");
    let mut active = false;
    // println!("## Rule");
    active = apply_rule_groups(&game.properties, stage, &game.rules) || active;
    // println!("## Movement");
    active = apply_movement(&game.collision_layers, stage) || active;
    // println!("## Late rules");
    active = apply_rule_groups(&game.properties, stage, &game.late_rules) || active;
    if has_movement(stage) {
        panic!("Got movement after late rules!");
    };
    // println!("## Check win");
    let won = check_win_conditions(&game.win_conditions, stage);
    if won {
        Advance::Won
    } else if active {
        Advance::Active
    } else {
        Advance::Nothing
    }
}
