use crate::grid::Grid;
use crate::math::*;
use crate::puzzlescript::ast::*;
use crate::puzzlescript::colors::*;
use failure::Fail;
use lazy_static::*;
use std::collections::HashSet;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Section {
  Prelude,
  Objects,
  Legend,
  Sounds,
  CollisionLayers,
  Rules,
  WinConditions,
  Levels,
}

impl fmt::Display for Section {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Section::Prelude => write!(f, "PRELUDE"),
      Section::Objects => write!(f, "OBJECTS"),
      Section::Legend => write!(f, "LEGEND"),
      Section::Sounds => write!(f, "SOUNDS"),
      Section::CollisionLayers => write!(f, "COLLISIONLAYERS"),
      Section::Rules => write!(f, "RULES"),
      Section::WinConditions => write!(f, "WINCONDITIONS"),
      Section::Levels => write!(f, "LEVELS"),
    }
  }
}

impl Section {
  fn next(self) -> Option<Section> {
    match self {
      Section::Prelude => Some(Section::Objects),
      Section::Objects => Some(Section::Legend),
      Section::Legend => Some(Section::Sounds),
      Section::Sounds => Some(Section::CollisionLayers),
      Section::CollisionLayers => Some(Section::Rules),
      Section::Rules => Some(Section::WinConditions),
      Section::WinConditions => Some(Section::Levels),
      Section::Levels => None,
    }
  }
}

struct ParseState<'a> {
  cursor: usize,
  chars: Vec<char>,
  ast: Ast,
  original_src: &'a str,
  section: Section,
}

type ErrorMsg = String;

fn has_char(chars: &[char], cursor: usize, ch: char) -> bool {
  cursor < chars.len() && chars[cursor] == ch
}

fn satisfies_char<F>(chars: &[char], cursor: usize, f: &F) -> bool
where
  F: Fn(char) -> bool,
{
  cursor < chars.len() && f(chars[cursor])
}

fn parse_byte(str: &str) -> Result<u8, ErrorMsg> {
  match u8::from_str_radix(str, 16) {
    Ok(x) => Ok(x),
    Err(_err) => Err(format!("Invalid byte {:?}", str)),
  }
}

fn assert_found<F>(what: &str, x: Option<F>) -> Result<F, ErrorMsg> {
  match x {
    None => Err(format!("Expected {}", what)),
    Some(x) => Ok(x),
  }
}

lazy_static! {
  static ref ENTITY_NAME_SYMBOLS: HashSet<char> = {
    let mut chars = HashSet::new();
    chars.insert('.');
    chars.insert('#');
    chars.insert('*');
    chars.insert('[');
    chars.insert(']');
    chars.insert('{');
    chars.insert('}');
    chars.insert('@');
    chars.insert('_');
    chars.insert(',');
    chars.insert('`');
    chars.insert('\'');
    chars.insert('~');
    chars.insert(';');
    chars.insert('$');
    chars.insert('-');
    chars.insert('|');
    chars
  };
}

fn is_entity_name_symbol(ch: char) -> bool {
  ch.is_digit(10) || ENTITY_NAME_SYMBOLS.contains(&ch)
}

fn parse_color(str: &str) -> Result<Color, ErrorMsg> {
  let fail = Err(format!("Invalid color: {}", str));

  if str.is_empty() {
    return fail;
  }

  if &str[0..1] == "#" {
    if str.len() == 7 {
      Ok(Color::Hex(vec3(
        parse_byte(&str[1..3])?,
        parse_byte(&str[3..5])?,
        parse_byte(&str[5..7])?,
      )))
    } else if str.len() == 4 {
      pub fn double_str(s: &str) -> String {
        let mut ds: String = s.to_string();
        ds += s;
        ds
      };

      Ok(Color::Hex(vec3(
        parse_byte(&double_str(&str[1..2]))?,
        parse_byte(&double_str(&str[2..3]))?,
        parse_byte(&double_str(&str[3..4]))?,
      )))
    } else {
      Err(format!("Invalid hex color: {}", str))
    }
  } else {
    match str {
      "black" => Ok(Color::Named(NamedColor::Black)),
      "white" => Ok(Color::Named(NamedColor::White)),
      "lightgray" | "lightgrey" => Ok(Color::Named(NamedColor::LightGray)),
      "gray" | "grey" => Ok(Color::Named(NamedColor::Gray)),
      "darkgray" | "darkgrey" => Ok(Color::Named(NamedColor::DarkGray)),
      "red" => Ok(Color::Named(NamedColor::Red)),
      "darkred" => Ok(Color::Named(NamedColor::DarkRed)),
      "lightred" => Ok(Color::Named(NamedColor::LightRed)),
      "brown" => Ok(Color::Named(NamedColor::Brown)),
      "darkbrown" => Ok(Color::Named(NamedColor::DarkBrown)),
      "lightbrown" => Ok(Color::Named(NamedColor::LightBrown)),
      "orange" => Ok(Color::Named(NamedColor::Orange)),
      "yellow" => Ok(Color::Named(NamedColor::Yellow)),
      "green" => Ok(Color::Named(NamedColor::Green)),
      "darkgreen" => Ok(Color::Named(NamedColor::DarkGreen)),
      "lightgreen" => Ok(Color::Named(NamedColor::LightGreen)),
      "blue" => Ok(Color::Named(NamedColor::Blue)),
      "lightblue" => Ok(Color::Named(NamedColor::LightBlue)),
      "darkblue" => Ok(Color::Named(NamedColor::DarkBlue)),
      "purple" => Ok(Color::Named(NamedColor::Purple)),
      "pink" => Ok(Color::Named(NamedColor::Pink)),
      "transparent" => Ok(Color::Transparent),
      _ => fail,
    }
  }
}

fn parse_color_palette(str: &str) -> Result<ColorPalette, ErrorMsg> {
  match str {
    "1" => Ok(ColorPalette::MasterSystem),
    "2" => Ok(ColorPalette::GameBoyColour),
    "3" => Ok(ColorPalette::Amiga),
    "4" => Ok(ColorPalette::Arne),
    "5" => Ok(ColorPalette::Famicom),
    "6" => Ok(ColorPalette::Atari),
    "7" => Ok(ColorPalette::Pastel),
    "8" => Ok(ColorPalette::Ega),
    "9" => Ok(ColorPalette::Amstrad),
    "10" => Ok(ColorPalette::ProteusMellow),
    "11" => Ok(ColorPalette::ProteusRich),
    "12" => Ok(ColorPalette::ProteusNight),
    "13" => Ok(ColorPalette::C64),
    "14" => Ok(ColorPalette::Whitingjp),
    "mastersystem" => Ok(ColorPalette::MasterSystem),
    "gameboycolour" => Ok(ColorPalette::GameBoyColour),
    "amiga" => Ok(ColorPalette::Amiga),
    "arnecolors" => Ok(ColorPalette::Arne),
    "famicom" => Ok(ColorPalette::Famicom),
    "atari" => Ok(ColorPalette::Atari),
    "pastel" => Ok(ColorPalette::Pastel),
    "ega" => Ok(ColorPalette::Ega),
    "amstrad" => Ok(ColorPalette::Amstrad),
    "proteus_mellow" => Ok(ColorPalette::ProteusMellow),
    "proteus_rich" => Ok(ColorPalette::ProteusRich),
    "proteus_night" => Ok(ColorPalette::ProteusNight),
    "c64" => Ok(ColorPalette::C64),
    "whitingjp" => Ok(ColorPalette::Whitingjp),
    _ => Err(format!("Bad color palette {}", str)),
  }
}

type ParsedMatcher = Vec<CellMatcher<()>>;

impl<'a> ParseState<'a> {
  fn peek_char(&self, ch: char) -> bool {
    if self.cursor < self.chars.len() {
      self.chars[self.cursor] == ch
    } else {
      false
    }
  }

  fn peek_str(&self, str: &str) -> bool {
    let mut local_cursor = self.cursor;

    // process string
    for ch in str.chars() {
      if !has_char(&self.chars, local_cursor, ch) {
        return false;
      }
      local_cursor += 1;
    }

    true
  }

  fn discard_spaces(&mut self) -> Result<(), ErrorMsg> {
    while self.peek_char(' ') || self.peek_char('\t') || self.peek_char('(') {
      // eat all whitespace
      while self.peek_char(' ') || self.peek_char('\t') {
        self.cursor += 1;
      }

      // eat comments, also spanning across newlines.
      if self.peek_char(')') {
        return Err("Closing unopened comment".to_string());
      }
      if self.peek_char('(') {
        let mut comment_level: usize = 0;
        'running: loop {
          if self.peek_char('(') {
            comment_level += 1;
            self.cursor += 1;
          } else if self.peek_char(')') {
            self.cursor += 1;
            comment_level -= 1;
            if comment_level == 0 {
              break 'running;
            }
          } else if self.at_eof() {
            return Err("Non-terminated comment!".to_string());
          } else {
            self.cursor += 1;
          }
        }
      }
    }

    Ok(())
  }

  fn at_eof(&mut self) -> bool {
    self.cursor >= self.chars.len()
  }

  fn lex<A, F>(&mut self, f: F) -> Result<Option<A>, ErrorMsg>
  where
    F: FnOnce(&mut ParseState) -> Result<Option<A>, ErrorMsg>,
  {
    let orig_cursor = self.cursor;

    match f(self)? {
      None => {
        self.cursor = orig_cursor;
        Ok(None)
      }
      Some(res) => {
        self.discard_spaces()?;
        Ok(Some(res))
      }
    }
  }

  fn lex_token(&mut self, str: &'static str) -> Result<bool, ErrorMsg> {
    self
      .lex(|st| {
        for ch in str.chars() {
          if !has_char(&st.chars, st.cursor, ch) {
            return Ok(None);
          }
          st.cursor += 1;
        }
        Ok(Some(()))
      })
      .map(|mb| mb.is_some())
  }

  fn lex_satisfy<F>(&mut self, f: &F) -> Result<Option<Vec<char>>, ErrorMsg>
  where
    F: Fn(char) -> bool,
  {
    let mut local_cursor = self.cursor;
    let mut good_chars: Vec<char> = Vec::new();

    // process string
    'running: loop {
      if !satisfies_char(&self.chars, local_cursor, f) {
        break 'running;
      }

      good_chars.push(self.chars[local_cursor]);
      local_cursor += 1;
    }

    if !good_chars.is_empty() {
      self.cursor = local_cursor;
      self.discard_spaces()?;
      Ok(Some(good_chars))
    } else {
      Ok(None)
    }
  }

  /// consumes an entire line _excluding the newline character_.
  fn discard_line(&mut self) {
    while !self.peek_char('\n') && !self.at_eof() {
      self.cursor += 1;
    }
  }

  /// like `discard_line`, but returns the _trimmed_, _comment free_ line
  /// instead of discarding it.
  fn consume_line(&mut self, original_src: bool) -> Result<String, ErrorMsg> {
    let start_cursor = self.cursor;

    while !self.peek_char('\n') && !self.at_eof() {
      self.cursor += 1;
    }

    let str_with_comments = if original_src {
      self.original_src[start_cursor..self.cursor]
        .trim()
        .to_string()
    } else {
      self.chars[start_cursor..self.cursor]
        .iter()
        .collect::<String>()
        .trim()
        .to_string()
    };
    let mut str = "".to_string();

    // remove comments
    let mut comment_level: Option<usize> = None;
    for ch in str_with_comments.chars() {
      if ch == '(' {
        comment_level = match comment_level {
          None => Some(0),
          Some(lvl) => Some(lvl + 1),
        }
      } else if ch == ')' {
        comment_level = match comment_level {
          None => return Err("Closing unopened comment".to_string()),
          Some(0) => None,
          Some(lvl) => Some(lvl - 1),
        }
      } else {
        match comment_level {
          None => str += &ch.to_string(),
          Some(_) => (),
        }
      }
    }

    // trim again after removing comments
    Ok(str.trim().to_string())
  }

  fn consume_numeric_line(&mut self) -> Result<f64, ErrorMsg> {
    let line = self.consume_line(false)?;
    match line.parse() {
      Ok(x) => Ok(x),
      Err(_err) => Err(format!("Invalid number {}", line)),
    }
  }

  fn prelude_line(&mut self) -> Result<(), ErrorMsg> {
    if self.lex_token("title")? {
      self.ast.prelude.title = Some(self.consume_line(true)?);
    } else if self.lex_token("author")? {
      self.ast.prelude.author = Some(self.consume_line(true)?);
    } else if self.lex_token("key_repeat_interval")? {
      self.ast.prelude.key_repeat_interval = self.consume_numeric_line()?;
    } else if self.lex_token("text_color")? {
      let line = self.consume_line(true)?;
      self.ast.prelude.text_color = parse_color(&line)?;
    } else if self.lex_token("homepage")? {
      self.ast.prelude.homepage = Some(self.consume_line(true)?);
    } else if self.lex_token("require_player_movement")? {
      self.ast.prelude.require_player_movement = true;
    } else if self.lex_token("background_color")? {
      self.ast.prelude.background_color = assert_found("background color", self.lex_color()?)?;
    } else if self.lex_token("color_palette")? {
      let palette_str = self.consume_line(true)?;
      self.ast.prelude.color_palette = parse_color_palette(&palette_str)?;
    } else if self.lex_token("noundo")? {
      self.ast.prelude.noundo = true;
    } else if self.lex_token("run_rules_on_level_start")? {
      self.ast.prelude.run_rules_on_level_start = true;
    } else if self.lex_token("again_interval")? {
      self.ast.prelude.again_interval = Some(self.consume_numeric_line()?);
    } else if self.lex_token("norepeat_action")? {
      self.ast.prelude.norepeat_action = true;
    } else if self.lex_token("debug")? {
      self.ast.prelude.debug = true;
    } else if self.lex_token("verbose_logging")? {
      self.ast.prelude.verbose_logging = true;
    } else {
      Err("Invalid prelude line")?;
    }
    Ok(())
  }

  fn assert_end_of_line(&mut self, for_: &str) -> Result<(), ErrorMsg> {
    if !self.peek_char('\n') && !self.at_eof() {
      Err(format!(
        "Expecting end of line or end of file after {}",
        for_
      ))
    } else {
      self.cursor += 1;
      Ok(())
    }
  }

  fn lex_section_start(&mut self) -> Result<Option<Section>, ErrorMsg> {
    if self.lex_token("objects")? {
      Ok(Some(Section::Objects))
    } else if self.lex_token("legend")? {
      Ok(Some(Section::Legend))
    } else if self.lex_token("sounds")? {
      Ok(Some(Section::Sounds))
    } else if self.lex_token("collisionlayers")? {
      Ok(Some(Section::CollisionLayers))
    } else if self.lex_token("rules")? {
      Ok(Some(Section::Rules))
    } else if self.lex_token("winconditions")? {
      Ok(Some(Section::WinConditions))
    } else if self.lex_token("levels")? {
      Ok(Some(Section::Levels))
    } else {
      Ok(None)
    }
  }

  fn lex_entity_symbol(&mut self) -> Result<Option<EntityName>, ErrorMsg> {
    if satisfies_char(&self.chars, self.cursor, &is_entity_name_symbol) {
      let ch = self.chars[self.cursor];
      self.cursor += 1;
      self.discard_spaces()?;
      Ok(Some(Rc::from(ch.to_string())))
    } else {
      Ok(None)
    }
  }

  fn lex_entity_name(&mut self) -> Result<Option<EntityName>, ErrorMsg> {
    // first see if this is alphabetic entity name, otherwise try to parse it
    // as a single-char symbol

    let before_cursor = self.cursor;

    let mb_entity = self
      .lex_satisfy(&|ch| ch.is_alphabetic() || ch.is_digit(10) || ch == '_')?
      .map(|chars| -> String { chars.iter().collect() });

    match mb_entity {
      Some(entity) => {
        if entity.chars().next().unwrap().is_alphabetic() {
          Ok(Some(Rc::from(entity)))
        } else {
          self.cursor = before_cursor;
          self.lex_entity_symbol()
        }
      }
      None => {
        self.cursor = before_cursor;
        self.lex_entity_symbol()
      }
    }
  }

  fn lex_color(&mut self) -> Result<Option<Color>, ErrorMsg> {
    match self.lex_satisfy(&|ch: char| ch == '#' || ch.is_digit(16) || ch.is_alphabetic())? {
      None => Ok(None),
      Some(chs) => {
        let str: String = chs.into_iter().collect();
        parse_color(&str).map(Some)
      }
    }
  }

  fn lex_many<F, A>(&mut self, lex: F) -> Result<Vec<A>, ErrorMsg>
  where
    F: Fn(&mut ParseState) -> Result<Option<A>, ErrorMsg>,
  {
    let mut xs = Vec::new();

    'running: loop {
      match lex(self)? {
        None => break 'running,
        Some(x) => xs.push(x),
      }
    }

    Ok(xs)
  }

  fn objects_line(&mut self) -> Result<Option<Vec<Option<u8>>>, ErrorMsg> {
    let mb_line: Option<Vec<Option<u8>>> = self
      .lex_satisfy(&|ch: char| ch == '.' || ch.is_digit(10))?
      .map(|chs| {
        chs
          .iter()
          .map(|ch| {
            if *ch == '.' {
              None
            } else {
              Some(ch.to_digit(10).unwrap() as u8)
            }
          })
          .collect()
      });
    match mb_line {
      None => Ok(None),
      Some(line) => {
        if line.len() == 5 {
          Ok(Some(line))
        } else {
          Err(format!(
            "Expecting line of length 5 for object, got line of length {}",
            line.len()
          ))
        }
      }
    }
  }

  fn object_lines(&mut self) -> Result<Vec<Vec<Option<u8>>>, ErrorMsg> {
    let mut lines: Vec<Vec<Option<u8>>> = Vec::new();

    'running: loop {
      self.discard_spaces()?;

      match self.objects_line()? {
        None => break 'running,
        Some(line) => {
          lines.push(line);
          self.assert_end_of_line("object line")?;
        }
      }
    }

    if lines.is_empty() || lines.len() == 5 {
      Ok(lines)
    } else {
      Err(format!("Expected 5 lines for object, got {}", lines.len()))
    }
  }

  fn objects_block(&mut self) -> Result<(), ErrorMsg> {
    let name = self.assert_entity_name()?;
    self.validate_entity_definition(&name)?;
    match self.lex_entity_name()? {
      None => (),
      Some(legend) => {
        self.validate_entity_definition(&legend)?;
        self
          .ast
          .entities
          .legend
          .insert(legend.clone(), LegendBody::Alias(name.clone()));
        self.ast.entities.legend_order.push(legend);
      }
    }
    self.assert_end_of_line("object title")?;

    self.discard_spaces()?;
    let colors = self.lex_many(|st| st.lex_color())?;
    if colors.is_empty() {
      return Err(format!("No colors specified for object {}", name));
    }
    self.assert_end_of_line(&format!("colors for object {}", name))?;

    let lines = self.object_lines()?;
    if lines.len() > 1 {
      let object_width = lines[0].len();
      if !lines.iter().all(|line| line.len() == object_width) {
        return Err(format!("Lines for object {} have uneven size", name));
      }
    }
    let check_line_colors = |line: &Vec<Option<u8>>| {
      line.iter().all(|mb_ix| match mb_ix {
        None => true,
        Some(ix) => (*ix as usize) < colors.len(),
      })
    };
    if !lines.iter().all(check_line_colors) {
      return Err(format!(
        "Lines for object {} have out of bound colors",
        name
      ));
    }

    let lines_colors: Vec<Vec<Color>> = lines
      .iter()
      .map(|line| {
        line
          .iter()
          .map(|mb_ix| match mb_ix {
            None => Color::Transparent,
            Some(ix) => colors[*ix as usize],
          })
          .collect()
      })
      .collect();
    let obj = if lines_colors.is_empty() {
      if colors.len() == 1 {
        Object::empty(colors[0])
      } else {
        return Err(format!(
          "Expecting 1 color for empty object, but got {}",
          colors.len()
        ));
      }
    } else {
      Object::normal(&lines_colors)
    };
    self.ast.entities.objects.insert(name, obj);

    Ok(())
  }

  fn validate_entity_definition(&self, name: &str) -> Result<(), ErrorMsg> {
    if self.ast.entities.objects.contains_key(name) || self.ast.entities.legend.contains_key(name) {
      Err(format!("Name {} already in use", name))
    } else {
      Ok(())
    }
  }

  fn validate_entity_usage(&self, for_: &str, target: &str) -> Result<(), ErrorMsg> {
    if self.ast.entities.objects.contains_key(target)
      || self.ast.entities.legend.contains_key(target)
    {
      Ok(())
    } else {
      Err(format!(
        "Cannot use non-existant entity {} as a target for {}",
        target, for_
      ))
    }
  }

  fn legend_line(&mut self) -> Result<(), ErrorMsg> {
    let name = assert_found("legend name", self.lex_entity_name()?)?;
    if !self.lex_token("=")? {
      return Err("Expected = after legend name".to_string());
    };
    let desc = format!("legend {}", name);

    let target0 = assert_found(
      format!("body of legend {}", name).as_ref(),
      self.lex_entity_name()?,
    )?;

    let mut body = LegendBody::Alias(target0);

    'running: loop {
      let and = if self.lex_token("or")? {
        false
      } else if self.lex_token("and")? {
        true
      } else {
        break 'running;
      };

      let target = assert_found(
        format!("name after 'or'/'and' in legend {}", name).as_ref(),
        self.lex_entity_name()?,
      )?;
      self.validate_entity_usage(desc.as_ref(), &target)?;

      match &mut body {
        LegendBody::Alias(target0) => {
          let mut targets = HashSet::new();
          targets.insert(target0.clone());
          targets.insert(target.clone());
          body = if and {
            LegendBody::Aggregate(targets)
          } else {
            LegendBody::Property(targets)
          }
        }
        LegendBody::Aggregate(ref mut targets) => {
          if and {
            targets.insert(target);
          } else {
            return Err("Cannot mix 'and' and 'or' in legend definition".to_string());
          }
        }
        LegendBody::Property(ref mut targets) => {
          if and {
            return Err("Cannot mix 'and' and 'or' in legend definition".to_string());
          } else {
            targets.insert(target);
          }
        }
      }
    }

    self.ast.entities.legend.insert(name.clone(), body);
    self.ast.entities.legend_order.push(name);

    Ok(())
  }

  fn assert_entity_name(&mut self) -> Result<EntityName, ErrorMsg> {
    assert_found("entity name", self.lex_entity_name()?)
  }

  fn collision_layers_line(&mut self) -> Result<(), ErrorMsg> {
    let entity0 = self.assert_entity_name()?;

    let for_ = format!("collision layer definition starting with {}", entity0);

    let mut entities = Vec::new();
    entities.push(entity0);

    while self.lex_token(",")? {
      let s = assert_found(
        format!("name after ',' in {}", for_).as_ref(),
        self.lex_entity_name()?,
      )?;
      self.validate_entity_usage(for_.as_ref(), &s)?;
      entities.push(s);
    }

    self.ast.collision_layers.push(entities);

    Ok(())
  }

  fn entity_qualifier(&mut self) -> Result<Option<EntityQualifier>, ErrorMsg> {
    // force there to be a space -- otherwise `upolice` is parsed as `up` `olice`

    let mut lex_qualifier = |qualifier| {
      self
        .lex(|st| {
          if st.peek_str(qualifier) {
            st.cursor += qualifier.len();
            if st.peek_char(' ') || st.peek_char('\t') {
              st.cursor += 1;
              Ok(Some(()))
            } else {
              Ok(None)
            }
          } else {
            Ok(None)
          }
        })
        .map(|mb| mb.is_some())
    };

    Ok(if lex_qualifier(">")? {
      Some(EntityQualifier::ArrowRight)
    } else if lex_qualifier("<")? {
      Some(EntityQualifier::ArrowLeft)
    } else if lex_qualifier("^")? {
      Some(EntityQualifier::ArrowUp)
    } else if lex_qualifier("v")? {
      Some(EntityQualifier::ArrowDown)
    } else if lex_qualifier("up")? {
      Some(EntityQualifier::Up)
    } else if lex_qualifier("down")? {
      Some(EntityQualifier::Down)
    } else if lex_qualifier("left")? {
      Some(EntityQualifier::Left)
    } else if lex_qualifier("right")? {
      Some(EntityQualifier::Right)
    } else if lex_qualifier("moving")? {
      Some(EntityQualifier::Moving)
    } else if lex_qualifier("stationary")? {
      Some(EntityQualifier::Stationary)
    } else if lex_qualifier("action")? {
      Some(EntityQualifier::Action)
    } else if lex_qualifier("no")? {
      Some(EntityQualifier::No)
    } else if lex_qualifier("randomdir")? {
      Some(EntityQualifier::RandomDir)
    } else if lex_qualifier("random")? {
      Some(EntityQualifier::Random)
    } else if lex_qualifier("perpendicular")? {
      Some(EntityQualifier::Perpendicular)
    } else if lex_qualifier("parallel")? {
      Some(EntityQualifier::Parallel)
    } else if lex_qualifier("orthogonal")? {
      Some(EntityQualifier::Orthogonal)
    } else {
      None
    })
  }

  fn lex_sound_fx(&mut self) -> Result<Option<SoundFx>, ErrorMsg> {
    if self.peek_str("sfx") && satisfies_char(&self.chars, self.cursor + 3, &|ch| ch.is_digit(10)) {
      self.cursor += 3;
      let ix_str: String = self
        .lex_satisfy(&|ch| ch.is_digit(10))?
        .unwrap()
        .iter()
        .collect();
      match u8::from_str_radix(ix_str.as_ref(), 10) {
        Ok(ix) if ix <= 10 => Ok(Some(ix)),
        _ => Err(format!("Invalid sound fx sfx{}", ix_str)),
      }
    } else {
      Ok(None)
    }
  }

  fn rule_cell(&mut self) -> Result<(CellMatcher<()>, Vec<SoundFx>), ErrorMsg> {
    if self.lex_token("...")? {
      // TODO technically I should be able to parse sounds _and_ ellipsis in the
      // same cell
      Ok((CellMatcher::Ellipsis, Vec::new()))
    } else {
      let mut sounds = Vec::new();
      let mut entities = Vec::new();

      // the cell ends when we get a `|` or a `]`
      while !self.peek_char('|') && !self.peek_char(']') {
        let mb_qualifier = self.entity_qualifier()?;
        let mb_sound = self.lex_sound_fx()?;
        match mb_sound {
          Some(sound) => match mb_qualifier {
            None => sounds.push(sound),
            Some(_) => return Err("Cannot give qualifier to sound".to_string()),
          },
          None => {
            let entity = assert_found("entity name", self.lex_entity_name()?)?;
            self.validate_entity_usage("rule", &entity)?;
            entities.push(MatchEntity {
              qualifier: mb_qualifier,
              entity,
            });
          }
        }
      }

      Ok((CellMatcher::Entities(entities, ()), sounds))
    }
  }

  fn rule_matcher(&mut self) -> Result<Option<(ParsedMatcher, Vec<SoundFx>)>, ErrorMsg> {
    if self.lex_token("[")? {
      let mut sounds = Vec::new();
      let mut cells = Vec::new();
      let mut first = true;

      while !self.lex_token("]")? {
        if !first && !self.lex_token("|")? {
          return Err("Expecting | after entity not followed by ]".to_string());
        }
        first = false;

        let (cell, mut cell_sounds) = self.rule_cell()?;
        cells.push(cell);
        sounds.append(&mut cell_sounds);
      }

      // if we got nothing, it's one empty cell.
      if cells.is_empty() {
        cells.push(CellMatcher::Entities(Vec::new(), ()));
      }

      Ok(Some((cells, sounds)))
    } else {
      Ok(None)
    }
  }

  #[allow(clippy::type_complexity)]
  fn rule_body(&mut self) -> Result<Option<(Vec<ParsedMatcher>, Vec<SoundFx>)>, ErrorMsg> {
    match self.rule_matcher()? {
      None => Ok(None),
      Some((matcher0, mut sounds)) => {
        let mut matchers = Vec::new();
        matchers.push(matcher0);
        'running: loop {
          match self.rule_matcher()? {
            None => break 'running,
            Some((matcher, mut matcher_sounds)) => {
              matchers.push(matcher);
              sounds.append(&mut matcher_sounds);
            }
          }
        }

        Ok(Some((matchers, sounds)))
      }
    }
  }

  fn lex_rule_direction(&mut self) -> Result<Option<RuleDirection>, ErrorMsg> {
    Ok(if self.lex_token("up")? {
      Some(RuleDirection::Up)
    } else if self.lex_token("down")? {
      Some(RuleDirection::Down)
    } else if self.lex_token("left")? {
      Some(RuleDirection::Left)
    } else if self.lex_token("right")? {
      Some(RuleDirection::Right)
    } else if self.lex_token("horizontal")? {
      Some(RuleDirection::Horizontal)
    } else if self.lex_token("vertical")? {
      Some(RuleDirection::Vertical)
    } else {
      None
    })
  }

  fn lex_rule_command(&mut self) -> Result<Option<RuleCommand>, ErrorMsg> {
    Ok(if self.lex_token("message")? {
      Some(RuleCommand::Message(Rc::from(self.consume_line(true)?)))
    } else if self.lex_token("cancel")? {
      Some(RuleCommand::Cancel)
    } else if self.lex_token("restart")? {
      Some(RuleCommand::Restart)
    } else if self.lex_token("again")? {
      Some(RuleCommand::Again)
    } else {
      None
    })
  }

  #[allow(clippy::useless_let_if_seq)]
  fn rule_line(&mut self) -> Result<(), ErrorMsg> {
    let line_number = compute_source_loc(&self.chars, self.cursor).0;

    let mut late = false;
    let mut random = false;
    let mut grouped = false;
    if self.lex_token("+")? {
      grouped = true;
    } else {
      // TODO this allows you to specify late / random multiple times, we should
      // probably make it stricter to only allow one declaration each.
      'running: loop {
        if self.lex_token("late")? {
          late = true;
        } else if self.lex_token("random")? {
          random = true;
        } else {
          break 'running;
        }
      }
    }

    let direction = self.lex_rule_direction()?;

    let (lhs_matchers, lhs_sounds) = assert_found("LHS of rule", self.rule_body()?)?;
    if !lhs_sounds.is_empty() {
      return Err("Did not expect sounds on LHS of rule".to_string());
    }
    if !self.lex_token("->")? {
      return Err("Expecting -> between LHS and RHS of rule".to_string());
    }
    let mb_rhs = self.rule_body()?;

    let mut commands = Vec::new();
    commands.append(&mut self.lex_many(|st| {
      st.lex_sound_fx()
        .map(|mb_sound| mb_sound.map(RuleCommand::Sound))
    })?);
    commands.append(&mut self.lex_rule_command()?.into_iter().collect());

    let body = match mb_rhs {
      None => RuleBody::NoConsequence(lhs_matchers),

      Some((rhs_matchers, rhs_sounds)) => {
        commands.append(&mut rhs_sounds.into_iter().map(RuleCommand::Sound).collect());

        let mut matchers = Vec::new();

        if lhs_matchers.len() != rhs_matchers.len() {
          return Err(format!(
            "Rules must have the same number of matchers -- got {} and {}",
            lhs_matchers.len(),
            rhs_matchers.len(),
          ));
        }

        for (lhs_matcher, rhs_matcher) in lhs_matchers.into_iter().zip(rhs_matchers.into_iter()) {
          if lhs_matcher.len() != rhs_matcher.len() {
            return Err(format!(
              "Rules must have the same number of cells for each matcher -- got {:?} and {:?}",
              lhs_matcher, rhs_matcher
            ));
          }

          let mut matcher = Vec::new();
          for (lhs_cell, rhs_cell) in lhs_matcher.into_iter().zip(rhs_matcher.into_iter()) {
            match lhs_cell {
              CellMatcher::Ellipsis => match rhs_cell {
                CellMatcher::Ellipsis => matcher.push(CellMatcher::Ellipsis),
                CellMatcher::Entities(entities, ()) => {
                  return Err(format!(
                    "Got ellipsis on the LHS but entities on the RHS: {:?}",
                    entities
                  ));
                }
              },
              CellMatcher::Entities(lhs_entities, ()) => match rhs_cell {
                CellMatcher::Ellipsis => {
                  return Err(format!(
                    "Got entities on the RHS: {:?}, but got ellipsis on the RHS",
                    lhs_entities
                  ));
                }
                CellMatcher::Entities(rhs_entities, ()) => {
                  matcher.push(CellMatcher::Entities(lhs_entities, rhs_entities));
                }
              },
            }
          }
          matchers.push(matcher);
        }

        RuleBody::Normal(matchers)
      }
    };

    let rule = Rule {
      line_number,
      direction,
      body,
      commands,
    };

    if grouped {
      match self.ast.rules.last_mut() {
        None => {
          return Err("Trying to group rule, but we have no rule before us".to_string());
        }
        Some(prev_rule_group) => prev_rule_group.rules.push(rule),
      }
    } else {
      self.ast.rules.push(RuleGroup {
        late,
        random,
        rules: vec![rule],
      });
    }

    Ok(())
  }

  fn win_conditions_line(&mut self) -> Result<(), ErrorMsg> {
    let win_condition = if self.lex_token("no")? {
      let entity = self.assert_entity_name()?;
      if self.lex_token("on")? {
        let entity_on = self.assert_entity_name()?;
        Ok(WinCondition::NoOn(entity, entity_on))
      } else {
        Ok(WinCondition::No(entity))
      }
    } else if self.lex_token("all")? {
      let entity = self.assert_entity_name()?;
      if self.lex_token("on")? {
        let entity_on = self.assert_entity_name()?;
        Ok(WinCondition::AllOn(entity, entity_on))
      } else {
        Err("Expected 'on' in 'all' win condition")
      }
    } else if self.lex_token("some")? {
      let entity = self.assert_entity_name()?;
      if self.lex_token("on")? {
        let entity_on = self.assert_entity_name()?;
        Ok(WinCondition::SomeOn(entity, entity_on))
      } else {
        Ok(WinCondition::Some(entity))
      }
    } else {
      Err("Bad win condition statement")
    };

    self.ast.win_conditions.push(win_condition?);
    Ok(())
  }

  fn levels_line(&mut self) -> Result<Option<Vec<EntityName>>, ErrorMsg> {
    let line: Vec<EntityName> = self
      .consume_line(false)?
      .chars()
      .map(|ch| Rc::from(ch.to_string()))
      .collect();
    for entity in line.iter() {
      self.validate_entity_usage("level entity", &entity)?;
    }

    if line.is_empty() {
      Ok(None)
    } else {
      Ok(Some(line))
    }
  }

  fn levels_block(&mut self) -> Result<(), ErrorMsg> {
    if self.lex_token("message")? {
      let msg = self.consume_line(true)?;
      self.ast.levels.push(Level::Message(Rc::from(msg)));
      Ok(())
    } else {
      self.discard_spaces()?;
      let line0 = assert_found("level line", self.levels_line()?)?;
      let line_len = line0.len();
      self.assert_end_of_line("first level line")?;

      let mut lines = Vec::new();
      lines.push(line0);

      'running: loop {
        self.discard_spaces()?;
        match self.levels_line()? {
          None => {
            break 'running;
          }
          Some(line) => {
            if line.len() == line_len {
              self.assert_end_of_line("level line")?;
              lines.push(line)
            } else {
              return Err(format!(
                "Got level line of width {}, but expected it of width {}",
                line.len(),
                line_len
              ));
            }
          }
        }
      }

      self.ast.levels.push(Level::Stage(Grid::generate(
        lines.len(),
        line_len,
        |(row, col)| lines[row][col].clone(),
      )));
      Ok(())
    }
  }

  fn run(&mut self) -> Result<(), ErrorMsg> {
    'running: loop {
      self.discard_spaces()?; // remove leading whitespace

      // stop if we're done
      if self.at_eof() {
        break 'running;
      }

      // skip =s leaded lines
      if self.peek_char('=') {
        self.discard_line();
        continue 'running;
      }

      // skip empty lines
      if self.peek_char('\n') {
        self.cursor += 1;
        continue 'running;
      }

      // switch mode if necessary
      if let Some(section) = self.lex_section_start()? {
        match self.section.next() {
          None => {
            return Err(format!(
              "Expecting no section after {:?}, but got {:?}.",
              self.section, section
            ));
          }
          Some(next_section) => {
            if next_section == section {
              self.section = section;
              continue 'running;
            } else {
              return Err(format!(
                "Expecting section {:?} after section {:?}, but got {:?}.",
                self.section.next(),
                self.section,
                section
              ));
            }
          }
        }
      }

      // finally, parse line
      match self.section {
        Section::Prelude => self.prelude_line()?,
        Section::Objects => self.objects_block()?,
        Section::Legend => self.legend_line()?,
        Section::Sounds => self.discard_line(), // we do not support sounds
        Section::CollisionLayers => self.collision_layers_line()?,
        Section::Rules => self.rule_line()?,
        Section::WinConditions => self.win_conditions_line()?,
        Section::Levels => self.levels_block()?,
      }

      let for_ = format!("line in section {}", self.section);
      self.assert_end_of_line(&for_)?;
    }

    Ok(())
  }
}

#[derive(Debug, Fail)]
#[fail(display = "Parse error {} at line {}, column {}", msg, line, column)]
pub struct ParseError {
  pub msg: String,
  pub line: usize,
  pub column: usize,
}

fn compute_source_loc(chars: &[char], cursor: usize) -> (usize, usize) {
  let mut line = 1;
  let mut column = 1;
  for ch in chars.iter().take(cursor) {
    if *ch == '\n' {
      line += 1;
      column = 1;
    } else {
      column += 1;
    }
  }
  (line, column)
}

pub fn parse(str: &str) -> Result<Ast, ParseError> {
  let mut st = ParseState {
    cursor: 0,
    chars: str.to_lowercase().chars().collect(),
    ast: Ast::empty(),
    section: Section::Prelude,
    original_src: str,
  };
  match st.run() {
    Err(msg) => {
      let (line, column) = compute_source_loc(&st.chars, st.cursor);
      Err(ParseError { msg, line, column })
    }
    Ok(()) => Ok(st.ast),
  }
}
