use std::ops::{Index, IndexMut};
use std::slice::{Iter, IterMut};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Grid<A> {
  cols: usize,
  rows: usize,
  cells: Vec<A>,
}

impl<A> Grid<A> {
  pub fn new(rows: usize, cols: usize, def: &A) -> Grid<A>
  where
    A: Clone,
  {
    if rows == 0 || cols == 0 {
      panic!(
        "Expecting non-zero dimensions for grid, got {}, {}",
        rows, cols
      )
    }
    let mut cells = Vec::new();
    for _ix in 0..rows * cols {
      cells.push(def.clone());
    }
    Grid { rows, cols, cells }
  }

  pub fn generate<F>(rows: usize, cols: usize, f: F) -> Grid<A>
  where
    F: Fn((usize, usize)) -> A,
  {
    if rows == 0 || cols == 0 {
      panic!(
        "Expecting non-zero dimensions for grid, got {}, {}",
        rows, cols
      )
    }
    let mut cells = Vec::new();
    for row in 0..rows {
      for col in 0..cols {
        cells.push(f((row, col)));
      }
    }
    Grid { rows, cols, cells }
  }

  pub fn ncols(&self) -> usize {
    self.cols
  }

  pub fn nrows(&self) -> usize {
    self.rows
  }

  pub fn iter(&self) -> Iter<A> {
    self.cells.iter()
  }

  pub fn iter_mut(&mut self) -> IterMut<A> {
    self.cells.iter_mut()
  }

  fn internal_index(&self, row: usize, col: usize) -> usize {
    if row >= self.rows {
      panic!("Row {} out of bound (rows: {})", row, self.rows);
    }
    if col >= self.cols {
      panic!("Col {} out of bound (cols: {})", col, self.cols);
    }
    row * self.cols + col
  }
}

impl<A> Index<(usize, usize)> for Grid<A> {
  type Output = A;

  fn index(&self, pos: (usize, usize)) -> &A {
    &self.cells[self.internal_index(pos.0, pos.1)]
  }
}

impl<A> IndexMut<(usize, usize)> for Grid<A> {
  fn index_mut<'a>(&'a mut self, pos: (usize, usize)) -> &'a mut A {
    let ix = self.internal_index(pos.0, pos.1);
    &mut self.cells[ix]
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SliceAxis {
  Row,
  Col,
}

impl SliceAxis {
  pub fn len<A>(&self, grid: &Grid<A>) -> usize {
    match self {
      SliceAxis::Row => grid.ncols(),
      SliceAxis::Col => grid.nrows(),
    }
  }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct SliceDesc {
  pub axis: SliceAxis,
  pub reversed: bool,
  pub index: usize,
}

impl SliceDesc {
  pub fn col(row: usize) -> SliceDesc {
    SliceDesc {
      axis: SliceAxis::Col,
      reversed: false,
      index: row,
    }
  }

  pub fn row(col: usize) -> SliceDesc {
    SliceDesc {
      axis: SliceAxis::Row,
      reversed: false,
      index: col,
    }
  }

  pub fn reverse(mut self) -> SliceDesc {
    self.reversed = !self.reversed;
    self
  }

  pub fn grid_coords<A>(&self, grid: &Grid<A>, ix: usize) -> (usize, usize) {
    match self.axis {
      SliceAxis::Row => {
        let row = self.index;
        let col = if self.reversed {
          grid.ncols() - 1 - ix
        } else {
          ix
        };
        (row, col)
      }
      SliceAxis::Col => {
        let row = if self.reversed {
          grid.nrows() - 1 - ix
        } else {
          ix
        };
        let col = self.index;
        (row, col)
      }
    }
  }
}

pub struct Slice<'a, A> {
  pub grid: &'a Grid<A>,
  pub desc: SliceDesc,
}

impl<'a, A> Slice<'a, A> {
  pub fn len(&self) -> usize {
    self.desc.axis.len(self.grid)
  }
}

impl<'a, A> Index<usize> for Slice<'a, A> {
  type Output = A;

  fn index(&self, ix: usize) -> &A {
    &self.grid[self.desc.grid_coords(self.grid, ix)]
  }
}

pub struct SliceMut<'a, A> {
  pub grid: &'a mut Grid<A>,
  pub desc: SliceDesc,
}

impl<'a, A> SliceMut<'a, A> {
  pub fn len(&self) -> usize {
    self.desc.axis.len(self.grid)
  }
}

impl<'a, A> Index<usize> for SliceMut<'a, A> {
  type Output = A;

  fn index(&self, ix: usize) -> &A {
    &self.grid[self.desc.grid_coords(self.grid, ix)]
  }
}

impl<'a, A> IndexMut<usize> for SliceMut<'a, A> {
  fn index_mut(&mut self, ix: usize) -> &mut A {
    let coords = self.desc.grid_coords(self.grid, ix);
    &mut self.grid[coords]
  }
}
