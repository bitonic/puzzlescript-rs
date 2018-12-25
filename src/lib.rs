#[macro_use]
extern crate serde_derive;

extern crate failure;
extern crate freetype;
extern crate gleam;
extern crate glutin;
extern crate im_rc;
extern crate image;
extern crate lazy_static;
extern crate nalgebra;
extern crate serde;
extern crate structopt;

use structopt::StructOpt;

pub mod grid;
pub mod math;
pub mod puzzlescript;
pub mod render;

pub fn run() -> Result<(), failure::Error> {
  puzzlescript::run(puzzlescript::Opts::from_args())
}
