#![feature(duration_as_u128)]
#![feature(stmt_expr_attributes)]

extern crate failure;
extern crate freetype;
extern crate gleam;
extern crate glutin;
extern crate im_rc;
extern crate image;
extern crate lazy_static;
extern crate nalgebra;
extern crate structopt;

use structopt::StructOpt;

pub mod math;
pub mod puzzlescript;
pub mod render;

pub fn run() -> Result<(), failure::Error> {
    puzzlescript::run(puzzlescript::Opts::from_args())
}
