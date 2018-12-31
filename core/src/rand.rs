// dead simple random number gen, taken from
// <http://git.musl-libc.org/cgit/musl/commit/?id=20d01d83b5a13c77805976e7c520f566244ba3ff>

use serde_derive::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize)]
pub struct State {
  seed: u32,
}

fn temper(mut x: u32) -> u32 {
  x ^= x.wrapping_shr(11);
  x ^= x.wrapping_shl(7) & 0x9D2C_5680;
  x ^= x.wrapping_shl(15) & 0xEFC6_0000;
  x ^= x.wrapping_shr(18);
  x
}

#[allow(clippy::should_implement_trait)]
impl State {
  pub fn new(seed: usize) -> State {
    State { seed: seed as u32 }
  }

  pub fn next(&mut self) -> u32 {
    self.seed = self.seed.wrapping_mul(1_103_515_245).wrapping_add(12_345);
    temper(self.seed) / 2
  }
}
