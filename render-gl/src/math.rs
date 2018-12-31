pub use nalgebra::*;

pub fn vec2<T: 'static>(x: T, y: T) -> Vector2<T>
where
  T: std::fmt::Debug + PartialEq + Copy,
{
  nalgebra::Vector2::new(x, y)
}

#[allow(dead_code)]
pub fn vec3<T: 'static>(x: T, y: T, z: T) -> Vector3<T>
where
  T: std::fmt::Debug + PartialEq + Copy,
{
  nalgebra::Vector3::new(x, y, z)
}

pub fn vec4<T: 'static>(x: T, y: T, z: T, w: T) -> Vector4<T>
where
  T: std::fmt::Debug + PartialEq + Copy,
{
  nalgebra::Vector4::new(x, y, z, w)
}
