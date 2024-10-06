pub mod common_curves;
pub mod montgomery;
pub mod point;

#[cfg(test)]
mod test;

pub mod weistrass;

pub use common_curves::*;
use point::*;

pub(self) use crate::finite_field::{ffint::*, uint_arr::UintArrTrait};

pub trait FFEllipticCurveTrait<'a, T: UintArrTrait>: PartialEq + Clone + Copy {
    type Point: ECPointTrait<'a>;
    fn point_zero(&'a self) -> Self::Point;
    fn point_add(&'a self, p: &Self::Point, q: &Self::Point) -> Self::Point;
    fn point_dbl(&'a self, p: &Self::Point) -> Self::Point;
    fn point_mul(&'a self, p: &Self::Point, n: FFInt<'a, T>) -> Self::Point;
}
