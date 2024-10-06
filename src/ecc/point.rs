use super::*;

use num_traits::ToBytes;

pub trait ECPointTrait<'a>: PartialEq + Clone + Copy + ToBytes {}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum ECPoint<T> {
    #[default]
    Infinity,
    Coordinate(T),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FFECStandardPoint<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> {
    pub(crate) point: ECPoint<(PFInt<'a, T>, PFInt<'a, T>)>,
    pub(crate) curve: &'a C,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FFECCompressedPoint<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> {
    pub(crate) point: ECPoint<(PFInt<'a, T>, bool)>,
    pub(crate) curve: &'a C,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FFECProjectivePoint<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> {
    pub(crate) point: (PFInt<'a, T>, PFInt<'a, T>),
    pub(crate) curve: &'a C,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FFECCompressedProjPoint<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> {
    pub(crate) point: PFInt<'a, T>,
    pub(crate) curve: &'a C,
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ECPointTrait<'a>
    for FFECStandardPoint<'a, C, T>
{
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ToBytes
    for FFECStandardPoint<'a, C, T>
{
    type Bytes = Vec<u8>;

    fn to_be_bytes(&self) -> Self::Bytes {
        todo!()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        todo!()
    }
}

impl<'a, C: 'a + FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ECPointTrait<'a>
    for FFECCompressedPoint<'a, C, T>
{
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ToBytes
    for FFECCompressedPoint<'a, C, T>
{
    type Bytes = Vec<u8>;

    fn to_be_bytes(&self) -> Self::Bytes {
        todo!()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        todo!()
    }
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ECPointTrait<'a>
    for FFECProjectivePoint<'a, C, T>
{
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ToBytes
    for FFECProjectivePoint<'a, C, T>
{
    type Bytes = Vec<u8>;

    fn to_be_bytes(&self) -> Self::Bytes {
        todo!()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        todo!()
    }
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ECPointTrait<'a>
    for FFECCompressedProjPoint<'a, C, T>
{
}

impl<'a, C: FFEllipticCurveTrait<'a, T>, T: UintArrTrait> ToBytes
    for FFECCompressedProjPoint<'a, C, T>
{
    type Bytes = <PFInt<'a, T> as ToBytes>::Bytes;

    fn to_be_bytes(&self) -> Self::Bytes {
        self.point.to_be_bytes()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        self.point.to_le_bytes()
    }
}
