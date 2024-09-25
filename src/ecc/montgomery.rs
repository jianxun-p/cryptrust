use super::{point::*, FFEllipticCurveTrait, FFInt};
use crate::finite_field::{
    ffint::FiniteFieldIntTrait, le_int_arr::OpaqueUintTrait, pfint::PFInt, PrimeField,
};
use num_traits::Inv;

#[derive(Debug, Clone, Copy, PartialEq)]
/// Bv^2 = u^3 + Au^2 + u
pub struct MontgomeryCurve<'a, T: OpaqueUintTrait> {
    a: PFInt<'a, T>,
    b: PFInt<'a, T>,
    prime_field: &'a PrimeField<T>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
/// Bv^2 = u^3 + Au^2 + u
pub struct CompressedMontgomeryCurve<'a, T: OpaqueUintTrait> {
    a: PFInt<'a, T>,
    b: PFInt<'a, T>,
    prime_field: &'a PrimeField<T>,
}

impl<'a, T: OpaqueUintTrait> CompressedMontgomeryCurve<'a, T> {
    pub fn new(a: PFInt<'a, T>, b: PFInt<'a, T>, prime_field: &'a PrimeField<T>) -> Self {
        Self {
            a: a,
            b: b,
            prime_field: prime_field,
        }
    }

    pub fn new_point(&'a self, x: PFInt<'a, T>) -> FFECCompressedProjPoint<'a, Self, T> {
        FFECCompressedProjPoint {
            curve: self,
            point: x,
        }
    }

    fn x_add(
        &self,
        p: (PFInt<'a, T>, PFInt<'a, T>),
        q: (PFInt<'a, T>, PFInt<'a, T>),
        d: (PFInt<'a, T>, PFInt<'a, T>),
    ) -> (PFInt<'a, T>, PFInt<'a, T>) {
        // https://eprint.iacr.org/2017/212.pdf
        let ((x_p, z_p), (x_q, z_q), (x_d, z_d)) = (p, q, d);
        let v1 = (x_p + z_p) * (x_q - z_q);
        let v2 = (x_p - z_p) * (x_q + z_q);
        let (v_sum, v_diff) = (v1 + v2, v1 - v2);
        let v3 = v_sum * v_sum;
        let v4 = v_diff * v_diff;
        let x_a = z_d * v3;
        let z_a = x_d * v4;
        (x_a, z_a)
    }

    pub(crate) fn x_dbl(&self, p: (PFInt<'a, T>, PFInt<'a, T>)) -> (PFInt<'a, T>, PFInt<'a, T>) {
        // https://eprint.iacr.org/2017/212.pdf
        let (x_p, z_p) = p;
        let (v1, v2): (PFInt<'a, T>, PFInt<'a, T>) = {
            let (sum, diff) = (x_p + z_p, x_p - z_p);
            (sum * sum, diff * diff)
        };
        let x_r: PFInt<'a, T> = v1 * v2;
        let v11 = v1 - v2;
        let two: PFInt<'_, T> = PFInt::one(self.prime_field).double();
        let a_2: PFInt<'_, T> = self.a + two;
        let inv: PFInt<'_, T> = two.double().inv();
        let v3: PFInt<'_, T> = a_2 * inv * v11 + v2;
        let z_r = v11 * v3;
        (x_r, z_r)
    }

    fn bitxor_points(
        a: (PFInt<'a, T>, PFInt<'a, T>),
        b: (PFInt<'a, T>, PFInt<'a, T>),
    ) -> (PFInt<'a, T>, PFInt<'a, T>) {
        let ((a0, a1), (b0, b1)) = (a, b);
        let ((a0_uint, a1_uint), (b0_uint, b1_uint)) = (
            (a0.to_opaqueuint(), a1.to_opaqueuint()),
            (b0.to_opaqueuint(), b1.to_opaqueuint()),
        );
        (
            PFInt::new(a0.field(), a0_uint.bitxor(b0_uint)),
            PFInt::new(b0.field(), a1_uint.bitxor(b1_uint)),
        )
    }

    fn bitand_point(a: (T, T), b: (PFInt<'a, T>, PFInt<'a, T>)) -> (PFInt<'a, T>, PFInt<'a, T>) {
        let ((a0, a1), (b0, b1)) = (a, b);
        let (b0_uint, b1_uint) = (b0.to_opaqueuint(), b1.to_opaqueuint());
        (
            PFInt::new(b0.field(), a0.bitand(b0_uint)),
            PFInt::new(b0.field(), a1.bitand(b1_uint)),
        )
    }

    fn swap_points(
        bit: bool,
        points: ((PFInt<'a, T>, PFInt<'a, T>), (PFInt<'a, T>, PFInt<'a, T>)),
    ) -> ((PFInt<'a, T>, PFInt<'a, T>), (PFInt<'a, T>, PFInt<'a, T>)) {
        let (x0, x1) = points;
        let b = (T::bit_to_mask(bit), T::bit_to_mask(bit));
        let v = Self::bitand_point(b, Self::bitxor_points(x0, x1));
        (Self::bitxor_points(x0, v), Self::bitxor_points(x1, v))
    }

    fn ladder(
        &self,
        k: FFInt<'a, T>,
        g: (PFInt<'a, T>, PFInt<'a, T>),
    ) -> ((PFInt<'a, T>, PFInt<'a, T>), (PFInt<'a, T>, PFInt<'a, T>)) {
        // https://eprint.iacr.org/2017/212.pdf

        let p_infinity = (PFInt::one(self.prime_field), PFInt::zero(self.prime_field));
        let k_is_zero = k.is_zero();

        // if k_is_zero {
        //     return (p_infinity, g);
        // }
        let (mut x0, mut x1) = (self.x_dbl(g), g);
        let k_uint = k.to_opaqueuint();
        let bit_iter = (0..FFInt::<T>::bits())
            .rev()
            .skip_while(|i| k_uint.bit(*i).unwrap() == false)
            .skip(1);
        for i in bit_iter {
            (x0, x1) = Self::swap_points(
                k_uint.bit(i).unwrap() ^ k_uint.bit(i + 1).unwrap(),
                (x0, x1),
            );
            (x0, x1) = (self.x_dbl(x0), self.x_add(x0, x1, g));
        }
        (x0, x1) = Self::swap_points(k_uint.bit(0).unwrap(), (x0, x1));
        (x0, x1) = (
            Self::swap_points(k_is_zero, (x0, p_infinity)).0,
            Self::swap_points(k_is_zero, (x1, g)).0,
        );
        (x0, x1)
    }
}

impl<'a, T: OpaqueUintTrait> FFEllipticCurveTrait<'a, T> for CompressedMontgomeryCurve<'a, T> {
    type Point = FFECCompressedProjPoint<'a, Self, T>;

    fn point_zero(&'a self) -> Self::Point {
        FFECCompressedProjPoint {
            point: PFInt::zero(self.prime_field),
            curve: self,
        }
    }

    fn point_add(&'a self, _p: &Self::Point, _q: &Self::Point) -> Self::Point {
        todo!()
    }

    fn point_dbl(&'a self, p: &Self::Point) -> Self::Point {
        let (x, z) = self.x_dbl((p.point, PFInt::one(self.prime_field)));
        FFECCompressedProjPoint {
            point: x * z.inv(),
            curve: self,
        }
    }

    fn point_mul(&'a self, p: &Self::Point, n: FFInt<'a, T>) -> Self::Point {
        let g = (p.point, PFInt::one(self.prime_field));
        let (x, z) = self.ladder(n, g).0;
        FFECCompressedProjPoint {
            point: x * z.inv(),
            curve: self,
        }
    }
}

impl<'a, T: OpaqueUintTrait> FFEllipticCurveTrait<'a, T> for MontgomeryCurve<'a, T> {
    type Point = FFECProjectivePoint<'a, Self, T>;

    fn point_zero(&'a self) -> Self::Point {
        FFECProjectivePoint {
            point: (PFInt::zero(self.prime_field), PFInt::zero(self.prime_field)),
            curve: self,
        }
    }

    fn point_add(&'a self, _p: &Self::Point, _q: &Self::Point) -> Self::Point {
        todo!()
    }

    fn point_dbl(&'a self, _p: &Self::Point) -> Self::Point {
        todo!()
    }

    fn point_mul(&'a self, _p: &Self::Point, _n: super::FFInt<'a, T>) -> Self::Point {
        todo!()
    }
}
