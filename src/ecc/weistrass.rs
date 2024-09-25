use num_traits::Inv;

use crate::finite_field::{
    ffint::FiniteFieldIntTrait, le_int_arr::OpaqueUintTrait, pfint::PFInt, PrimeField,
};

use super::{point::*, *};

#[derive(Debug, Clone, Copy, PartialEq)]
/// y^2 = x^3 + Ax + B
pub struct WeistrassCurve<'a, T: OpaqueUintTrait> {
    a: PFInt<'a, T>,
    b: PFInt<'a, T>,
    prime_field: &'a PrimeField<T>,
}

impl<'a, T: OpaqueUintTrait> WeistrassCurve<'a, T> {
    pub fn new(a: PFInt<'a, T>, b: PFInt<'a, T>, prime_field: &'a PrimeField<T>) -> Self {
        Self {
            a: a,
            b: b,
            prime_field: prime_field,
        }
    }

    pub fn new_point(&'a self, x: &[u8], y: &[u8]) -> FFECStandardPoint<'a, Self, T> {
        let x_int: PFInt<T> = PFInt::from_le_bytes(x, self.prime_field);
        let y_int: PFInt<T> = PFInt::from_le_bytes(y, self.prime_field);
        FFECStandardPoint {
            curve: self,
            point: ECPoint::Coordinate((x_int, y_int)),
        }
    }

    fn internal_add(
        &self,
        p: (PFInt<'a, T>, PFInt<'a, T>),
        q: (PFInt<'a, T>, PFInt<'a, T>),
    ) -> ECPoint<(PFInt<'a, T>, PFInt<'a, T>)> {
        let ((x1, y1), (x2, y2)) = (p, q);
        let (lambda, c) = match (x1 == x2, y1 == y2, y1.is_zero()) {
            (true, false, _) | (true, true, true) => {
                return ECPoint::Infinity;
            }
            (false, _, _) => {
                // x1 != x2, and 0 <= x1, x2 < prime, then gcd(x2-x1, prime) = 1
                // therefore, exists an unique multiplicative inverse
                let inv = (x2 - x1).inv();
                ((y2 - y1) * inv, (y1 * x2 - y2 * x1) * inv)
            }
            (true, true, false) => {
                // y1 != 0, and 0 <= y1 < prime, then gcd(y1, prime) = 1
                // therefore, exists an unique multiplicative inverse
                let two: PFInt<T> = PFInt::new(
                    self.prime_field,
                    T::from_word(<T::WORD>::from_le_bytes(&[2u8])),
                );
                let three: PFInt<T> = PFInt::new(
                    self.prime_field,
                    T::from_word(<T::WORD>::from_le_bytes(&[3u8])),
                );
                let inv = (two * y1).inv();
                let x1_sqr = x1 * x1;
                (
                    (three * x1_sqr + self.a) * inv,
                    (x1 * (self.a - x1_sqr) + two * self.b) * inv,
                )
            }
        };
        let lambda_sqr = lambda * lambda;
        let lambda_cube = lambda_sqr * lambda;
        let x3 = (lambda_sqr - x1 - x2).reduce();
        let y3 = (-lambda_cube + lambda * (x1 + x2) - c).reduce();
        ECPoint::Coordinate((x3, y3))
    }
}

impl<'a, T: OpaqueUintTrait> FFEllipticCurveTrait<'a, T> for WeistrassCurve<'a, T> {
    type Point = FFECStandardPoint<'a, Self, T>;

    fn point_add(&'a self, p: &Self::Point, q: &Self::Point) -> Self::Point {
        match (p.point, q.point) {
            (ECPoint::Infinity, _) => q.clone(),
            (_, ECPoint::Infinity) => p.clone(),
            (ECPoint::Coordinate(p_coordinate), ECPoint::Coordinate(q_coordinate)) => {
                FFECStandardPoint {
                    curve: self,
                    point: self.internal_add(p_coordinate, q_coordinate),
                }
            }
        }
    }

    fn point_dbl(&'a self, p: &Self::Point) -> Self::Point {
        match p.point {
            ECPoint::Infinity => p.clone(),
            ECPoint::Coordinate((x, y)) => {
                match y.is_zero() {
                    true => Self::Point {
                        curve: self,
                        point: ECPoint::Infinity,
                    },
                    false => {
                        // y1 != 0, and 0 <= y1 < prime, then gcd(y1, prime) = 1
                        // therefore, exists an unique multiplicative inverse
                        let (lambda, c) = {
                            let two: PFInt<T> = PFInt::new(
                                self.prime_field,
                                T::from_word(<T::WORD>::from_le_bytes(&[2u8])),
                            );
                            let three: PFInt<T> = PFInt::new(
                                self.prime_field,
                                T::from_word(<T::WORD>::from_le_bytes(&[3u8])),
                            );
                            let inv = (two * y).inv();
                            let x_sqr = x * x;
                            (
                                (three * x_sqr + self.a) * inv,
                                (x * (self.a - x_sqr) + two * self.b) * inv,
                            )
                        };
                        let x_dbl = x.double();
                        let lambda_sqr = lambda * lambda;
                        let lambda_cube = lambda_sqr * lambda;
                        let x3 = (lambda_sqr - x_dbl).reduce();
                        let y3 = (-lambda_cube + lambda * x_dbl - c).reduce();
                        Self::Point {
                            curve: self,
                            point: ECPoint::Coordinate((x3, y3)),
                        }
                    }
                }
            }
        }
    }

    fn point_mul(&'a self, p: &Self::Point, n: FFInt<T>) -> Self::Point {
        let n_bytes: T = n.to_opaqueuint();

        let mut q = p.clone();
        let mut ans: FFECStandardPoint<WeistrassCurve<T>, T> = self.point_zero();
        for i in 0..T::bits() {
            let bit: bool = n_bytes.bit(i).unwrap();
            let inc = [self.point_zero(), q];
            ans = self.point_add(&ans, &inc[bit as usize]);
            q = self.point_dbl(&q);
        }
        ans
    }

    fn point_zero(&'a self) -> Self::Point {
        FFECStandardPoint {
            curve: self,
            point: ECPoint::Infinity,
        }
    }
}

mod test {

    #[test]
    fn weierstrass_curve_test1() {
        use super::*;
        use crate::finite_field::le_int_arr::LeIntArr;
        use crate::finite_field::FiniteField;
        use num_traits::FromBytes;
        use std::pin::pin;
        type T = LeIntArr<u8, 2>;
        let pf_bytes: [u8; 2] = [17, 0];
        let pf = pin!(PrimeField::<T>::from_le_bytes(&pf_bytes));
        let a = PFInt::new(&pf, T::from_word(0));
        let b = PFInt::new(&pf, T::from_word(7));
        let curve = WeistrassCurve::<T>::new(a, b, &pf);

        let (g1_x, g1_y): ([u8; 2], [u8; 2]) = ([15, 0], [13, 0]);
        let g1 = curve.new_point(&g1_x, &g1_y);
        let order1_bytes: [u8; 2] = [18, 0];
        let order1 = pin!(FiniteField::<T>::from_le_bytes(&order1_bytes));
        let k1 = FFInt::<T>::new(&order1, T::from_word(6));
        let (r1_x, r1_y): ([u8; 2], [u8; 2]) = ([5, 0], [8, 0]);
        let r1 = curve.new_point(&r1_x, &r1_y);
        assert_eq!(curve.point_mul(&g1, k1), r1);

        let (g2_x, g2_y): ([u8; 2], [u8; 2]) = ([15, 0], [4, 0]);
        let g2 = curve.new_point(&g2_x, &g2_y);
        let order2_bytes: [u8; 2] = [18, 0];
        let order2 = pin!(FiniteField::<T>::from_le_bytes(&order2_bytes));
        let k2 = FFInt::<T>::new(&order2, T::from_word(7));
        let (r2_x, r2_y): ([u8; 2], [u8; 2]) = ([10, 0], [2, 0]);
        let r2 = curve.new_point(&r2_x, &r2_y);
        assert_eq!(curve.point_mul(&g2, k2), r2);

        let (g3_x, g3_y): ([u8; 2], [u8; 2]) = ([5, 0], [8, 0]);
        let g3 = curve.new_point(&g3_x, &g3_y);
        let order3_bytes: [u8; 2] = [3, 0];
        let order3 = pin!(FiniteField::<T>::from_le_bytes(&order3_bytes));
        let k3 = FFInt::new(&order3, T::from_word(6));
        let r3 = curve.point_zero();
        assert_eq!(curve.point_mul(&g3, k3), r3);
    }
}
