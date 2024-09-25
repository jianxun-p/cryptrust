use super::*;

pub use ffint::FiniteFieldIntTrait;
use le_int_arr::OpaqueUintTrait;
use num_traits::{CheckedAdd, CheckedSub, Inv, ToBytes};
use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

pub trait PrimeFieldIntTrait<'a, T: OpaqueUintTrait, PF: 'a + PrimeFieldTrait<T>>:
    FiniteFieldIntTrait<'a, T, PF> + Inv
{
}

#[derive(Clone, Copy)]
pub struct PFInt<'a, T: OpaqueUintTrait> {
    data: T,
    field: &'a PrimeField<T>,
}

impl<T: OpaqueUintTrait> std::fmt::Debug for PFInt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "PFInt {{ data: {} }} ",
            // "PFInt {{ data: {}, field: {} }} ",
            self.reduce().data.to_string(),
            // self.field.to_string(),
            // self.len.to_string(),
            // self.modinv.to_string(),
        ))
    }
}

impl<T: OpaqueUintTrait> std::fmt::Display for PFInt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reduced = self.reduce();
        f.write_fmt(format_args!("PFInt {{{}}} ", reduced.data.to_string()))
    }
}

impl<T: OpaqueUintTrait> PFInt<'_, T> {
    fn internal_addsub<O: Fn(&T, &T) -> (T, bool)>(mut self, rhs: &Self, op: O) -> Self {
        let mut overflowed;
        (self.data, overflowed) = op(&self.data, &rhs.data);
        // let carry_mask = T::bit_to_mask(overflowed);
        // let masked_op = |a: &T, b: &T| op(a, &b.bitand(carry_mask));
        // (self.data, overflowed) = masked_op(&self.data, &self.field.len);
        while overflowed {
            (self.data, overflowed) = op(&self.data, &self.field.len);
        }
        self
    }

    fn internal_mul(mut self, rhs: Self) -> Self {
        let mut ans = Self::zero(self.field);
        // self = self.reduce();
        let rhs_opaqueint = rhs.to_opaqueuint();
        for i in 0..T::bits() {
            match rhs_opaqueint.bit(i) {
                Some(bit) => {
                    let mask = T::bit_to_mask(bit);
                    ans = ans
                        .internal_addsub(&self, |a: &T, b: &T| a.overflowing_add(&b.bitand(mask)));
                    self = self.double();
                }
                None => {
                    self = self.double();
                }
            };
        }
        ans
    }
}

impl<T: OpaqueUintTrait> Inv for PFInt<'_, T> {
    type Output = Self;
    /// the caller is responsible for ensuring that the value is not zero under the current field
    fn inv(mut self) -> Self::Output {
        let (_, t, gcd, t_is_positive) = T::eea(&self.field.prime, &self.data);
        assert!(gcd.is_one(), "modular inverse of zero does not exists");
        let t_neg = self.field.prime - t;
        self.data = [t_neg, t][t_is_positive as usize];
        self
    }
}

impl<T: OpaqueUintTrait> ToBytes for PFInt<'_, T> {
    type Bytes = <T as ToBytes>::Bytes;

    fn to_le_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_le_bytes()
    }

    fn to_be_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_be_bytes()
    }
}

impl<T: OpaqueUintTrait> Add for PFInt<'_, T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_add)
    }
}

impl<T: OpaqueUintTrait> Sub for PFInt<'_, T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_sub)
    }
}

impl<T: OpaqueUintTrait> Mul for PFInt<'_, T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_mul(rhs)
    }
}

impl<T: OpaqueUintTrait> Neg for PFInt<'_, T> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self::zero(self.field) - self
    }
}

impl<T: OpaqueUintTrait> PartialOrd for PFInt<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let reduced = (self.clone().reduce(), other.clone().reduce());
        match reduced.0.field == reduced.1.field {
            true => self.data.partial_cmp(&other.data),
            false => None,
        }
    }
}

impl<T: OpaqueUintTrait> PartialEq for PFInt<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        if self.field != other.field {
            return false;
        }
        let bytes = (self.to_le_bytes(), other.to_le_bytes());
        let iters = (bytes.0.as_ref().iter(), bytes.1.as_ref().iter());
        iters
            .0
            .zip(iters.1)
            .fold(true, |acc, (a, b)| acc && a.eq(b))
    }
}

impl<T: OpaqueUintTrait> CheckedAdd for PFInt<'_, T> {
    fn checked_add(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() + v.clone()),
            false => None,
        }
    }
}

impl<T: OpaqueUintTrait> CheckedSub for PFInt<'_, T> {
    fn checked_sub(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() - v.clone()),
            false => None,
        }
    }
}

impl<'a, T: OpaqueUintTrait> FiniteFieldIntTrait<'a, T, PrimeField<T>> for PFInt<'a, T> {
    fn from_le_bytes(data: &[u8], field: &'a PrimeField<T>) -> Self {
        let num = T::from_le_bytes(data);
        Self {
            data: num,
            field: field,
        }
    }

    fn from_be_bytes(data: &[u8], field: &'a PrimeField<T>) -> Self {
        let num = T::from_be_bytes(data);
        Self {
            data: num,
            field: field,
        }
    }

    fn zero(field: &'a PrimeField<T>) -> Self {
        Self {
            data: T::ZERO,
            field: field,
        }
    }

    fn one(field: &'a PrimeField<T>) -> Self {
        Self {
            data: T::ONE,
            field: field,
        }
    }

    fn double(mut self) -> Self {
        let mut overflowed;
        (self.data, overflowed) = self.data.overflowing_double();
        // let carry_mask = T::bit_to_mask(overflowed);
        // let masked_op = |a: &T, b: &T| T::overflowing_add(a, &b.bitand(carry_mask));
        // (self.data, overflowed) = masked_op(&self.data, &self.field.len);
        while overflowed {
            (self.data, overflowed) = T::overflowing_add(&self.data, &self.field.len);
        }
        self
    }

    fn reduce(mut self) -> Self {
        self.data = self.data.rem_euclid(&self.field.prime);
        self
    }

    fn field(&self) -> &'a PrimeField<T> {
        self.field
    }

    fn new(field: &'a PrimeField<T>, val: T) -> Self {
        Self { data: val, field }
    }

    fn to_opaqueuint(&self) -> T {
        self.reduce().data
    }
}

impl<'a, T: OpaqueUintTrait> PrimeFieldIntTrait<'a, T, PrimeField<T>> for PFInt<'a, T> {}
