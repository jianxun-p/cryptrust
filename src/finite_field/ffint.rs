use super::*;

use le_int_arr::OpaqueUintTrait;
use num_traits::{CheckedAdd, CheckedSub, Inv, ToBytes};
use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

pub trait FiniteFieldIntTrait<'a, T: OpaqueUintTrait, F: 'a + FiniteFieldTrait<T>>:
    Clone
    + ToBytes
    + PartialEq
    + PartialOrd
    + Add<Output = Self>
    + CheckedAdd<Output = Self>
    + Sub<Output = Self>
    + CheckedSub<Output = Self>
    + Neg<Output = Self>
    + Mul<Output = Self>
{
    fn from_le_bytes(data: &[u8], field: &'a F) -> Self;
    fn from_be_bytes(data: &[u8], field: &'a F) -> Self;
    fn zero(field: &'a F) -> Self;
    fn is_zero(&self) -> bool {
        *self == Self::zero(&self.field())
    }
    fn one(field: &'a F) -> Self;
    fn is_one(&self) -> bool {
        *self == Self::one(&self.field())
    }
    fn new(field: &'a F, val: T) -> Self;
    fn double(self) -> Self;
    fn reduce(self) -> Self;
    fn field(&self) -> &'a F;
    fn to_opaqueuint(&self) -> T;
    fn bits() -> usize {
        T::bits()
    }
    fn pow(mut self, exponent: T) -> Self {
        let mut ans = Self::zero(self.field());
        for i in 0..T::bits() {
            let bytes = self.to_opaqueuint();
            let inc = T::bit_to_mask(exponent.bit(i).unwrap())
                .bitand(bytes)
                .to_le_bytes();
            ans = ans * Self::from_le_bytes(inc.as_ref(), self.field());
            self = self.clone() * self;
        }
        ans
    }
}

#[derive(Clone, Copy)]
pub struct FFInt<'a, T: OpaqueUintTrait> {
    data: T,
    field: &'a FiniteField<T>,
}

impl<T: OpaqueUintTrait> std::fmt::Debug for FFInt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "FFInt {{ data: {} }} ",
            // "FFInt {{ data: {}, field: {} }} ",
            self.reduce().data.to_string(),
            // self.field.to_string(),
            // self.len.to_string(),
            // self.modinv.to_string(),
        ))
    }
}

impl<T: OpaqueUintTrait> std::fmt::Display for FFInt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reduced = self.reduce();
        f.write_fmt(format_args!("FFInt {{{}}} ", reduced.data.to_string()))
    }
}

impl<T: OpaqueUintTrait> FFInt<'_, T> {
    fn internal_addsub<O: Fn(&T, &T) -> (T, bool)>(mut self, rhs: &Self, op: O) -> Self {
        let mut overflowed;
        (self.data, overflowed) = op(&self.data, &rhs.data);
        // let carry_mask = T::bit_to_mask(overflowed);
        // let masked_op = |a: &T, b: &T| op(a, &b.bitand(carry_mask));
        // (self.data, _) = masked_op(&self.data, &self.field.len);
        while overflowed {
            (self.data, overflowed) = op(&self.data, &self.field.len);
        }
        self
    }

    fn internal_mul(mut self, rhs: Self) -> Self {
        let mut ans = Self::zero(self.field);
        for i in 0..T::bits() {
            match rhs.data.bit(i as usize) {
                Some(bit) => {
                    let mask = T::bit_to_mask(bit);
                    ans = ans
                        .internal_addsub(&self, |a: &T, b: &T| a.overflowing_add(&b.bitand(mask)));
                    self = self.double();
                }
                None => {
                    return ans;
                }
            }
        }
        ans
    }
}

impl<T: OpaqueUintTrait> ToBytes for FFInt<'_, T> {
    type Bytes = <T as ToBytes>::Bytes;

    fn to_le_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_le_bytes()
    }

    fn to_be_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_be_bytes()
    }
}

impl<T: OpaqueUintTrait> Add for FFInt<'_, T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_add)
    }
}

impl<T: OpaqueUintTrait> Sub for FFInt<'_, T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_sub)
    }
}

impl<T: OpaqueUintTrait> Mul for FFInt<'_, T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_mul(rhs)
    }
}

impl<T: OpaqueUintTrait> Neg for FFInt<'_, T> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self::zero(self.field) - self
    }
}

impl<T: OpaqueUintTrait> PartialOrd for FFInt<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let reduced = (self.clone().reduce(), other.clone().reduce());
        match reduced.0.field == reduced.1.field {
            true => self.data.partial_cmp(&other.data),
            false => None,
        }
    }
}

impl<T: OpaqueUintTrait> PartialEq for FFInt<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        if self.field != other.field {
            return false;
        }
        let bytes = (self.to_le_bytes(), other.to_le_bytes());
        let iters = (bytes.0.as_ref().iter(), bytes.1.as_ref().iter());
        iters.0.zip(iters.1).fold(true, |acc, (a, b)| acc && a == b)
    }
}

impl<T: OpaqueUintTrait> CheckedAdd for FFInt<'_, T> {
    fn checked_add(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() + v.clone()),
            false => None,
        }
    }
}

impl<T: OpaqueUintTrait> CheckedSub for FFInt<'_, T> {
    fn checked_sub(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() - v.clone()),
            false => None,
        }
    }
}

impl<'a, T: OpaqueUintTrait> FiniteFieldIntTrait<'a, T, FiniteField<T>> for FFInt<'a, T> {
    fn from_le_bytes(data: &[u8], field: &'a FiniteField<T>) -> Self {
        let num = T::from_le_bytes(data);
        Self {
            data: num,
            field: field,
        }
    }

    fn from_be_bytes(data: &[u8], field: &'a FiniteField<T>) -> Self {
        let num = T::from_be_bytes(data);
        Self {
            data: num,
            field: field,
        }
    }

    fn zero(field: &'a FiniteField<T>) -> Self {
        Self {
            data: T::ZERO,
            field: field,
        }
    }

    fn one(field: &'a FiniteField<T>) -> Self {
        Self {
            data: T::ONE,
            field: field,
        }
    }

    fn new(field: &'a FiniteField<T>, val: T) -> Self {
        Self {
            data: val,
            field: field,
        }
    }

    fn double(mut self) -> Self {
        let mut overflowed;
        (self.data, overflowed) = self.data.overflowing_double();
        // let carry_mask = T::bit_to_mask(overflowed);
        // let masked_op = |a: &T, b: &T| T::overflowing_add(a, &b.bitand(carry_mask));
        // (self.data, _) = masked_op(&self.data, &self.field.len);
        while overflowed {
            (self.data, overflowed) = T::overflowing_add(&self.data, &self.field.len);
        }
        self
    }

    fn reduce(mut self) -> Self {
        (_, self.data) = self.data.div_rem_euclid(&self.field.modulo);
        self
    }

    fn field(&self) -> &'a FiniteField<T> {
        self.field
    }

    fn to_opaqueuint(&self) -> T {
        self.reduce().data
    }
}

impl<T: OpaqueUintTrait> Inv for FFInt<'_, T> {
    type Output = Option<Self>;

    fn inv(mut self) -> Self::Output {
        let (_, t, gcd, t_is_positive) = T::eea(&self.field.modulo, &self.data);
        match gcd.is_one() {
            false => None,
            true => {
                let t_neg = self.field.modulo - t;
                self.data = [t_neg, t][t_is_positive as usize];
                Some(self)
            }
        }
    }
}
