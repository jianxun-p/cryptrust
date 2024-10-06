use super::*;

pub use ffint::FiniteFieldIntTrait;
use uint_arr::UintArrTrait;
use num_traits::{CheckedAdd, CheckedSub, Inv, Pow, ToBytes};
use std::{
    cmp::Ordering,
    ops::{Add, Mul, Neg, Sub},
};

pub trait PrimeFieldIntTrait<'a, T: UintArrTrait, PF: 'a + PrimeFieldTrait<T>>:
    FiniteFieldIntTrait<'a, T, PF> + Inv
{
}

#[derive(Clone, Copy, Eq)]
pub struct PFInt<'a, T: UintArrTrait> {
    data: T,
    field: &'a PrimeField<T>,
}

impl<T: UintArrTrait> Pow<T> for PFInt<'_, T> {
    type Output = Self;
    fn pow(self, rhs: T) -> Self::Output {
        if rhs.is_zero() {
            Self { data: T::ONE, field: self.field }
        } else if rhs.is_one() {
            self
        } else {
            let sqr = self * self;
            match rhs.bit(0) {
                false => sqr.pow(rhs / (T::ONE + T::ONE)),
                true => self * sqr.pow(rhs / (T::ONE + T::ONE)),
            }
        }
    }
}

impl<T: UintArrTrait> std::fmt::Debug for PFInt<'_, T> {
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

impl<T: UintArrTrait> std::fmt::Display for PFInt<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reduced = self.reduce();
        f.write_fmt(format_args!("PFInt {{{}}} ", reduced.data.to_string()))
    }
}

impl<T: UintArrTrait> PFInt<'_, T> {
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
        for i in 0..T::BITS {
            let mask = T::bit_to_mask(rhs_opaqueint.bit(i));
            ans = ans
                .internal_addsub(&self, |a: &T, b: &T| a.overflowing_add(&b.bitand(mask)));
            self = self.double();
        }
        ans
    }
}

impl<T: UintArrTrait> Inv for PFInt<'_, T> {
    type Output = Self;
    /// the caller is responsible for ensuring that the value is not zero under the current field
    fn inv(mut self) -> Self::Output {
        assert!(!self.is_zero(), "modular inverse of zero does not exists");
        let (_, t, _, t_is_positive) = T::eea(self.field.prime, self.data);
        self.data = match t_is_positive {
            true => t,
            false => self.field.prime - t,
        };
        self
    }
}

impl<T: UintArrTrait> ToBytes for PFInt<'_, T> {
    type Bytes = <T as ToBytes>::Bytes;

    fn to_le_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_le_bytes()
    }

    fn to_be_bytes(&self) -> Self::Bytes {
        self.reduce().data.to_be_bytes()
    }
}

impl<T: UintArrTrait> Add for PFInt<'_, T> {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_add)
    }
}

impl<T: UintArrTrait> Sub for PFInt<'_, T> {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_addsub(&rhs, T::overflowing_sub)
    }
}

impl<T: UintArrTrait> Mul for PFInt<'_, T> {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        assert!(self.field == rhs.field);
        self.internal_mul(rhs)
    }
}

impl<T: UintArrTrait> Neg for PFInt<'_, T> {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self::zero(self.field) - self
    }
}

impl<T: UintArrTrait> PartialOrd for PFInt<'_, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let reduced = (self.clone().reduce(), other.clone().reduce());
        match reduced.0.field == reduced.1.field {
            true => self.data.partial_cmp(&other.data),
            false => None,
        }
    }
}

impl<T: UintArrTrait> PartialEq for PFInt<'_, T> {
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

impl<T: UintArrTrait> CheckedAdd for PFInt<'_, T> {
    fn checked_add(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() + v.clone()),
            false => None,
        }
    }
}

impl<T: UintArrTrait> CheckedSub for PFInt<'_, T> {
    fn checked_sub(&self, v: &Self) -> Option<Self> {
        match self.field == v.field {
            true => Some(self.clone() - v.clone()),
            false => None,
        }
    }
}

impl<'a, T: UintArrTrait> FiniteFieldIntTrait<'a, T, PrimeField<T>> for PFInt<'a, T> {
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
        (self.data, overflowed) = self.data.overflowing_add(&self.data);
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

impl<'a, T: UintArrTrait> PrimeFieldIntTrait<'a, T, PrimeField<T>> for PFInt<'a, T> {}

macro_rules! impl_const_pfint {
    ($word:ty, $len:expr, $size:expr) => {
        impl<'a> PFInt<'a, UintArr<$word, $len, $size>> {
            pub const fn const_from_opaqueuint(num: UintArr<$word, $len, $size>, field: &'a PrimeField<UintArr<$word, $len, $size>>) -> Self {
                Self { data: num, field }
            }
        }
    };
}

crate::finite_field::macro_helper::impl_trait_for_biguint!(impl_const_pfint);
