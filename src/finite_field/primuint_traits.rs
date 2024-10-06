use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub}, Bounded, ConstOne, ConstZero, Euclid, One, Pow, ToBytes, Unsigned, WrappingAdd, WrappingMul, WrappingSub, Zero
};
use core::ops::{BitAnd, BitOr, BitXor, Shl, Shr, Not};


pub trait PrimUint:
    PartialEq
    + Unsigned
    + Eq
    + Copy
    + Clone
    + Bounded
    + Zero
    + ConstZero
    + ConstOne
    + One
    + Not<Output = Self>
    + WrappingAdd<Output = Self>
    + WrappingSub<Output = Self>
    + WrappingMul<Output = Self>
    + OverflowingAdd
    + OverflowingSub
    + WrappingAdd
    + WrappingSub
    + Pow<usize, Output = Self>
    + ToBytes
    + Default
    + Euclid
    + Ord
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + PartialOrd
    // + From<BigUint>
    + ToString
    + Shr<usize>
    + Shl<usize>
    + std::fmt::Debug
{
    const LEN: usize;
    const SIZE: usize;
    const BITS: usize;
    const TWO: Self;
    fn from_be_bytes(bytes: &[u8]) -> Self;
    fn from_le_bytes(bytes: &[u8]) -> Self;

    #[cfg(feature = "bigint")]
    fn to_biguint(&self) -> BigUint;
    #[cfg(feature = "bigint")]
    fn from_biguint(biguint: BigUint) -> Self;

    fn neg(self) -> Self;
    fn repeat(val: u8) -> Self;
    #[inline]
    fn bit_to_mask(bit: bool) -> Self {
        Self::repeat([u8::MIN, u8::MAX][bit as usize])
    }
    fn from_bool(val: bool) -> Self;
    fn bit(&self, n: usize) -> bool;
    fn set_bit(&mut self, n: usize, val: bool);
    fn overflowing_double(self) -> (Self, bool);
    /// returns (least significant word, most significant word) of the full product
    fn widening_mul(&self, b: &Self) -> (Self, Self);
    fn eea(a: &Self, b: &Self) -> (Self, Self, Self, bool) {
        if b > a {
            let ans = Self::eea(b, a);
            return (ans.1, ans.0, ans.2, !ans.3);
        }
        let mut s_is_negative = false;
        let mut q;
        // row0: (s0, t0, r0), row1: (s0, t0, r0)
        let mut row0 = (Self::one(), Self::zero(), a.clone());
        let mut row1 = (Self::zero(), Self::one(), b.clone());
        while !row1.2.is_zero() {
            (q, row0.2) = row0.2.div_rem_euclid(&row1.2);
            (row0.0, row0.1) = {
                let (q_mul_s1, _) = q.widening_mul(&row1.0);
                let (q_mul_t1, _) = q.widening_mul(&row1.1);
                (q_mul_s1 + row0.0, row0.1 + q_mul_t1)
            };
            (row0, row1) = (row1, row0);
            s_is_negative = !s_is_negative;
        }
        (row0.0, row0.1, row0.2, s_is_negative)
    }
}

#[macro_export]
macro_rules! primuint_widening_mul {
    ($prim:ty, $lhs:expr, $rhs:expr) => {
        {
            let half_bits = <$prim>::BITS >> 1;
            let (a, b) = ($lhs >> half_bits, ($lhs << half_bits) >> half_bits);
            let (c, d) = ($rhs >> half_bits, ($rhs << half_bits) >> half_bits);
            let (mut low, high) = (b * d, a * c);
            let (mid, mut overflowed) = (a * d).overflowing_add(b * c);
            let mut carry = (overflowed as $prim) << half_bits;
            let mid_high = mid << half_bits;
            let mid_low = mid >> half_bits;
            (low, overflowed) = low.overflowing_add(mid_high);
            carry = carry + overflowed as $prim;
            (low, high + mid_low + carry)
        }
    };
}

pub(super) use primuint_widening_mul;


macro_rules! impl_opaqueuint_for_primuint {

    ($unsigned:ty, $signed:ty, $word_size:expr) => {

        impl PrimUint for $unsigned {

            const LEN: usize = $word_size;
            const SIZE: usize = $word_size;
            const BITS: usize = <$unsigned>::BITS as usize;
            const TWO: $unsigned = 2;
            #[inline]
            fn from_bool(val: bool) -> Self {
                val as $unsigned
            }
            fn from_be_bytes(bytes: &[u8]) -> Self {
                let mut arr = [0u8; $word_size];
                for i in 0..bytes.len().min($word_size) {
                    arr[i] = bytes[i];
                }
                Self::from_be_bytes(arr)
            }
            fn from_le_bytes(bytes: &[u8]) -> Self {
                let mut arr = [0u8; $word_size];
                for i in 0..bytes.len().min($word_size) {
                    arr[i] = bytes[i];
                }
                Self::from_le_bytes(arr)
            }
            #[inline]
            fn bit(&self, n: usize) -> bool {
                0 != (self >> n) & 1
            }
            fn set_bit(&mut self, n: usize, val: bool) {
                *self = [*self & !(Self::ONE << n), *self | (Self::ONE << n)][val as usize];
            }
            #[inline]
            fn overflowing_double(self) -> (Self, bool) {
                self.overflowing_add(self)
            }
            fn widening_mul(&self, rhs: &Self) -> (Self, Self) {
                const HALF_BITS: $unsigned = <$unsigned>::BITS as $unsigned >> 1;
                let (a, b): (Self, Self) = (self >> HALF_BITS, (self << HALF_BITS) >> HALF_BITS);
                let (c, d): (Self, Self) = (rhs >> HALF_BITS, (rhs << HALF_BITS) >> HALF_BITS);
                let (mut low, high): (Self, Self) = (b * d, a * c);
                let (mid, mut overflowed): (Self, bool) = (a * d).overflowing_add(b * c);
                let mut carry = overflowed as $unsigned << HALF_BITS;
                let mid_high = mid << HALF_BITS;
                let mid_low = mid >> HALF_BITS;
                (low, overflowed) = low.overflowing_add(mid_high);
                carry += overflowed as $unsigned;
                (low, high + mid_low + carry)
            }
            #[cfg(feature = "bigint")]
            fn to_biguint(&self) -> BigUint {
                <Self as ToBigUint>::to_biguint(self).unwrap()
            }
            
            #[cfg(feature = "bigint")]
            fn from_biguint(biguint: BigUint) -> Self {
                let v = biguint.to_bytes_le();
                <Self as PrimUint>::from_le_bytes(v.as_slice())
            }

            #[inline]
            fn neg(self) -> Self {
                !self + 1
            }

            fn repeat(val: u8) -> Self {
                let arr = [0u8; Self::SIZE].map(|_| val);
                Self::from_be_bytes(arr)
            }
        }
    };

}

macro_rules! impl_primuint_trait_helper {
    ($unsigned:ty, $signed:ty) => {
        impl_opaqueuint_for_primuint!($unsigned, $signed, (<$unsigned>::BITS / u8::BITS) as usize);
    };
}

impl_primuint_trait_helper!(u8, i8);
impl_primuint_trait_helper!(u16, i16);
impl_primuint_trait_helper!(u32, i32);
impl_primuint_trait_helper!(u64, i64);
impl_primuint_trait_helper!(u128, i128);
impl_primuint_trait_helper!(usize, isize);

#[cfg(test)]
#[test]
#[allow(unstable_name_collisions)]
fn test_widening_mul() {
    assert_eq!(20u8.widening_mul(&20u8), (144, 1));
    assert_eq!(255u8.widening_mul(&255u8), (1, 254));
}
