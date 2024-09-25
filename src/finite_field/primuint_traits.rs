use super::le_int_arr::OpaqueUintTrait;
use num_bigint::{BigUint, ToBigUint};
use num_traits::{ConstOne, FromPrimitive};

/// usize, u8, u16, u32, u64, u128
macro_rules! impl_opaqueuint_for_primuint {

    ($unsigned:ty, $signed:ty, $word_size:expr) => {

        impl OpaqueUintTrait for $unsigned {

            type WORD = u8;
            const LEN: usize = std::mem::size_of::<$unsigned>();
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
            fn from_word(val: Self::WORD) -> Self {
                Self::from_u8(val).unwrap()
            }
            fn repeat(val: Self::WORD) -> Self {
                let arr = [val; $word_size];
                Self::from_le_bytes(arr)
            }
            fn bit_to_mask(bit: bool) -> Self {
                ((((bit as Self) << (Self::BITS - 1)) as $signed) >> Self::BITS - 1) as $unsigned
            }
            fn bit(&self, n: usize) -> Option<bool> {
                match n < Self::BITS as usize {
                    true => Some(0 != (self >> n) & 1),
                    _ => None,
                }
            }
            fn set_bit(&mut self, n: usize, val: bool) {
                *self = [*self & !(Self::ONE << n), *self | (Self::ONE << n)][val as usize];
            }
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
            fn bits() -> usize {
                Self::BITS as usize
            }
            fn size() -> usize {
                std::mem::size_of::<$unsigned>()
            }
            fn from_bool(b: bool) -> Self {
                Self::from_u8(b as u8).unwrap()
            }
            fn neg(self) -> Self {
                Self::MAX - self + 1
            }
            fn to_biguint(&self) -> BigUint {
                <Self as ToBigUint>::to_biguint(self).unwrap()
            }
            fn from_biguint(biguint: BigUint) -> Self {
                let v = biguint.to_bytes_le();
                <Self as OpaqueUintTrait>::from_le_bytes(v.as_slice())
            }
        }
    };

}

macro_rules! impl_primuint_trait_helper {
    ($unsigned:ty, $signed:ty) => {
        impl_opaqueuint_for_primuint!($unsigned, $signed, std::mem::size_of::<$unsigned>());
    };
}

impl_primuint_trait_helper!(usize, isize);
impl_primuint_trait_helper!(u8, i8);
impl_primuint_trait_helper!(u16, i16);
impl_primuint_trait_helper!(u32, i32);
impl_primuint_trait_helper!(u64, i64);
// impl_primuint_trait_helper!(u128, i128);

#[cfg(test)]
#[test]
#[allow(unstable_name_collisions)]
fn test_widening_mul() {
    assert_eq!(20u8.widening_mul(&20u8), (144, 1));
    assert_eq!(255u8.widening_mul(&255u8), (1, 254));
}
