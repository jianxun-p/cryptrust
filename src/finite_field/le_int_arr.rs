// use super::OpaqueUintTrait;

pub use num_bigint::BigUint;
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub},
    Bounded, ConstOne, ConstZero, Euclid, FromBytes, One, ToBytes, WrappingAdd, WrappingSub, Zero,
};
use std::{
    cmp::Ordering,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
};

pub trait OpaqueUintTrait:
    PartialEq
    + Eq
    + Copy
    + Clone
    + Bounded
    + Zero
    + ConstZero
    + ConstOne
    + One
    + Add<Output = Self>
    + Sub<Output = Self>
    // + Neg<Output = Self>
    + Mul<Output = Self>
    + OverflowingAdd
    + OverflowingSub
    + WrappingAdd
    + WrappingSub
    // + FromBytes<Bytes = [u8]>
    + ToBytes
    + Default
    + Euclid
    + Ord
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + BitXor<Output = Self>
    + PartialOrd
    // + ToBigUint
    // + From<BigUint>
    + ToString
    // + Shr<usize>
    + std::fmt::Debug
{
    type WORD: OpaqueUintTrait;
    const LEN: usize;
    fn from_word(val: Self::WORD) -> Self;
    fn from_be_bytes(bytes: &[u8]) -> Self;
    fn from_le_bytes(bytes: &[u8]) -> Self;
    fn to_biguint(&self) -> BigUint;
    fn from_biguint(biguint: BigUint) -> Self;
    fn neg(self) -> Self;
    fn repeat(val: Self::WORD) -> Self;
    fn bit_to_mask(bit: bool) -> Self {
        Self::repeat(Self::WORD::bit_to_mask(bit))
    }
    fn bit(&self, n: usize) -> Option<bool>;
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
            std::mem::swap(&mut row0, &mut row1);
            s_is_negative = !s_is_negative;
        }
        (row0.0, row0.1, row0.2, s_is_negative)
    }
    fn bits() -> usize {
        Self::LEN * <Self::WORD as OpaqueUintTrait>::bits()
    }
    fn size() -> usize {
        Self::LEN * <Self::WORD as OpaqueUintTrait>::size()
    }
    fn from_bool(b: bool) -> Self {
        Self::from_word(Self::WORD::from_bool(b))
    }
}

pub trait ToByteArr<const S: usize> {
    fn to_le_byte_arr(&self) -> [u8; S];
    fn to_be_byte_arr(&self) -> [u8; S];
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct LeIntArr<T: OpaqueUintTrait, const L: usize> {
    pub(super) le_words: [T; L], // words in little endian
}

impl<T: OpaqueUintTrait, const L: usize> std::fmt::Debug for LeIntArr<T, L> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("LeIntArr {:?}", self.le_words))
    }
}

impl<T: OpaqueUintTrait, const L: usize> Zero for LeIntArr<T, L> {
    fn zero() -> Self {
        Self {
            le_words: [T::zero(); L],
        }
    }

    fn is_zero(&self) -> bool {
        self.le_words
            .iter()
            .fold(true, |acc, i| acc && T::zero().eq(i))
    }
}

impl<T: OpaqueUintTrait, const L: usize> ConstZero for LeIntArr<T, L> {
    const ZERO: Self = Self {
        le_words: [T::ZERO; L],
    };
}

impl<T: OpaqueUintTrait, const L: usize> One for LeIntArr<T, L> {
    fn one() -> Self {
        let mut a = Self::ZERO;
        a.le_words[0] = T::ONE;
        a
    }
}

impl<T: OpaqueUintTrait, const L: usize> ConstOne for LeIntArr<T, L> {
    const ONE: Self = {
        let mut a = Self::ZERO;
        a.le_words[0] = T::ONE;
        a
    };
}

impl<T: OpaqueUintTrait, const L: usize> Default for LeIntArr<T, L> {
    fn default() -> Self {
        Self::ZERO
    }
}

impl<T: OpaqueUintTrait, const L: usize> PartialOrd for LeIntArr<T, L> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: OpaqueUintTrait, const L: usize> Ord for LeIntArr<T, L> {
    fn cmp(&self, other: &Self) -> Ordering {
        (0..L).rev().fold(Ordering::Equal, |acc, i| match acc {
            Ordering::Equal => self.le_words[i].cmp(&other.le_words[i]),
            _ => acc,
        })
    }
}

impl<T: OpaqueUintTrait, const L: usize> Add for LeIntArr<T, L> {
    type Output = Self;

    fn add(mut self, b: Self) -> Self {
        self.overflowing_addsub(&b, &T::overflowing_add);
        self
    }
}

impl<T: OpaqueUintTrait, const L: usize> OverflowingAdd for LeIntArr<T, L> {
    fn overflowing_add(&self, v: &Self) -> (Self, bool) {
        let mut ans = self.clone();
        let overflowed = ans.overflowing_addsub(&v, &T::overflowing_add);
        (ans, overflowed)
    }
}

impl<T: OpaqueUintTrait, const L: usize> OverflowingSub for LeIntArr<T, L> {
    fn overflowing_sub(&self, v: &Self) -> (Self, bool) {
        let mut ans = self.clone();
        let overflowed = ans.overflowing_addsub(&v, &T::overflowing_sub);
        (ans, overflowed)
    }
}

impl<T: OpaqueUintTrait, const L: usize> LeIntArr<T, L> {
    fn overflowing_addsub<O: Fn(&T, &T) -> (T, bool)>(&mut self, rhs: &Self, op: &O) -> bool {
        let mut carry = false;
        for i in 0..L {
            let carry_t = T::from_bool(carry);
            let mut tmp = op(&self.le_words[i], &carry_t);
            carry = tmp.1;
            tmp = op(&tmp.0, &rhs.le_words[i]);
            (self.le_words[i], carry) = (tmp.0, tmp.1 | carry);
        }
        carry
    }
}

impl<T: OpaqueUintTrait, const L: usize> Bounded for LeIntArr<T, L> {
    fn min_value() -> Self {
        Self::ZERO
    }
    fn max_value() -> Self {
        Self {
            le_words: [T::max_value(); L],
        }
    }
}

impl<T: OpaqueUintTrait, const L: usize> FromBytes for LeIntArr<T, L> {
    type Bytes = [u8];

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let mut le_words: [T; L] = [T::zero(); L];
        for i in 0..L {
            let mut tmp: Vec<u8> = Vec::with_capacity(T::size());
            for j in 0..T::size().min(bytes.len() - i * T::size()) {
                tmp.push(bytes[i * T::size() + j]);
            }
            le_words[i] = T::from_le_bytes(tmp.as_slice());
            if bytes.len() - i * T::size() < T::size() {
                break;
            }
        }
        Self { le_words: le_words }
    }

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let mut le_words: [T; L] = [T::zero(); L];
        for i in 0..L {
            let mut tmp: Vec<u8> = Vec::with_capacity(T::size());
            for j in 0..T::size().min(bytes.len() - i * T::size()) {
                tmp.push(bytes[i * T::size() + j]);
            }
            le_words[L - i - 1] = T::from_be_bytes(tmp.as_slice());
            if bytes.len() - i * T::size() < T::size() {
                break;
            }
        }
        // dbg!(Self { le_words: le_words }.to_be_bytes());
        Self { le_words: le_words }
    }
}

impl<T: OpaqueUintTrait, const L: usize> ToBytes for LeIntArr<T, L> {
    type Bytes = Vec<u8>;

    fn to_le_bytes(&self) -> Self::Bytes {
        let mut bytes = Vec::with_capacity(L * T::bits());
        for i in 0..L {
            let tmp = self.le_words[i].to_le_bytes();
            bytes.extend_from_slice(tmp.as_ref());
        }
        bytes
    }

    fn to_be_bytes(&self) -> Self::Bytes {
        let mut bytes = Vec::with_capacity(L * T::bits());
        for i in 0..L {
            let tmp = self.le_words[L - i - 1].to_be_bytes();
            bytes.extend_from_slice(tmp.as_ref());
        }
        bytes
    }
}

impl<T: OpaqueUintTrait, const L: usize> WrappingAdd for LeIntArr<T, L> {
    fn wrapping_add(&self, b: &Self) -> Self {
        let mut a = self.clone();
        a.overflowing_addsub(b, &T::overflowing_add);
        a
    }
}

impl<T: OpaqueUintTrait, const L: usize> Sub for LeIntArr<T, L> {
    type Output = Self;

    fn sub(mut self, b: Self) -> Self {
        self.overflowing_addsub(&b, &T::overflowing_sub);
        self
    }
}

impl<T: OpaqueUintTrait, const L: usize> WrappingSub for LeIntArr<T, L> {
    fn wrapping_sub(&self, b: &Self) -> Self {
        let mut a = self.clone();
        a.overflowing_addsub(b, &T::overflowing_sub);
        a
    }
}

impl<T: OpaqueUintTrait, const L: usize> Div for LeIntArr<T, L> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        let (mut q, mut r) = (Self::ZERO, Self::ZERO);
        for i in (0..Self::bits()).rev() {
            let overflowed;
            (r, overflowed) = r.overflowing_double();
            r.set_bit(0, self.bit(i as usize).unwrap());
            let mask = T::bit_to_mask(r.ge(&rhs) | overflowed);
            let masked_sub = move |a: &T, b: &T| {
                let masked_b = *b & mask;
                a.overflowing_sub(&masked_b)
            };
            r.overflowing_addsub(&rhs, &masked_sub);
            q.set_bit(i as usize, mask != T::zero());
        }
        q
    }
}

impl<T: OpaqueUintTrait, const L: usize> Rem for LeIntArr<T, L> {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        let mut r = Self::ZERO;
        for i in (0..Self::bits()).rev() {
            let overflowed;
            (r, overflowed) = r.overflowing_double();
            r.set_bit(0, self.bit(i as usize).unwrap());
            let mask = T::bit_to_mask(r.ge(&rhs) | overflowed);
            let masked_sub = move |a: &T, b: &T| {
                let masked_b = *b & mask;
                a.overflowing_sub(&masked_b)
            };
            r.overflowing_addsub(&rhs, &masked_sub);
        }
        r
    }
}

impl<T: OpaqueUintTrait, const L: usize> Euclid for LeIntArr<T, L> {
    fn div_euclid(&self, v: &Self) -> Self {
        self.clone() / v.clone()
    }
    fn rem_euclid(&self, v: &Self) -> Self {
        self.clone() % v.clone()
    }
    fn div_rem_euclid(&self, v: &Self) -> (Self, Self) {
        let (mut q, mut r) = (Self::ZERO, Self::ZERO);
        for i in (0..Self::bits()).rev() {
            let overflowed;
            (r, overflowed) = r.overflowing_double();
            r.set_bit(0, self.bit(i as usize).unwrap());
            let mask = T::bit_to_mask(r.ge(&v) | overflowed);
            let masked_sub = move |a: &T, b: &T| {
                let masked_b = *b & mask;
                a.overflowing_sub(&masked_b)
            };
            r.overflowing_addsub(&v, &masked_sub);
            q.set_bit(i as usize, mask != T::zero());
        }
        (q, r)
    }
}

impl<T: OpaqueUintTrait, const L: usize> Mul for LeIntArr<T, L> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut ans = Self::ZERO;
        for i in 0..L {
            for j in 0..(L - i) {
                #[allow(unstable_name_collisions)]
                let (val, mut carry) = self.le_words[i].widening_mul(&rhs.le_words[j]);
                let mut overflowed;
                (ans.le_words[i + j], overflowed) = ans.le_words[i + j].overflowing_add(&val);
                let mut k = 1;
                while i + j + k < L {
                    let overflowed2: bool;
                    (carry, overflowed) = carry.overflowing_add(&T::from_bool(overflowed));
                    (ans.le_words[i + j + k], overflowed2) =
                        ans.le_words[i + j + k].overflowing_add(&carry);
                    carry = T::from_bool(overflowed) + T::from_bool(overflowed2);
                    overflowed = false;
                    k += 1;
                }
            }
        }
        ans
    }
}

impl<T: OpaqueUintTrait, const L: usize> ToString for LeIntArr<T, L> {
    fn to_string(&self) -> String {
        // let mut s = String::new();
        // let mut a = self.clone();
        // let ten_bytes = [10u8];
        // let ten: LeIntArr<T, L> = <Self as OpaqueUintTrait>::from_le_bytes(&ten_bytes);
        // while !a.is_zero() {
        //     let c = (a % ten).le_words[0].to_string();
        //     a = a / ten;
        //     s.extend(c.chars())
        // }
        // match s.is_empty() {
        //     true => String::from("0"),
        //     _ => String::from_iter(s.chars().rev()),
        // }
        self.to_biguint().to_string()
    }
}

impl<T: OpaqueUintTrait, const L: usize> BitAnd for LeIntArr<T, L> {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        for i in 0..L {
            self.le_words[i] = self.le_words[i].bitand(rhs.le_words[i]);
        }
        self
    }
}

impl<T: OpaqueUintTrait, const L: usize> BitOr for LeIntArr<T, L> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        for i in 0..L {
            self.le_words[i] = self.le_words[i].bitor(rhs.le_words[i]);
        }
        self
    }
}

impl<T: OpaqueUintTrait, const L: usize> BitXor for LeIntArr<T, L> {
    type Output = Self;

    fn bitxor(mut self, rhs: Self) -> Self::Output {
        for i in 0..L {
            self.le_words[i] = self.le_words[i].bitxor(rhs.le_words[i]);
        }
        self
    }
}

impl<T: OpaqueUintTrait, const L: usize> OpaqueUintTrait for LeIntArr<T, L> {
    type WORD = T;
    const LEN: usize = L;

    fn from_be_bytes(bytes: &[u8]) -> Self {
        <Self as FromBytes>::from_be_bytes(bytes)
    }
    fn from_le_bytes(bytes: &[u8]) -> Self {
        <Self as FromBytes>::from_le_bytes(bytes)
    }

    fn from_biguint(biguint: BigUint) -> Self {
        let v = biguint.to_bytes_le();
        <Self as OpaqueUintTrait>::from_le_bytes(v.as_slice())
    }

    fn to_biguint(&self) -> BigUint {
        let bytes = self.to_le_bytes();
        BigUint::from_bytes_le(bytes.as_slice())
    }

    fn neg(mut self) -> Self {
        for i in 0..Self::LEN {
            self.le_words[i] = Self::WORD::max_value() - self.le_words[i];
        }
        self + Self::ONE
    }

    fn from_word(val: Self::WORD) -> Self {
        let mut a = Self::zero();
        a.le_words[0] = val;
        a
    }

    fn bit(&self, n: usize) -> Option<bool> {
        let i: usize = n / (T::bits() as usize);
        let j: usize = n % (T::bits() as usize);
        match i < L {
            false => None,
            true => self.le_words[i].bit(j),
        }
    }

    fn set_bit(&mut self, n: usize, val: bool) {
        let (i, j) = n.div_rem_euclid(&T::bits());
        if i < L {
            self.le_words[i].set_bit(j, val);
        }
    }

    fn overflowing_double(mut self) -> (Self, bool) {
        let mut carry = false;
        for i in 0..L {
            let prev_carry = carry;
            (self.le_words[i], carry) = self.le_words[i].overflowing_double();
            self.le_words[i].set_bit(0, prev_carry);
        }
        (self, carry)
    }

    /// returns (least significant words, most significant words) of the full product
    fn widening_mul(&self, b: &Self) -> (Self, Self) {
        let mut ans = [Self::ZERO; 2];
        for i in 0..L {
            for j in 0..L {
                #[allow(unstable_name_collisions)]
                let (val, mut carry) = self.le_words[i].widening_mul(&b.le_words[j]);
                let mut overflowed;
                (ans[(i + j) / L].le_words[(i + j) % L], overflowed) =
                    ans[(i + j) / L].le_words[(i + j) % L].overflowing_add(&val);
                let mut k = 1;
                while i + j + k < 2 * L {
                    let overflowed2: bool;
                    (carry, overflowed) = carry.overflowing_add(&T::from_bool(overflowed));
                    (ans[(i + j + k) / L].le_words[(i + j + k) % L], overflowed2) =
                        ans[(i + j + k) / L].le_words[(i + j + k) % L].overflowing_add(&carry);
                    carry = T::from_bool(overflowed) + T::from_bool(overflowed2);
                    overflowed = false;
                    k += 1;
                }
            }
        }
        // (least significant words, most significant words)
        (ans[0], ans[1])
    }

    fn repeat(val: Self::WORD) -> Self {
        Self { le_words: [val; L] }
    }
}

macro_rules! impl_to_bytes_arr_helper {
    ($word:ty, $len:expr, $size:expr) => {
        impl ToByteArr<$size> for LeIntArr<$word, $len> {
            fn to_le_byte_arr(&self) -> [u8; $size] {
                let mut bytes = [0u8; $size];
                for i in 0..$len {
                    let tmp = self.le_words[i].to_le_bytes();
                    let word_size = tmp.len();
                    for j in 0..word_size {
                        bytes[i * word_size + j] = tmp[j];
                    }
                }
                bytes
            }
            fn to_be_byte_arr(&self) -> [u8; $size] {
                let mut bytes = [0u8; $size];
                for i in 0..$len {
                    let tmp = self.le_words[$len - i - 1].to_be_bytes();
                    let word_size = tmp.len();
                    for j in 0..word_size {
                        bytes[i * word_size + j] = tmp[j];
                    }
                }
                bytes
            }
        }
    };
}

macro_rules! impl_to_bytes_arr {
    ($word:ty, $len:literal) => {
        impl_to_bytes_arr_helper!($word, $len, { $len * std::mem::size_of::<$word>() });
    };
}

macro_rules! impl_to_bytes_arr_for_len {
    ($len:literal) => {
        impl_to_bytes_arr!(usize, $len);
        impl_to_bytes_arr!(u8, $len);
        impl_to_bytes_arr!(u16, $len);
        impl_to_bytes_arr!(u32, $len);
        impl_to_bytes_arr!(u64, $len);
        // impl_to_bytes_arr!(u128, $len);
    };
}

impl_to_bytes_arr_for_len!(1);
impl_to_bytes_arr_for_len!(2);
impl_to_bytes_arr_for_len!(4);
impl_to_bytes_arr_for_len!(7);
impl_to_bytes_arr_for_len!(8);
impl_to_bytes_arr_for_len!(14);
impl_to_bytes_arr_for_len!(16);
impl_to_bytes_arr_for_len!(28);
impl_to_bytes_arr_for_len!(32);
impl_to_bytes_arr_for_len!(56);
impl_to_bytes_arr_for_len!(64);
impl_to_bytes_arr_for_len!(112);
impl_to_bytes_arr_for_len!(128);
impl_to_bytes_arr_for_len!(224);
impl_to_bytes_arr_for_len!(256);
impl_to_bytes_arr_for_len!(448);
impl_to_bytes_arr_for_len!(512);
impl_to_bytes_arr_for_len!(896);
impl_to_bytes_arr_for_len!(1024);
