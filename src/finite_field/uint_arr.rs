use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingSub}, Bounded, ConstOne, ConstZero, Euclid, FromBytes, One, Pow, ToBytes, WrappingAdd, WrappingSub, Zero
};
use core::{
    cmp::Ordering,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Rem, Sub},
};

use super::primuint_traits::PrimUint;

use arrayvec::ArrayVec;

pub type WORD = u64;

pub const fn arr_len<WORD: PrimUint>(uint_arr_size: usize) -> usize {
    uint_arr_size / WORD::SIZE
}


pub trait UintArrTrait :
PartialEq
+ Eq
+ Copy
+ Clone
+ Bounded
+ Zero
+ ConstZero
+ ConstOne
+ One
+ WrappingAdd<Output = Self>
+ WrappingSub<Output = Self>
// + WrappingMul<Output = Self>
+ OverflowingAdd
+ OverflowingSub
+ WrappingAdd
+ WrappingSub
+ Pow<usize, Output = Self>
+ ToBytes
+ FromBytes<Bytes = [u8]>
+ Default
+ Euclid
+ Ord
+ BitAnd<Output = Self>
+ BitOr<Output = Self>
+ BitXor<Output = Self>
+ PartialOrd
+ ToString
+ std::fmt::Debug
{
    type WORD: PrimUint;
    const LEN: usize;
    const SIZE: usize;
    const BITS: usize;
    const TWO: Self;

    #[cfg(feature = "bigint")]
    fn from_biguint(biguint: BigUint) -> Self {
        let v = biguint.to_bytes_le();
        <Self as PrimUint>::from_le_bytes(v.as_slice())
    }

    #[cfg(feature = "bigint")]
    fn to_biguint(&self) -> BigUint {
        let bytes = self.to_le_bytes();
        BigUint::from_bytes_le(bytes.as_slice())
    }

    fn neg(self) -> Self;

    fn from_word(val: Self::WORD) -> Self;

    fn bit(&self, n: usize) -> bool;

    fn set_bit(&mut self, n: usize, val: bool);

    fn bit_to_mask(val: bool) -> Self;

    /// returns (least significant words, most significant words) of the full product
    fn widening_mul(&self, b: &Self) -> (Self, Self);

    fn repeat(val: Self::WORD) -> Self;

    fn eea(a: Self, b: Self) -> (Self, Self, Self, bool) {
        let mut s_is_negative = false;
        let mut q;
        // row0: (s0, t0, r0), row1: (s0, t0, r0)
        let mut row0 = (Self::ONE, Self::ZERO, a);
        let mut row1 = (Self::ZERO, Self::ONE, b);
        while !row1.2.is_zero() {
            (q, row0.2) = row0.2.div_rem_euclid(&row1.2);
            (row0.0, row0.1) = {
                let q_mul_s1 = q.mul(row1.0);
                let q_mul_t1 = q.mul(row1.1);
                (q_mul_s1 + row0.0, row0.1 + q_mul_t1)
            };
            (row0, row1) = (row1, row0);
            s_is_negative = !s_is_negative;
        }
        (row0.0, row0.1, row0.2, s_is_negative)
    }

}



// pub trait FromBytes where Self: Sized {
//     type Bytes: AsRef<[u8]> + ?Sized;
//     const BYWORDE_ARR_SIZE: usize;
//     fn from_le_bytes<const S: usize>(bytes: &Self::Bytes) -> Self;

//     fn from_be_bytes<const S: usize>(bytes: &Self::Bytes) -> Self;

//     fn from_ne_bytes<const S: usize>(bytes: &Self::Bytes) -> Self {
//         if cfg!(target_endian = "little") {
//             Self::from_le_bytes::<S>(bytes)
//         } else {
//             Self::from_be_bytes::<S>(bytes)
//         }
//     }
// }




pub trait ToByteArr<const S: usize> {
    fn to_le_byte_arr(&self) -> [u8; S];
    fn to_be_byte_arr(&self) -> [u8; S];
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct UintArr<WORD: PrimUint, const LEN: usize, const SIZE: usize> {
    pub(crate) le_words: [WORD; LEN], // words in little endian
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> std::fmt::Debug for UintArr<WORD, LEN, SIZE> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("UintArr {:?}", self.le_words))
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Pow<usize> for UintArr<WORD, LEN, SIZE> {
    type Output = Self;
    fn pow(self, rhs: usize) -> Self::Output {
        match rhs {
            0 => Self::ONE,
            1 => self,
            _ => {
                let sqr = self * self;
                match rhs.bit(0) {
                    false => sqr.pow(rhs / 2),
                    true => self * sqr.pow(rhs / 2),
                }
            }
        }
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Zero for UintArr<WORD, LEN, SIZE> {
    fn zero() -> Self {
        Self {
            le_words: [WORD::zero(); LEN],
        }
    }

    fn is_zero(&self) -> bool {
        self.le_words
            .iter()
            .fold(true, |acc, i| acc && WORD::zero().eq(i))
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> ConstZero for UintArr<WORD, LEN, SIZE> {
    const ZERO: Self = Self {
        le_words: [WORD::ZERO; LEN],
    };
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> One for UintArr<WORD, LEN, SIZE> {
    fn one() -> Self {
        let mut a = Self::ZERO;
        a.le_words[0] = WORD::ONE;
        a
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> ConstOne for UintArr<WORD, LEN, SIZE> {
    const ONE: Self = {
        let mut a = Self::ZERO;
        a.le_words[0] = WORD::ONE;
        a
    };
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Default for UintArr<WORD, LEN, SIZE> {
    fn default() -> Self {
        Self::ZERO
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> PartialOrd for UintArr<WORD, LEN, SIZE> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Ord for UintArr<WORD, LEN, SIZE> {
    fn cmp(&self, other: &Self) -> Ordering {
        (0..LEN).rev().fold(Ordering::Equal, |acc, i| match acc {
            Ordering::Equal => self.le_words[i].cmp(&other.le_words[i]),
            _ => acc,
        })
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Add for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn add(mut self, b: Self) -> Self {
        self.overflowing_addsub(&b, &WORD::overflowing_add);
        self
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> OverflowingAdd for UintArr<WORD, LEN, SIZE> {
    fn overflowing_add(&self, v: &Self) -> (Self, bool) {
        let mut ans = self.clone();
        let overflowed = ans.overflowing_addsub(&v, &WORD::overflowing_add);
        (ans, overflowed)
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> OverflowingSub for UintArr<WORD, LEN, SIZE> {
    fn overflowing_sub(&self, v: &Self) -> (Self, bool) {
        let mut ans = self.clone();
        let overflowed = ans.overflowing_addsub(&v, &WORD::overflowing_sub);
        (ans, overflowed)
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> UintArr<WORD, LEN, SIZE> {
    pub fn overflowing_addsub<O: Fn(&WORD, &WORD) -> (WORD, bool)>(&mut self, rhs: &Self, op: &O) -> bool {
        let mut carry = false;
        for i in 0..LEN {
            let carry_t = WORD::from_bool(carry);
            let mut tmp = op(&self.le_words[i], &carry_t);
            carry = tmp.1;
            tmp = op(&tmp.0, &rhs.le_words[i]);
            (self.le_words[i], carry) = (tmp.0, tmp.1 | carry);
        }
        carry
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Bounded for UintArr<WORD, LEN, SIZE> {
    fn min_value() -> Self {
        Self::ZERO
    }
    fn max_value() -> Self {
        Self {
            le_words: [WORD::max_value(); LEN],
        }
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> FromBytes for UintArr<WORD, LEN, SIZE> {

    type Bytes = [u8];

    fn from_le_bytes(bytes: &[u8]) -> Self {
        let mut le_words: [WORD; LEN] = [WORD::zero(); LEN];
        let mut byte_arr = [0u8; SIZE];
        let copy_size = Self::SIZE.min(bytes.len());
        let copy_len = copy_size.div_ceil(WORD::SIZE);
        byte_arr[0..copy_size].copy_from_slice(&bytes[0..copy_size]);
        for i in 0..copy_len {
            let (s, e) = (i * WORD::SIZE, (i + 1) * WORD::SIZE);
            le_words[i] = WORD::from_le_bytes(&byte_arr[s..e]);
        }
        Self { le_words: le_words }
    }

    fn from_be_bytes(bytes: &[u8]) -> Self {
        let mut le_words: [WORD; LEN] = [WORD::zero(); LEN];
        let mut byte_arr = [0u8; SIZE];
        let copy_size = Self::SIZE.min(bytes.len());
        let copy_len = copy_size.div_ceil(WORD::SIZE);
        byte_arr[0..copy_size].copy_from_slice(&bytes[0..copy_size]);
        for i in 0..copy_len {
            let (s, e) = (i * WORD::SIZE, (i + 1) * WORD::SIZE);
            le_words[LEN - i - 1] = WORD::from_be_bytes(&byte_arr[s..e]);
        }
        Self { le_words: le_words }
    }
    
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> ToBytes for UintArr<WORD, LEN, SIZE> {
    type Bytes = ArrayVec<u8, SIZE>;

    fn to_le_bytes(&self) -> Self::Bytes {
        let mut bytes = ArrayVec::new();
        for i in 0..LEN {
            let tmp = self.le_words[i].to_le_bytes();
            bytes.try_extend_from_slice(tmp.as_ref()).unwrap();
        }
        bytes
    }

    fn to_be_bytes(&self) -> Self::Bytes {
        let mut bytes = ArrayVec::new();
        for i in 0..LEN {
            let tmp = self.le_words[LEN - i - 1].to_be_bytes();
            bytes.try_extend_from_slice(tmp.as_ref()).unwrap();
        }
        bytes
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> WrappingAdd for UintArr<WORD, LEN, SIZE> {
    fn wrapping_add(&self, b: &Self) -> Self {
        let mut a = self.clone();
        a.overflowing_addsub(b, &WORD::overflowing_add);
        a
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Sub for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn sub(mut self, b: Self) -> Self {
        self.overflowing_addsub(&b, &WORD::overflowing_sub);
        self
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> WrappingSub for UintArr<WORD, LEN, SIZE> {
    fn wrapping_sub(&self, b: &Self) -> Self {
        let mut a = self.clone();
        a.overflowing_addsub(b, &WORD::overflowing_sub);
        a
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Div for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        self.div_euclid(&rhs)
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Rem for UintArr<WORD, LEN, SIZE> {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        self.rem_euclid(&rhs)
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Euclid for UintArr<WORD, LEN, SIZE> {
    fn div_euclid(&self, rhs: &Self) -> Self {
        self.div_rem_euclid(rhs).0
    }
    fn rem_euclid(&self, rhs: &Self) -> Self {
        self.div_rem_euclid(rhs).1
    }
    fn div_rem_euclid(&self, rhs: &Self) -> (Self, Self) {
        let (mut q, mut r) = (Self::ZERO, Self::ZERO);
        for i in (0..Self::BITS).rev() {
            let overflowed;
            (r, overflowed) = r.overflowing_add(&r);
            r.set_bit(0, self.bit(i as usize));
            let mask = WORD::bit_to_mask(r.ge(&rhs) | overflowed);
            let masked_sub = move |a: &WORD, b: &WORD| {
                let masked_b = *b & mask;
                a.overflowing_sub(&masked_b)
            };
            r.overflowing_addsub(&rhs, &masked_sub);
            q.set_bit(i as usize, mask != WORD::zero());
        }
        (q, r)
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> Mul for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut ans = Self::ZERO;
        for i in 0..LEN {
            for j in 0..(LEN - i) {
                let (val, mut carry) = PrimUint::widening_mul(&self.le_words[i], &rhs.le_words[j]);
                let mut overflowed;
                (ans.le_words[i + j], overflowed) = ans.le_words[i + j].overflowing_add(&val);
                let mut k = 1;
                while i + j + k < LEN {
                    let overflowed2: bool;
                    (carry, overflowed) = carry.overflowing_add(&WORD::from_bool(overflowed));
                    (ans.le_words[i + j + k], overflowed2) =
                        ans.le_words[i + j + k].overflowing_add(&carry);
                    carry = WORD::from_bool(overflowed) + WORD::from_bool(overflowed2);
                    overflowed = false;
                    k += 1;
                }
            }
        }
        ans
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> ToString for UintArr<WORD, LEN, SIZE> {
    fn to_string(&self) -> String {
        let mut s = String::new();
        let mut a = self.clone();
        let ten_bytes = [10u8];
        let ten: UintArr<WORD, LEN, SIZE> = <Self as FromBytes>::from_le_bytes(&ten_bytes);
        while !a.is_zero() {
            let c = (a % ten).le_words[0].to_string();
            a = a / ten;
            s.extend(c.chars())
        }
        match s.is_empty() {
            true => String::from("0"),
            _ => String::from_iter(s.chars().rev()),
        }
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> BitAnd for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn bitand(mut self, rhs: Self) -> Self::Output {
        for i in 0..LEN {
            self.le_words[i] = self.le_words[i].bitand(rhs.le_words[i]);
        }
        self
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> BitOr for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn bitor(mut self, rhs: Self) -> Self::Output {
        for i in 0..LEN {
            self.le_words[i] = self.le_words[i].bitor(rhs.le_words[i]);
        }
        self
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> BitXor for UintArr<WORD, LEN, SIZE> {
    type Output = Self;

    fn bitxor(mut self, rhs: Self) -> Self::Output {
        for i in 0..LEN {
            self.le_words[i] = self.le_words[i].bitxor(rhs.le_words[i]);
        }
        self
    }
}

impl<WORD: PrimUint, const LEN: usize, const SIZE: usize> UintArrTrait for UintArr<WORD, LEN, SIZE> {
    type WORD = WORD;
    const LEN: usize = LEN;
    const SIZE: usize = LEN * WORD::SIZE;
    const BITS: usize = LEN * WORD::BITS;
    const TWO: Self = {
        let mut le_words = [WORD::ZERO; LEN];
        le_words[0] = WORD::TWO;
        Self { le_words, }
    };

    #[cfg(feature = "bigint")]
    fn from_biguint(biguint: BigUint) -> Self {
        let v = biguint.to_bytes_le();
        <Self as PrimUint>::from_le_bytes(v.as_slice())
    }

    #[cfg(feature = "bigint")]
    fn to_biguint(&self) -> BigUint {
        let bytes = self.to_le_bytes();
        BigUint::from_bytes_le(bytes.as_slice())
    }

    #[inline]
    fn neg(mut self) -> Self {
        self.le_words = self.le_words.map(|b| !b);
        self + Self::ONE
    }

    fn from_word(val: Self::WORD) -> Self {
        let mut a = Self::zero();
        a.le_words[0] = val;
        a
    }

    fn bit(&self, n: usize) -> bool {
        let i: usize = n / (WORD::BITS as usize);
        let j: usize = n % (WORD::BITS as usize);
        match i < LEN {
            false => false,
            true => self.le_words[i].bit(j),
        }
    }

    fn set_bit(&mut self, n: usize, val: bool) {
        let (i, j) = n.div_rem_euclid(&WORD::BITS);
        if i < LEN {
            self.le_words[i].set_bit(j, val);
        }
    }

    /// returns (least significant words, most significant words) of the full product
    fn widening_mul(&self, b: &Self) -> (Self, Self) {
        let mut ans = [Self::ZERO; 2];
        for i in 0..LEN {
            for j in 0..LEN {
                #[allow(unstable_name_collisions)]
                let (val, mut carry) = self.le_words[i].widening_mul(&b.le_words[j]);
                let mut overflowed;
                (ans[(i + j) / LEN].le_words[(i + j) % LEN], overflowed) =
                    ans[(i + j) / LEN].le_words[(i + j) % LEN].overflowing_add(&val);
                let mut k = 1;
                while i + j + k < 2 * LEN {
                    let overflowed2: bool;
                    (carry, overflowed) = carry.overflowing_add(&WORD::from_bool(overflowed));
                    (ans[(i + j + k) / LEN].le_words[(i + j + k) % LEN], overflowed2) =
                        ans[(i + j + k) / LEN].le_words[(i + j + k) % LEN].overflowing_add(&carry);
                    carry = WORD::from_bool(overflowed) + WORD::from_bool(overflowed2);
                    overflowed = false;
                    k += 1;
                }
            }
        }
        // (least significant words, most significant words)
        (ans[0], ans[1])
    }

    #[inline]
    fn bit_to_mask(val: bool) -> Self {
        let word = <Self::WORD as PrimUint>::bit_to_mask(val);
        Self::repeat(word)
    }

    #[inline]
    fn repeat(val: Self::WORD) -> Self {
        Self { le_words: [val; LEN] }
    }
}

macro_rules! impl_const_uint_arr {
    ($word:ty, $len:expr, $size:expr) => {

        impl UintArr<$word, $len, $size> {
            pub const fn const_from_be_bytes(bytes: [u8; $size]) -> Self {
                const WORD_SIZE: usize = (<$word>::BITS / u8::BITS) as usize;
                let mut le_words = [0; $len];
                let mut i = 0;
                while i < $len {
                    let mut tmp = [0u8; WORD_SIZE];
                    let mut j = 0;
                    while j < WORD_SIZE {
                        tmp[j] = bytes[i * WORD_SIZE + j];
                        j += 1;
                    }
                    le_words[$len - i - 1] = <$word>::from_be_bytes(tmp);
                    i += 1;
                }
                Self { le_words }
            }
            pub const fn const_bit(self, n: usize) -> bool {
                const BITS: usize = <$word>::BITS as usize;
                let bit_of_word = n % BITS;
                let word = self.le_words[n / BITS] >> bit_of_word;
                (word & 1) != 0
            }
            pub const fn const_set_bit(mut self, n: usize, val: bool) -> Self {
                const BITS: usize = <$word>::BITS as usize;
                let word = self.le_words[n / BITS];
                let bit_of_word = n % BITS;
                self.le_words[n / BITS] = [word & !(1 << bit_of_word), word | (1 << bit_of_word)][val as usize];
                self
            }
            pub const fn const_overflowing_add(mut self, rhs: Self) -> (Self, bool) {
                let mut carry = false;
                let mut i = 0;
                while i < $len {
                    let mut tmp = self.le_words[i].overflowing_add(carry as $word);
                    carry = tmp.1;
                    tmp = tmp.0.overflowing_add(rhs.le_words[i]);
                    (self.le_words[i], carry) = (tmp.0, tmp.1 | carry);
                    i += 1;
                }
                (self, carry)
            }
            pub const fn const_add(self, rhs: Self) -> Self {
                self.const_overflowing_add(rhs).0
            }
            pub const fn const_overflowing_sub(mut self, rhs: Self) -> (Self, bool) {
                let mut borrow = false;
                let mut i = 0;
                while i < $len {
                    let mut tmp = self.le_words[i].overflowing_sub(borrow as $word);
                    borrow = tmp.1;
                    tmp = tmp.0.overflowing_sub(rhs.le_words[i]);
                    (self.le_words[i], borrow) = (tmp.0, tmp.1 | borrow);
                    i += 1;
                }
                (self, borrow)
            }
            pub const fn const_sub(self, rhs: Self) -> Self {
                self.const_overflowing_sub(rhs).0
            }
            pub const fn const_neg(mut self) -> Self {
                let mut i = 0;
                while i < $len {
                    self.le_words[i] = <$word>::MAX - self.le_words[i];
                    i += 1;
                }
                self.const_add(Self::ONE)
            }
            pub const fn const_mul(self, rhs: Self) -> Self {
                use super::primuint_traits::primuint_widening_mul;
                let mut ans = Self::ZERO;
                let mut i = 0;
                while i < $len {
                    let mut j = 0;
                    while j < $len - i {
                        #[allow(unstable_name_collisions)]
                        let (val, mut carry) = primuint_widening_mul!($word, self.le_words[i], rhs.le_words[j]);
                        let mut overflowed;
                        (ans.le_words[i + j], overflowed) = ans.le_words[i + j].overflowing_add(val);
                        let mut k = 1;
                        while i + j + k < $len {
                            let overflowed2: bool;
                            (carry, overflowed) = carry.overflowing_add(overflowed as $word);
                            (ans.le_words[i + j + k], overflowed2) =
                                ans.le_words[i + j + k].overflowing_add(carry);
                            carry = (overflowed as $word) + (overflowed2 as $word);
                            overflowed = false;
                            k += 1;
                        }
                        j += 1;
                    }
                    i += 1;
                }
                ans
            }
            pub const fn const_pow(self, rhs: usize) -> Self {
                match rhs {
                    0 => Self::ONE,
                    1 => self,
                    _ => {
                        let sqr = self.const_mul(self);
                        match rhs & 1 != 0 {
                            false => sqr.const_pow(rhs / 2),
                            true => self.const_mul(sqr.const_pow(rhs / 2)),
                        }
                    }
                }
            }
            pub const fn const_ge(self, rhs: Self) -> bool {
                let mut j = 0;
                while j < $len {
                    let i = $len - j - 1;
                    if self.le_words[i] > rhs.le_words[i] {
                        return true;
                    } else if self.le_words[i] < rhs.le_words[i] {
                        return false;
                    }
                    j += 1;
                }
                true
            }
            pub const fn const_div_rem_euclid(self, rhs: Self) -> (Self, Self) {
                const fn bit_to_mask(b: bool) -> $word {
                    ((((b as u64) << (u64::BITS - 1)) as i64) >> u64::BITS - 1) as u64
                }
                let (mut q, mut r) = (Self::ZERO, Self::ZERO);
                let mut j = 0;
                while j < Self::BITS {
                    let i = Self::BITS - j - 1;
                    let overflowed;
                    (r, overflowed) = r.const_overflowing_add(r);
                    r = r.const_set_bit(0, self.const_bit(i as usize));
                    let mask = bit_to_mask(r.const_ge(rhs) | overflowed);
                    let mut v = rhs;
                    let mut k = 0;
                    while k < $len {
                        v.le_words[k] &= mask;
                        k += 1;
                    }
                    r = r.const_sub(v);
                    q = q.const_set_bit(i as usize, mask != 0);
                    j += 1;
                }
                (q, r)
            }
            pub const fn const_is_zero(&self) -> bool {
                let mut i = 0;
                while i < $len {
                    if self.le_words[i] != 0 {
                        return false;
                    }
                    i += 1;
                }
                true
            }
            /// a must be greater or equals to b
            /// returns (s, t, d, s < 0) where as + bt = d = gcd(a, b)
            pub const fn const_eea(a: Self, b: Self) -> (Self, Self, Self, bool) {
                let mut s_is_negative = false;
                let mut q;
                // row0: (s0, t0, r0), row1: (s0, t0, r0)
                let mut row0 = (Self::ONE, Self::ZERO, a);
                let mut row1 = (Self::ZERO, Self::ONE, b);
                while !row1.2.const_is_zero() {
                    (q, row0.2) = row0.2.const_div_rem_euclid(row1.2);
                    (row0.0, row0.1) = {
                        let q_mul_s1 = q.const_mul(row1.0);
                        let q_mul_t1 = q.const_mul(row1.1);
                        (q_mul_s1.const_add(row0.0), row0.1.const_add(q_mul_t1))
                    };
                    (row0, row1) = (row1, row0);
                    s_is_negative = !s_is_negative;
                }
                (row0.0, row0.1, row0.2, s_is_negative)
            }

            pub const fn to_le_byte_arr(&self) -> [u8; $len * (<$word>::BITS / u8::BITS) as usize] {
                let mut bytes = [0u8; $len * (<$word>::BITS/ u8::BITS) as usize];
                let mut i = 0;
                while i < $len {
                    let tmp = self.le_words[i].to_le_bytes();
                    let word_size = tmp.len();
                    let mut j = 0;
                    while j < word_size {
                        bytes[i * word_size + j] = tmp[j];
                        j += 1;
                    }
                    i += 1;
                }
                bytes
            }
            pub const fn to_be_byte_arr(&self) -> [u8; $len * (<$word>::BITS / u8::BITS) as usize] {
                let mut bytes = [0u8; $len * (<$word>::BITS / u8::BITS) as usize];
                let mut i = 0;
                while i < $len {
                    let tmp = self.le_words[$len - i - 1].to_be_bytes();
                    let word_size = tmp.len();
                    let mut j = 0;
                    while j < word_size {
                        bytes[i * word_size + j] = tmp[j];
                        j += 1;
                    }
                    i += 1;
                }
                bytes
            }

        }
    };
}



crate::finite_field::macro_helper::impl_trait_for_biguint!(impl_const_uint_arr);
