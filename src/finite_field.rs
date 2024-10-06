use uint_arr::{UintArrTrait, UintArr};
use num_traits::FromBytes;

pub mod ffint;
pub mod pfint;
// pub mod gfint;
pub mod uint_arr;
pub(crate) mod macro_helper;

mod test;

pub(crate) mod primuint_traits;

pub trait FiniteFieldTrait<T: UintArrTrait>: PartialEq + Clone + FromBytes {
    fn from_opaqueuint(modulo: T) -> Self;
}

pub trait PrimeFieldTrait<T: UintArrTrait>: FiniteFieldTrait<T> {}

#[derive(Clone, Copy, Eq)]
pub struct FiniteField<T: UintArrTrait> {
    modulo: T, // M modulo of the field
    len: T,    // L mod M
}

#[derive(Clone, Copy, Eq)]
pub struct PrimeField<T: UintArrTrait> {
    pub prime: T,        // P prime modulo of the field
    pub prime_addinv: T, // L - P: additive inverse of prime, P + (L - P) = 0 (mod L)
    pub len: T,          // L mod P: the reduced L
    pub modinv: T,       // P': modular inverse of prime, PP' = 1 (mod L)
                     // // PP' - RR' = 1
                     // r: T,                // R of montgomery form
                     // r_modinv: T,         // R': RR' = 1 (mod P)
                     // prime_modinv: T,     // P': of montgomery form (modular inverse of P), PP' = 1 (mod R)
}

impl<T: UintArrTrait> PartialEq for FiniteField<T> {
    fn eq(&self, other: &Self) -> bool {
        self.modulo == other.modulo 
            && self.len.rem_euclid(&self.modulo) == other.len.rem_euclid(&other.modulo) 
    }
}

impl<T: UintArrTrait> PartialEq for PrimeField<T> {
    fn eq(&self, other: &Self) -> bool {
        self.prime == other.prime 
            && self.prime_addinv == other.prime_addinv 
            && self.len.rem_euclid(&self.prime) == other.len.rem_euclid(&other.prime) 
            && self.modinv.mul(self.prime) == other.modinv.mul(other.prime)
    }
}




#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GaloisField<T: UintArrTrait> {
    modulo: T, // M modulo of the field
}

impl<T: UintArrTrait> FromBytes for FiniteField<T> {
    type Bytes = [u8];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        Self::from_opaqueuint(<T as FromBytes>::from_be_bytes(bytes))
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        Self::from_opaqueuint(T::from_le_bytes(bytes))
    }
}

impl<T: UintArrTrait> std::fmt::Debug for FiniteField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(&self, f)
    }
}

impl<T: UintArrTrait> std::fmt::Display for FiniteField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "FiniteField {{ modulo: {} }} ",
            self.modulo.to_string(),
        ))
    }
}

impl<T: UintArrTrait> FiniteFieldTrait<T> for FiniteField<T> {
    fn from_opaqueuint(modulo: T) -> Self {
        let len = modulo.neg().rem_euclid(&modulo);
        Self { modulo, len, }
    }
}

impl<T: UintArrTrait> FromBytes for PrimeField<T> {
    type Bytes = [u8];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        Self::from_opaqueuint(T::from_be_bytes(bytes))
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        Self::from_opaqueuint(T::from_le_bytes(bytes))
    }
}

impl<T: UintArrTrait> std::fmt::Debug for PrimeField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(&self, f)
    }
}

impl<T: UintArrTrait> std::fmt::Display for PrimeField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "PrimeField {{ prime: {}, prime_addinv: {}, len: {}, modinv: {} }} ",
            self.prime.to_string(),
            self.prime_addinv.to_string(),
            self.len.to_string(),
            self.modinv.to_string(),
        ))
    }
}

impl<T: UintArrTrait> FiniteFieldTrait<T> for PrimeField<T> {
    fn from_opaqueuint(prime: T) -> Self {
        let prime_addinv = prime.neg();
        // 2^N = prime_addinv (mod prime) where N = T::BITS
        let len = prime_addinv.div_rem_euclid(&prime).1;
        let modinv = {
            let (x, y, _, x_is_neg) = T::eea(prime_addinv, prime);
            let abs_modinv = x + y;
            match x_is_neg {
                false => abs_modinv.neg(),
                true => abs_modinv,
            }
        };
        Self {prime, prime_addinv, len, modinv}
    }
}

impl<T: UintArrTrait> PrimeFieldTrait<T> for PrimeField<T> {}

// impl<T: UintArrTrait> FiniteFieldTrait<T> for GaloisField<T> {}


macro_rules! impl_const_ff {
    ($word:ty, $len:expr, $size:expr) => {
        impl FiniteField<UintArr<$word, $len, $size>> {
            pub const fn const_from_opaqueuint(modulo: UintArr<$word, $len, $size>) -> Self {
                let len = modulo.const_neg().const_div_rem_euclid(modulo).1;
                Self { modulo, len, }
            }
        }
    }
}

macro_rules! impl_const_pf {
    ($word:ty, $len:expr, $size:expr) => {
        impl PrimeField<UintArr<$word, $len, $size>> {
            pub const fn const_from_opaqueuint(prime: UintArr<$word, $len, $size>) -> Self {
                type T = UintArr<$word, $len, $size>;
                let prime_addinv = prime.const_neg();
                // 2^N = prime_addinv (mod prime) where N = T::BITS
                let len = prime_addinv.const_div_rem_euclid(prime).1;
                let modinv = {
                    let (x, y, _, x_is_neg) = T::const_eea(prime_addinv, prime);
                    let abs_modinv = x.const_add(y);
                    match x_is_neg {
                        false => abs_modinv.const_neg(),
                        true => abs_modinv,
                    }
                };
                Self {prime, prime_addinv, len, modinv}
            }
        }
    };
}

crate::finite_field::macro_helper::impl_trait_for_biguint!(impl_const_pf);
crate::finite_field::macro_helper::impl_trait_for_biguint!(impl_const_ff);

