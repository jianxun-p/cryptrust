use le_int_arr::OpaqueUintTrait;
use num_bigint::BigUint;
use num_traits::{FromBytes, FromPrimitive};

pub mod ffint;
pub mod pfint;
// pub mod gfint;
pub mod le_int_arr;

pub(crate) mod primuint_traits;

pub trait FiniteFieldTrait<T: OpaqueUintTrait>: PartialEq + Clone + FromBytes {}

pub trait PrimeFieldTrait<T: OpaqueUintTrait>: FiniteFieldTrait<T> {}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FiniteField<T: OpaqueUintTrait> {
    modulo: T, // M modulo of the field
    len: T,    // L mod M
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PrimeField<T: OpaqueUintTrait> {
    prime: T,        // P prime modulo of the field
    prime_addinv: T, // -P: additive inverse of prime, P + -P = 0 (mod L)
    len: T,          // L mod P
    modinv: T,       // P': modular inverse of prime, PP' = 1 (mod L)
                     // // PP' - RR' = 1
                     // r: T,                // R of montgomery form
                     // r_modinv: T,         // R': RR' = 1 (mod P)
                     // prime_modinv: T,     // P': of montgomery form (modular inverse of P), PP' = 1 (mod R)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GaloisField<T: OpaqueUintTrait> {
    modulo: T, // M modulo of the field
}

impl<T: OpaqueUintTrait> FromBytes for FiniteField<T> {
    type Bytes = [u8];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let n_bigint: BigUint = BigUint::from_u8(2).unwrap().pow(T::bits() as u32);
        let modulo = T::from_be_bytes(bytes);
        let m_bigint: BigUint = modulo.to_biguint();
        let len_bigint = &n_bigint % &m_bigint;
        Self {
            modulo: modulo,
            len: T::from_biguint(len_bigint),
        }
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let n_bigint: BigUint = BigUint::from_u8(2).unwrap().pow(T::bits() as u32);
        let modulo = T::from_le_bytes(bytes);
        let m_bigint: BigUint = modulo.to_biguint();
        let len_bigint = &n_bigint % &m_bigint;
        Self {
            modulo: modulo,
            len: T::from_biguint(len_bigint),
        }
    }
}

impl<T: OpaqueUintTrait> std::fmt::Debug for FiniteField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(&self, f)
    }
}

impl<T: OpaqueUintTrait> std::fmt::Display for FiniteField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "FiniteField {{ modulo: {} }} ",
            self.modulo.to_string(),
        ))
    }
}

impl<T: OpaqueUintTrait> FiniteFieldTrait<T> for FiniteField<T> {}

#[cfg(feature = "check_prime")]
fn is_prime<T: OpaqueUintTrait>(num: T) -> bool {
    let two = T::ONE.overflowing_double().0;
    if (num % two).is_zero() {
        return false;
    }
    let four = two.overflowing_double().0;
    let mut i = two + T::ONE; // starting from 3
    let mut i_sqr = i * i;
    while i_sqr <= num {
        if (num % i).is_zero() {
            return false;
        }
        let four_i = i.overflowing_double().0.overflowing_double().0;
        i_sqr = i_sqr + four_i + four;
        i = i + two;
    }
    true
}

impl<T: OpaqueUintTrait> FromBytes for PrimeField<T> {
    type Bytes = [u8];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let n_bigint: BigUint = BigUint::from_u8(2).unwrap().pow(T::bits() as u32);
        let prime = T::from_be_bytes(bytes);
        #[cfg(feature = "check_prime")]
        debug_assert!(is_prime(prime), "the argument given is a not a prime");
        let prime_bigint: BigUint = prime.to_biguint();
        let len_bigint = &n_bigint % &prime_bigint;
        let modinv = T::from_biguint(prime_bigint.modinv(&n_bigint).unwrap());
        Self {
            prime: prime,
            prime_addinv: prime.neg(),
            len: T::from_biguint(len_bigint),
            modinv: modinv,
        }
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let n_bigint: BigUint = BigUint::from_u8(2).unwrap().pow(T::bits() as u32);
        let prime = T::from_le_bytes(bytes);
        #[cfg(feature = "check_prime")]
        debug_assert!(is_prime(prime), "the argument given is a not a prime");
        let prime_bigint: BigUint = prime.to_biguint();
        let len_bigint = &n_bigint % &prime_bigint;
        let modinv = T::from_biguint(prime_bigint.modinv(&n_bigint).unwrap());
        Self {
            prime: prime,
            prime_addinv: prime.neg(),
            len: T::from_biguint(len_bigint),
            modinv: modinv,
        }
    }
}

impl<T: OpaqueUintTrait> std::fmt::Debug for PrimeField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as std::fmt::Display>::fmt(&self, f)
    }
}

impl<T: OpaqueUintTrait> std::fmt::Display for PrimeField<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "PrimeField {{ prime: {}, prime_addinv: {}, modinv: {} }} ",
            self.prime.to_string(),
            self.prime_addinv.to_string(),
            // self.len.to_string(),
            self.modinv.to_string(),
        ))
    }
}

impl<T: OpaqueUintTrait> FiniteFieldTrait<T> for PrimeField<T> {}

impl<T: OpaqueUintTrait> PrimeFieldTrait<T> for PrimeField<T> {}

// impl<T: OpaqueUintTrait> FiniteFieldTrait<T> for GaloisField<T> {}
