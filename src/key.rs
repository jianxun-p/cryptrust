use num_traits::{FromBytes, ToBytes};

#[cfg(feature = "key_exchange")]
pub mod key_exchange;

pub trait Key<const S: usize>: Clone + FromBytes + ToBytes + Sized {
    const SIZE: usize = S;
    fn from_slice(data: &[u8]) -> Self;
}

pub trait PrivateKey<const S: usize>: Key<S> {
    fn from_rng(rng: impl Fn() -> u8) -> Self {
        let random_val = [0u8; S].map(|_| rng());
        Self::from_slice(random_val.as_slice())
    }
}

pub trait PublicKey<const S: usize>: Key<S> {}

pub trait SharedSecret<const S: usize>: Key<S> {}
