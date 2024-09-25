use num_traits::{FromBytes, ToBytes};

pub trait Key: Clone + FromBytes + ToBytes + Sized {
    const SIZE: usize;
    fn from_slice(data: &[u8]) -> Self;
}

pub trait PrivateKey: Key {
    fn rand() -> Self;
}

pub trait PublicKey: Key {
    
}

pub trait SharedSecret: Key {}
