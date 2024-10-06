use super::*;

pub trait KeyExchangeTrait<const SIZE: usize, Private: PrivateKey<SIZE>, Public: PublicKey<SIZE>, Shared: SharedSecret<SIZE>> {
    fn public_key(private_key: &Private) -> Public;
    fn shared_secret(private_key: Private, public_key: Public) -> Shared;
}

#[cfg(feature = "ecc")]
pub mod x25519;

#[cfg(feature = "ecc")]
pub mod x448;

#[cfg(feature = "dh")]
pub mod dh;

#[cfg(feature = "ecc")]
pub use self::{x25519::*, x448::*};
