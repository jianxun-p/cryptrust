use crate::key::*;

pub trait KeyExchangeTrait<Private: PrivateKey, Public: PublicKey, Shared: SharedSecret> {
    fn public_key(private_key: &Private) -> Public;
    fn key_pair() -> (Private, Public) {
        let private_key = Private::rand();
        let public_key = Self::public_key(&private_key);
        (private_key, public_key)
    }
    fn shared_secret(private_key: Private, public_key: Public) -> Shared;
}

#[cfg(feature = "ecc")]
pub mod x25519;

#[cfg(feature = "ecc")]
pub mod x448;

#[cfg(feature = "ecc")]
pub use self::{x25519::*, x448::*};
