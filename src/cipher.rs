use crate::key::Key;

pub trait Cipher<const KEY_SIZE: usize, K: Key<KEY_SIZE>> {
    type Input: AsRef<[u8]> + ?Sized;
    type Output: AsRef<[u8]>;
    fn encrypt(plaintext: &Self::Input, key: &K) -> Self::Output;
    fn decrypt(ciphertext: &Self::Input, key: &K) -> Self::Output;
}

#[cfg(feature = "block_cipher")]
pub trait BlockCipher<const KEY_SIZE: usize, K: Key<KEY_SIZE>, const BLK_SIZE: usize>:
    Cipher<KEY_SIZE, K, Input = [u8; BLK_SIZE], Output = [u8; BLK_SIZE]>
{
}

#[cfg(feature = "aead")]
pub trait AEAD {
    type Input: AsRef<[u8]> + ?Sized;
    type AAD: AsRef<[u8]> + ?Sized;
    type Output: AsRef<[u8]>;
    type Authentication: AsRef<[u8]> + Eq;
    fn encrypt(&self, plaintext: &Self::Input, iv: &Self::Input) -> Self::Output;
    fn decrypt(&self, ciphertext: &Self::Input, iv: &Self::Input) -> Self::Output;
    fn authentication_tag(
        &self,
        ciphertext: &Self::Input,
        iv: &Self::Input,
        aad: &Self::AAD,
    ) -> Self::Output;
}

#[cfg(all(feature = "aes", feature = "block_cipher"))]
mod aes;

#[cfg(feature = "aes")]
pub use aes::{AES128Key, AES192Key, AES256Key, AES128, AES192, AES256};

#[cfg(feature = "aead")]
mod gcm;

#[cfg(feature = "aead")]
pub use gcm::*;
