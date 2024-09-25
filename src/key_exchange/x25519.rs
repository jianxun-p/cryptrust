use num_traits::{FromBytes, ToBytes};
use rand::random;

use crate::{
    ecc::{common_curves::curve25519, FFEllipticCurveTrait, PFInt}, finite_field::{
        ffint::{FFInt, FiniteFieldIntTrait},
        le_int_arr::ToByteArr,
    }, key::{Key, PrivateKey, PublicKey, SharedSecret}, key_exchange::KeyExchangeTrait
};

pub struct X25519();

#[derive(Debug, Clone, Copy)]
pub struct X25519PrivKey(FFInt<'static, curve25519::OpaqueInt>);

#[derive(Debug, Clone, Copy)]
pub struct X25519PubKey(PFInt<'static, curve25519::OpaqueInt>);

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct X25519SharedKey(PFInt<'static, curve25519::OpaqueInt>);

impl FromBytes for X25519PrivKey {
    type Bytes = [u8; curve25519::SIZE];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let mut encoded = bytes.clone();
        encoded[curve25519::SIZE - 1] &= 0xf8;
        encoded[0] &= 0x7f;
        encoded[0] |= 0x40;
        Self(FFInt::from_be_bytes(
            &encoded,
            curve25519::curve25519_finite_field(),
        ))
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let mut encoded = bytes.clone();
        encoded[0] &= 0xf8;
        encoded[curve25519::SIZE - 1] &= 0x7f;
        encoded[curve25519::SIZE - 1] |= 0x40;

        Self(FFInt::from_le_bytes(
            &encoded,
            curve25519::curve25519_finite_field(),
        ))
    }
}

impl ToBytes for X25519PrivKey {
    type Bytes = [u8; curve25519::SIZE];

    fn to_be_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_be_byte_arr()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_le_byte_arr()
    }
}

impl FromBytes for X25519PubKey {
    type Bytes = [u8; curve25519::SIZE];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let mut encoded = bytes.clone();
        encoded[0] &= 0b0111_1111;
        Self(PFInt::from_be_bytes(
            &encoded,
            curve25519::curve25519_prime_field(),
        ))
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let mut encoded = bytes.clone();
        encoded[curve25519::SIZE - 1] &= 0b0111_1111;
        Self(PFInt::from_le_bytes(
            &encoded,
            curve25519::curve25519_prime_field(),
        ))
    }
}

impl ToBytes for X25519PubKey {
    type Bytes = [u8; curve25519::SIZE];

    fn to_be_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_be_byte_arr()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_le_byte_arr()
    }
}

impl FromBytes for X25519SharedKey {
    type Bytes = [u8; curve25519::SIZE];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        Self(PFInt::from_be_bytes(
            bytes,
            curve25519::curve25519_prime_field(),
        ))
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        Self(PFInt::from_le_bytes(
            bytes,
            curve25519::curve25519_prime_field(),
        ))
    }
}

impl ToBytes for X25519SharedKey {
    type Bytes = [u8; curve25519::SIZE];

    fn to_be_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_be_byte_arr()
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        self.0.to_opaqueuint().to_le_byte_arr()
    }
}

impl Key for X25519PrivKey {
    const SIZE: usize = curve25519::SIZE;
    fn from_slice(data: &[u8]) -> Self {
        let mut key_bytes = [0u8; curve25519::SIZE];
        let mid = data.len().min(curve25519::SIZE);
        for i in 0..mid {
            key_bytes[i] = data[i];
        }
        for i in mid..curve25519::SIZE {
            key_bytes[i] = random();
        }
        Self::from_ne_bytes(&key_bytes)
    }
}

impl Key for X25519PubKey {
    const SIZE: usize = curve25519::SIZE;
    fn from_slice(data: &[u8]) -> Self {
        let mut key_bytes = [0u8; curve25519::SIZE];
        let mid = data.len().min(curve25519::SIZE);
        for i in 0..mid {
            key_bytes[i] = data[i];
        }
        for i in mid..curve25519::SIZE {
            key_bytes[i] = random();
        }
        Self::from_ne_bytes(&key_bytes)
    }
}

impl Key for X25519SharedKey {
    const SIZE: usize = curve25519::SIZE;
    fn from_slice(data: &[u8]) -> Self {
        let mut key_bytes = [0u8; curve25519::SIZE];
        let mid = data.len().min(curve25519::SIZE);
        for i in 0..mid {
            key_bytes[i] = data[i];
        }
        for i in mid..curve25519::SIZE {
            key_bytes[i] = random();
        }
        Self::from_ne_bytes(&key_bytes)
    }
}

impl PrivateKey for X25519PrivKey {
    fn rand() -> Self {
        let key_bytes: [u8; curve25519::SIZE] = random();
        Self::from_ne_bytes(&key_bytes)
    }
}

impl PublicKey for X25519PubKey {}

impl SharedSecret for X25519SharedKey {}

impl KeyExchangeTrait<X25519PrivKey, X25519PubKey, X25519SharedKey> for X25519 {

    fn shared_secret(private_key: X25519PrivKey, public_key: X25519PubKey) -> X25519SharedKey {
        use curve25519::*;
        let p = curve25519().new_point(public_key.0);
        let shared = curve25519().point_mul(&p, private_key.0);
        X25519SharedKey(shared.point)
    }
    
    fn public_key(private_key: &X25519PrivKey) -> X25519PubKey {
        use curve25519::*;
        let public_key = curve25519().point_mul(curve25519_generator(), private_key.0);
        X25519PubKey(public_key.point)
    }
}

#[cfg(test)]
mod test {

    #[test]
    fn x25519_test1() {
        /// https://x25519.xargs.org/
        use super::*;
        let k: [u8; 32] = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x11,
        ];
        let g: [u8; 32] = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x09,
        ];
        let r: [u8; 32] = [
            0x02, 0xdf, 0x4e, 0xc7, 0x96, 0x1b, 0xb9, 0xd7, 0xb1, 0x5c, 0x0c, 0x5d, 0xbc, 0x42,
            0xe6, 0x8c, 0xc6, 0xef, 0x44, 0xed, 0x32, 0xf5, 0x4e, 0xda, 0xf3, 0x9a, 0xfb, 0xbb,
            0xc7, 0xf8, 0x20, 0x8d,
        ];
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_be_bytes(&k),
                X25519PubKey::from_be_bytes(&g)
            ),
            X25519SharedKey::from_be_bytes(&r)
        );
    }

    #[test]
    fn x25519_test2() {
        /// https://x25519.xargs.org/
        use super::*;
        let k: [u8; 32] = [
            0x51, 0x65, 0xc8, 0xed, 0x2f, 0xdb, 0xbd, 0x89, 0x54, 0xce, 0x07, 0x5c, 0x39, 0x7a,
            0x66, 0xfa, 0x2b, 0x35, 0x92, 0x8b, 0x96, 0x08, 0xb1, 0xe3, 0xed, 0x28, 0xc3, 0x98,
            0x53, 0x68, 0x3b, 0x82,
        ];
        let g: [u8; 32] = [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x09,
        ];
        let r: [u8; 32] = [
            0x73, 0x06, 0x69, 0x7d, 0x69, 0xf0, 0xa5, 0xe8, 0xfd, 0x38, 0x3e, 0xa1, 0xe8, 0xe9,
            0x28, 0xef, 0x97, 0x7e, 0x19, 0x93, 0x66, 0x07, 0xa5, 0x3b, 0xe0, 0xcb, 0xbf, 0x2d,
            0x88, 0x19, 0xfc, 0x16,
        ];
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_be_bytes(&k),
                X25519PubKey::from_be_bytes(&g)
            ),
            X25519SharedKey::from_be_bytes(&r)
        );
    }

    #[test]
    fn x25519_test3() {
        /// https://www.rfc-editor.org/rfc/rfc7748#section-5.2
        use super::*;

        let scalar: [u8; 32] = [
            0xa5, 0x46, 0xe3, 0x6b, 0xf0, 0x52, 0x7c, 0x9d, 0x3b, 0x16, 0x15, 0x4b, 0x82, 0x46,
            0x5e, 0xdd, 0x62, 0x14, 0x4c, 0x0a, 0xc1, 0xfc, 0x5a, 0x18, 0x50, 0x6a, 0x22, 0x44,
            0xba, 0x44, 0x9a, 0xc4,
        ];
        let u: [u8; 32] = [
            0xe6, 0xdb, 0x68, 0x67, 0x58, 0x30, 0x30, 0xdb, 0x35, 0x94, 0xc1, 0xa4, 0x24, 0xb1,
            0x5f, 0x7c, 0x72, 0x66, 0x24, 0xec, 0x26, 0xb3, 0x35, 0x3b, 0x10, 0xa9, 0x03, 0xa6,
            0xd0, 0xab, 0x1c, 0x4c,
        ];
        let expect: [u8; 32] = [
            0xc3, 0xda, 0x55, 0x37, 0x9d, 0xe9, 0xc6, 0x90, 0x8e, 0x94, 0xea, 0x4d, 0xf2, 0x8d,
            0x08, 0x4f, 0x32, 0xec, 0xcf, 0x03, 0x49, 0x1c, 0x71, 0xf7, 0x54, 0xb4, 0x07, 0x55,
            0x77, 0xa2, 0x85, 0x52,
        ];
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_le_bytes(&scalar),
                X25519PubKey::from_le_bytes(&u)
            ),
            X25519SharedKey::from_le_bytes(&expect)
        );
    }

    #[test]
    fn x25519_test4() {
        /// https://www.rfc-editor.org/rfc/rfc7748#section-5.2
        use super::*;

        let scalar: [u8; 32] = [
            0x4b, 0x66, 0xe9, 0xd4, 0xd1, 0xb4, 0x67, 0x3c, 0x5a, 0xd2, 0x26, 0x91, 0x95, 0x7d,
            0x6a, 0xf5, 0xc1, 0x1b, 0x64, 0x21, 0xe0, 0xea, 0x01, 0xd4, 0x2c, 0xa4, 0x16, 0x9e,
            0x79, 0x18, 0xba, 0x0d,
        ];
        let u: [u8; 32] = [
            0xe5, 0x21, 0x0f, 0x12, 0x78, 0x68, 0x11, 0xd3, 0xf4, 0xb7, 0x95, 0x9d, 0x05, 0x38,
            0xae, 0x2c, 0x31, 0xdb, 0xe7, 0x10, 0x6f, 0xc0, 0x3c, 0x3e, 0xfc, 0x4c, 0xd5, 0x49,
            0xc7, 0x15, 0xa4, 0x93,
        ];
        let expect: [u8; 32] = [
            0x95, 0xcb, 0xde, 0x94, 0x76, 0xe8, 0x90, 0x7d, 0x7a, 0xad, 0xe4, 0x5c, 0xb4, 0xb8,
            0x73, 0xf8, 0x8b, 0x59, 0x5a, 0x68, 0x79, 0x9f, 0xa1, 0x52, 0xe6, 0xf8, 0xf7, 0x64,
            0x7a, 0xac, 0x79, 0x57,
        ];
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_le_bytes(&scalar),
                X25519PubKey::from_le_bytes(&u)
            ),
            X25519SharedKey::from_le_bytes(&expect)
        );
    }

    #[test]
    fn x25519_test5() {
        /// https://datatracker.ietf.org/doc/html/rfc7748#section-6.1
        use super::*;

        let a_pri: [u8; 32] = [
            0x77, 0x07, 0x6d, 0x0a, 0x73, 0x18, 0xa5, 0x7d, 0x3c, 0x16, 0xc1, 0x72, 0x51, 0xb2,
            0x66, 0x45, 0xdf, 0x4c, 0x2f, 0x87, 0xeb, 0xc0, 0x99, 0x2a, 0xb1, 0x77, 0xfb, 0xa5,
            0x1d, 0xb9, 0x2c, 0x2a,
        ];
        let a_pub: [u8; 32] = [
            0x85, 0x20, 0xf0, 0x09, 0x89, 0x30, 0xa7, 0x54, 0x74, 0x8b, 0x7d, 0xdc, 0xb4, 0x3e,
            0xf7, 0x5a, 0x0d, 0xbf, 0x3a, 0x0d, 0x26, 0x38, 0x1a, 0xf4, 0xeb, 0xa4, 0xa9, 0x8e,
            0xaa, 0x9b, 0x4e, 0x6a,
        ];
        let b_pri: [u8; 32] = [
            0x5d, 0xab, 0x08, 0x7e, 0x62, 0x4a, 0x8a, 0x4b, 0x79, 0xe1, 0x7f, 0x8b, 0x83, 0x80,
            0x0e, 0xe6, 0x6f, 0x3b, 0xb1, 0x29, 0x26, 0x18, 0xb6, 0xfd, 0x1c, 0x2f, 0x8b, 0x27,
            0xff, 0x88, 0xe0, 0xeb,
        ];
        let b_pub: [u8; 32] = [
            0xde, 0x9e, 0xdb, 0x7d, 0x7b, 0x7d, 0xc1, 0xb4, 0xd3, 0x5b, 0x61, 0xc2, 0xec, 0xe4,
            0x35, 0x37, 0x3f, 0x83, 0x43, 0xc8, 0x5b, 0x78, 0x67, 0x4d, 0xad, 0xfc, 0x7e, 0x14,
            0x6f, 0x88, 0x2b, 0x4f,
        ];
        let shared: [u8; 32] = [
            0x4a, 0x5d, 0x9d, 0x5b, 0xa4, 0xce, 0x2d, 0xe1, 0x72, 0x8e, 0x3b, 0xf4, 0x80, 0x35,
            0x0f, 0x25, 0xe0, 0x7e, 0x21, 0xc9, 0x47, 0xd1, 0x9e, 0x33, 0x76, 0xf0, 0x9b, 0x3c,
            0x1e, 0x16, 0x17, 0x42,
        ];
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_le_bytes(&a_pri),
                X25519PubKey::from_le_bytes(&b_pub)
            ),
            X25519SharedKey::from_le_bytes(&shared)
        );
        assert_eq!(
            X25519::shared_secret(
                X25519PrivKey::from_le_bytes(&b_pri),
                X25519PubKey::from_le_bytes(&a_pub)
            ),
            X25519SharedKey::from_le_bytes(&shared)
        );
    }
}
