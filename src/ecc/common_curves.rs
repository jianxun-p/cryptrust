use std::sync::OnceLock;

pub use num_traits::FromBytes;

pub use crate::finite_field::{le_int_arr::LeIntArr, pfint::PFInt, FiniteField, PrimeField};

use super::{montgomery::CompressedMontgomeryCurve, FFECCompressedProjPoint, FiniteFieldIntTrait};

use std::mem::size_of;

pub(crate) mod curve25519 {
    use super::*;

    pub(crate) const SIZE: usize = 32;
    type Word = u64;
    pub(crate) type OpaqueInt = LeIntArr<Word, { SIZE / size_of::<Word>() }>;
    type Curve = CompressedMontgomeryCurve<'static, OpaqueInt>;

    const CURVE25519_PRIME_BE_BYTES: [u8; SIZE] = [
        0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xed,
    ];
    const CURVE25519_A_BE_BYTES: [u8; SIZE] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x07,
        0x6d, 0x06,
    ];
    const CURVE25519_B_BE_BYTES: [u8; SIZE] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x01,
    ];
    const CURVE25519_GENERATOR_BE_BYTES: ([u8; SIZE], [u8; SIZE], [u8; SIZE]) = (
        [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x09,
        ],
        [
            0x20, 0xae, 0x19, 0xa1, 0xb8, 0xa0, 0x86, 0xb4, 0xe0, 0x1e, 0xdd, 0x2c, 0x77, 0x48,
            0xd1, 0x4c, 0x92, 0x3d, 0x4d, 0x7e, 0x6d, 0x7c, 0x61, 0xb2, 0x29, 0xe9, 0xc5, 0xa2,
            0x7e, 0xce, 0xd3, 0xd9,
        ],
        [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x01,
        ],
    );
    const CURVE25519_SUBGROUP_ORDER_BE_BYTES: [u8; SIZE] = [
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x14, 0xde, 0xf9, 0xde, 0xa2, 0xf7, 0x9c, 0xd6, 0x58, 0x12, 0x63, 0x1a, 0x5c, 0xf5,
        0xd3, 0xed,
    ];

    static CURVE25519_PRIME_FIELD: OnceLock<PrimeField<OpaqueInt>> = OnceLock::new();
    pub fn curve25519_prime_field() -> &'static PrimeField<OpaqueInt> {
        CURVE25519_PRIME_FIELD.get_or_init(|| PrimeField::from_be_bytes(&CURVE25519_PRIME_BE_BYTES))
    }

    static CURVE25519_FINITE_FIELD: OnceLock<FiniteField<OpaqueInt>> = OnceLock::new();
    pub fn curve25519_finite_field() -> &'static FiniteField<OpaqueInt> {
        CURVE25519_FINITE_FIELD
            .get_or_init(|| FiniteField::from_be_bytes(&CURVE25519_PRIME_BE_BYTES))
    }

    static CURVE25519_A: OnceLock<PFInt<OpaqueInt>> = OnceLock::new();
    fn curve25519_a() -> &'static PFInt<'static, OpaqueInt> {
        CURVE25519_A
            .get_or_init(|| PFInt::from_be_bytes(&CURVE25519_A_BE_BYTES, curve25519_prime_field()))
    }

    static CURVE25519_B: OnceLock<PFInt<OpaqueInt>> = OnceLock::new();
    fn curve25519_b() -> &'static PFInt<'static, OpaqueInt> {
        CURVE25519_B
            .get_or_init(|| PFInt::from_be_bytes(&CURVE25519_B_BE_BYTES, curve25519_prime_field()))
    }

    static CURVE25519: OnceLock<Curve> = OnceLock::new();
    pub fn curve25519() -> &'static Curve {
        CURVE25519
            .get_or_init(|| Curve::new(*curve25519_a(), *curve25519_b(), curve25519_prime_field()))
    }
    static CURVE25519_GENERATOR: OnceLock<FFECCompressedProjPoint<'static, Curve, OpaqueInt>> =
        OnceLock::new();
    pub fn curve25519_generator() -> &'static FFECCompressedProjPoint<'static, Curve, OpaqueInt> {
        CURVE25519_GENERATOR.get_or_init(|| {
            curve25519().new_point(PFInt::from_be_bytes(
                &CURVE25519_GENERATOR_BE_BYTES.0,
                curve25519_prime_field(),
            ))
        })
    }

    static CURVE25519_SUBGROUP_ORDER: OnceLock<FiniteField<OpaqueInt>> = OnceLock::new();
    pub fn curve25519_subgroup_order() -> &'static FiniteField<OpaqueInt> {
        CURVE25519_SUBGROUP_ORDER
            .get_or_init(|| FiniteField::from_be_bytes(&CURVE25519_SUBGROUP_ORDER_BE_BYTES))
    }
}

pub(crate) mod curve448 {
    use super::*;

    pub(crate) const SIZE: usize = 56;
    type Word = u64;
    pub(crate) type OpaqueInt = LeIntArr<Word, { SIZE / size_of::<Word>() }>;
    type Curve = CompressedMontgomeryCurve<'static, OpaqueInt>;

    const CURVE448_PRIME_BE_BYTES: [u8; SIZE] = [
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    ];
    const CURVE448_A_BE_BYTES: [u8; SIZE] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x62, 0xa6,
    ];
    const CURVE448_B_BE_BYTES: [u8; SIZE] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
    ];
    const CURVE448_GENERATOR_BE_BYTES: ([u8; SIZE], [u8; SIZE], [u8; SIZE]) = (
        [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05,
        ],
        [
            0x7d, 0x23, 0x5d, 0x12, 0x95, 0xf5, 0xb1, 0xf6, 0x6c, 0x98, 0xab, 0x6e, 0x58, 0x32,
            0x6f, 0xce, 0xcb, 0xae, 0x5d, 0x34, 0xf5, 0x55, 0x45, 0xd0, 0x60, 0xf7, 0x5d, 0xc2,
            0x8d, 0xf3, 0xf6, 0xed, 0xb8, 0x02, 0x7e, 0x23, 0x46, 0x43, 0x0d, 0x21, 0x13, 0x12,
            0xc4, 0xb1, 0x50, 0x67, 0x7a, 0xf7, 0x6f, 0xd7, 0x22, 0x3d, 0x45, 0x7b, 0x5b, 0x1a,
        ],
        [
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
        ],
    );
    const CURVE448_SUBGROUP_ORDER_BE_BYTES: [u8; SIZE] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x83, 0x35,
        0xdc, 0x16, 0x3b, 0xb1, 0x24, 0xb6, 0x51, 0x29, 0xc9, 0x6f, 0xde, 0x93, 0x3d, 0x8d, 0x72,
        0x3a, 0x70, 0xaa, 0xdc, 0x87, 0x3d, 0x6d, 0x54, 0xa7, 0xbb, 0x0d,
    ];

    static CURVE448_PRIME_FIELD: OnceLock<PrimeField<OpaqueInt>> = OnceLock::new();
    pub fn curve448_prime_field() -> &'static PrimeField<OpaqueInt> {
        CURVE448_PRIME_FIELD.get_or_init(|| PrimeField::from_be_bytes(&CURVE448_PRIME_BE_BYTES))
    }

    static CURVE448_FINITE_FIELD: OnceLock<FiniteField<OpaqueInt>> = OnceLock::new();
    pub fn curve448_finite_field() -> &'static FiniteField<OpaqueInt> {
        CURVE448_FINITE_FIELD.get_or_init(|| FiniteField::from_be_bytes(&CURVE448_PRIME_BE_BYTES))
    }

    static CURVE448_A: OnceLock<PFInt<OpaqueInt>> = OnceLock::new();
    fn curve448_a() -> &'static PFInt<'static, OpaqueInt> {
        CURVE448_A
            .get_or_init(|| PFInt::from_be_bytes(&CURVE448_A_BE_BYTES, curve448_prime_field()))
    }

    static CURVE448_B: OnceLock<PFInt<OpaqueInt>> = OnceLock::new();
    fn curve448_b() -> &'static PFInt<'static, OpaqueInt> {
        CURVE448_B
            .get_or_init(|| PFInt::from_be_bytes(&CURVE448_B_BE_BYTES, curve448_prime_field()))
    }

    static CURVE448: OnceLock<Curve> = OnceLock::new();
    pub fn curve448() -> &'static Curve {
        CURVE448.get_or_init(|| Curve::new(*curve448_a(), *curve448_b(), curve448_prime_field()))
    }

    static CURVE448_GENERATOR: OnceLock<FFECCompressedProjPoint<'static, Curve, OpaqueInt>> =
        OnceLock::new();
    pub fn curve448_generator() -> &'static FFECCompressedProjPoint<'static, Curve, OpaqueInt> {
        CURVE448_GENERATOR.get_or_init(|| {
            curve448().new_point(PFInt::from_be_bytes(
                &CURVE448_GENERATOR_BE_BYTES.0,
                curve448_prime_field(),
            ))
        })
    }

    static CURVE448_SUBGROUP_ORDER: OnceLock<FiniteField<OpaqueInt>> = OnceLock::new();
    pub fn curve448_subgroup_order() -> &'static FiniteField<OpaqueInt> {
        CURVE448_SUBGROUP_ORDER
            .get_or_init(|| FiniteField::from_be_bytes(&CURVE448_SUBGROUP_ORDER_BE_BYTES))
    }
}

pub use curve25519::{
    curve25519, curve25519_finite_field, curve25519_generator, curve25519_prime_field,
    curve25519_subgroup_order,
};

pub use curve448::{
    curve448, curve448_finite_field, curve448_generator, curve448_prime_field,
    curve448_subgroup_order,
};
