

#[test]
fn test_uint_arr() {
    use super::{*, uint_arr::UintArr};
    use num_traits::{Pow, ConstZero};
    type T = UintArr<u8, 2, 2>;
    let a = T::from_word(2);
    assert_eq!(a + a.neg(), T::ZERO);
    let b = T::from_word(2);
    assert_eq!(b.pow(3), b * b * b);
}

#[test]
fn test_eq() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    type T = UintArr<u8, 2, 2>;
    let field1_bytes: [u8; 2] = [61, 0]; // (mod 61)
    let a_bytes: [u8; 2] = [2, 0]; // 2
    let b_bytes: [u8; 2] = [63, 0]; // 63
    let field1 = PrimeField::<T>::from_le_bytes(&field1_bytes);
    let a = PFInt::<T>::from_le_bytes(&a_bytes, &field1);
    let b = PFInt::<T>::from_le_bytes(&b_bytes, &field1);

    assert_eq!(a, b); // 2 = 63 (mod 61)
}

#[test]
fn test_add_sub() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    use num_traits::{CheckedAdd, CheckedSub};
    type T = UintArr<u8, 2, 2>;

    let field1_bytes: [u8; 2] = [79, 145]; // (mod 37199)
    let field2_bytes: [u8; 2] = [61, 0]; // (mod 61)
    let field3_bytes: [u8; 2] = [17, 0]; // (mod 17)
    let a_bytes: [u8; 2] = [3, 0]; // 3
    let b_bytes: [u8; 2] = [6, 0]; // 6
    let c_bytes: [u8; 2] = [78, 145]; // 37198
    let d_bytes: [u8; 2] = [181, 110]; // 28341
    let e_bytes: [u8; 2] = [180, 110]; // 28340
    let f_bytes: [u8; 2] = [78, 145]; // 37198
    let z_bytes: [u8; 2] = [2, 0]; // 2

    let field1 = PrimeField::<T>::from_le_bytes(&field1_bytes);
    let field2 = PrimeField::<T>::from_le_bytes(&field2_bytes);
    let field3 = PrimeField::<T>::from_le_bytes(&field3_bytes);
    let a = PFInt::<T>::from_le_bytes(&a_bytes, &field1);
    let b = PFInt::<T>::from_le_bytes(&b_bytes, &field1);
    let c = PFInt::<T>::from_le_bytes(&c_bytes, &field1);
    let d = PFInt::<T>::from_le_bytes(&d_bytes, &field1);
    let e = PFInt::<T>::from_le_bytes(&e_bytes, &field1);
    let f = PFInt::<T>::from_le_bytes(&f_bytes, &field1);
    let z = PFInt::<T>::from_le_bytes(&z_bytes, &field2);

    assert_eq!(
        PFInt::zero(&field3) - PFInt::new(&field3, T::from_word(225)),
        PFInt::new(&field3, T::from_word(13))
    );

    assert_eq!(a.clone() + a.clone(), b.clone()); // 3 + 3 = 6 (mod 37199)
    assert_eq!(c.clone() + d.clone(), e.clone()); // 37198 + 28341 = 28340 (mod 37199)
    assert_eq!(a.clone().checked_add(&z), None); // different field

    assert_eq!(b - a.clone(), a.clone()); // 6 - 3 = 3 (mod 37199)
    assert_eq!(e - d, f); // 28340 - 28341 = 37198 (mod 37199)
    assert_eq!(a.clone().checked_sub(&z), None); // different field
}

#[test]
fn test_double() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    type T = UintArr<u8, 2, 2>;

    let field1_bytes: [u8; 2] = [61, 0]; // (mod 61)
    let a_bytes: [u8; 2] = [2, 0]; // 2
    let b_bytes: [u8; 2] = [3, 0]; // 3
    let c_bytes: [u8; 2] = [6, 0]; // 6
    let d_bytes: [u8; 2] = [11, 0]; // 11
    let e_bytes: [u8; 2] = [50, 0]; // 50
    let f_bytes: [u8; 2] = [255, 255]; // 65535
    let field1 = PrimeField::<T>::from_le_bytes(&field1_bytes);
    let a = PFInt::<T>::from_le_bytes(&a_bytes, &field1);
    let b = PFInt::<T>::from_le_bytes(&b_bytes, &field1);
    let c = PFInt::<T>::from_le_bytes(&c_bytes, &field1);
    let d = PFInt::<T>::from_le_bytes(&d_bytes, &field1);
    let e = PFInt::<T>::from_le_bytes(&e_bytes, &field1);
    let f = PFInt::<T>::from_le_bytes(&f_bytes, &field1);

    assert_eq!(a.clone() + a.clone(), a.double());
    assert_eq!(b.clone() + b.clone(), b.double());
    assert_eq!(c.clone() + c.clone(), c.double());
    assert_eq!(d.clone() + d.clone(), d.double());
    assert_eq!(e.clone() + e.clone(), e.double());
    assert_eq!(f.clone() + f.clone(), f.double());
}

#[test]
fn test_mul() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    type T = UintArr<u8, 2, 2>;

    let field1_bytes: [u8; 2] = [61, 0]; // (mod 61)
    let field2_be_bytes: [u8; 56] = [
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    ];
    let a_bytes: [u8; 2] = [2, 0]; // 2
    let b_bytes: [u8; 2] = [3, 0]; // 3
    let c_bytes: [u8; 2] = [6, 0]; // 6
    let d_bytes: [u8; 2] = [11, 0]; // 11
    let e_be_bytes: [u8; 56] = [
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x98, 0xaa,
    ];
    let f_be_bytes: [u8; 56] = [
        0x1a, 0x83, 0xe1, 0x3b, 0xee, 0x9e, 0x9e, 0x2a, 0x86, 0xb6, 0x53, 0x6c, 0xa5, 0x53, 0xea,
        0x0c, 0x97, 0x6b, 0x18, 0x66, 0x33, 0x0e, 0xf7, 0x74, 0xc7, 0x01, 0x36, 0x09, 0xe4, 0x20,
        0x3c, 0x09, 0xfd, 0x0c, 0x1e, 0xf5, 0x30, 0xce, 0x22, 0xb6, 0x28, 0xfc, 0x9b, 0x57, 0xc9,
        0xb1, 0x7f, 0x6a, 0xfe, 0x1c, 0xd3, 0xe9, 0x03, 0x9b, 0xf0, 0x1a,
    ];
    let g_be_bytes: [u8; 56] = [
        0xe9, 0x4f, 0x27, 0x7a, 0xa3, 0x3e, 0x48, 0x39, 0xb6, 0x9b, 0xe8, 0x4f, 0x9c, 0xb0, 0xe6,
        0x44, 0x23, 0x9a, 0xe2, 0x2e, 0xca, 0xdd, 0xa9, 0xb4, 0xde, 0xe3, 0xd1, 0xd3, 0xa1, 0x0d,
        0xcc, 0xe1, 0x3c, 0x6e, 0x23, 0xcc, 0xcd, 0x7f, 0x35, 0x4d, 0x33, 0xfb, 0x48, 0x0d, 0x53,
        0x86, 0x23, 0xed, 0xdc, 0xf7, 0x12, 0xe0, 0xfc, 0x1c, 0xf1, 0x13,
    ];
    let field1 = PrimeField::<T>::from_le_bytes(&field1_bytes);
    let field2 = PrimeField::<UintArr<u8, 56, 56>>::from_be_bytes(&field2_be_bytes);
    let a = PFInt::<T>::from_le_bytes(&a_bytes, &field1);
    let b = PFInt::<T>::from_le_bytes(&b_bytes, &field1);
    let c = PFInt::<T>::from_le_bytes(&c_bytes, &field1);
    let d = PFInt::<T>::from_le_bytes(&d_bytes, &field1);
    let e = PFInt::<UintArr<u8, 56, 56>>::from_be_bytes(&e_be_bytes, &field2);
    let f = PFInt::<UintArr<u8, 56, 56>>::from_be_bytes(&f_be_bytes, &field2);
    let g = PFInt::<UintArr<u8, 56, 56>>::from_be_bytes(&g_be_bytes, &field2);

    assert_eq!(a.clone() * b.clone(), c.clone()); // 2 * 3 = 6 (mod 61)
    assert_eq!(c * d, a + b); // 6 * 11 = 66 = 5 = 2 + 3 (mod 61)
    assert_eq!(e * f, g);
}

#[test]
fn test_modinv() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    use num_traits::Inv;
    type T = UintArr<u8, 2, 2>;

    let field1_bytes: [u8; 2] = [61, 0]; // (mod 61)
    let a_bytes: [u8; 2] = [2, 0]; // 2
    let b_bytes: [u8; 2] = [3, 0]; // 3
    let c_bytes: [u8; 2] = [6, 0]; // 6
    let d_bytes: [u8; 2] = [11, 0]; // 11
    let e_bytes: [u8; 2] = [50, 0]; // 50
    let field1 = PrimeField::<T>::from_le_bytes(&field1_bytes);
    let a = PFInt::<T>::from_le_bytes(&a_bytes, &field1);
    let b = PFInt::<T>::from_le_bytes(&b_bytes, &field1);
    let c = PFInt::<T>::from_le_bytes(&c_bytes, &field1);
    let d = PFInt::<T>::from_le_bytes(&d_bytes, &field1);
    let e = PFInt::<T>::from_le_bytes(&e_bytes, &field1);

    assert_eq!(a.inv(), PFInt::<T>::new(&field1, T::from_word(31)));
    assert_eq!(b.inv(), PFInt::<T>::new(&field1, T::from_word(41)));
    assert_eq!(c.inv(), PFInt::<T>::new(&field1, T::from_word(51)));
    assert_eq!(d.inv(), PFInt::<T>::new(&field1, T::from_word(50)));
    assert_eq!(e.inv(), PFInt::<T>::new(&field1, T::from_word(11)));
}

#[test]
fn test_pow() {
    use super::{*, uint_arr::UintArr, pfint::PFInt, ffint::FiniteFieldIntTrait};
    use num_traits::{ConstZero, ConstOne, Pow};
    type T = UintArr<u8, 2, 2>;
    let field1 = PrimeField::<T>::from_be_bytes(&[0, 5]);
    let a = PFInt::<T>::from_be_bytes(&[0, 2], &field1);
    let k3 = <T as FromBytes>::from_be_bytes(&[0, 3]);
    assert_eq!(a.pow(T::ZERO), PFInt::<T>::one(&field1));
    assert_eq!(a.pow(T::ONE), a);
    assert_eq!(a.pow(k3), a * a * a);
}
