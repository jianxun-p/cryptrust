use num_traits::{FromBytes, ToBytes};

use crate::{cipher::*, finite_field::primuint_traits::PrimUint};

use crate::key::Key;

/// size of a row of a block used in AES block cipher
const BLK_ROW_SIZE: usize = 4;

/// size of a column of a block used in AES block cipher
const BLK_COL_SIZE: usize = 4;

/// size of a block used in AES block cipher
const BLK_SIZE: usize = BLK_ROW_SIZE * BLK_COL_SIZE;

/// implementation of AES in u32 words
const U32_SIZE: usize = (u32::BITS / u8::BITS) as usize;
const WORD_SIZE: usize = (u32::BITS / u8::BITS) as usize;

/// Substitution values used in the SubBytes() transformation of the AES algorithm
const S_BOX: [u8; 256] = [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16,
];

/// Substitution values used in the InvSubBytes() transformation of the AES algorithm
const INV_S_BOX: [u8; 256] = [
    0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
    0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
    0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
    0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
    0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
    0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
    0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
    0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
    0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
    0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
    0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
    0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
    0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
    0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
    0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
    0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d,
];

/// modulo polynomial coefficient used for AES block cipher
/// m(x) = x^8 + x^4 + x^3 + x + 1
const AES_MODULO_POLYNOMIAL_COEF: u32 = 0b100011011;

struct State([u32; BLK_ROW_SIZE]);

#[derive(Debug)]
pub struct AES<const SIZE: usize, K: Key<SIZE>>(K);

fn get_byte(word: u32, n: usize) -> u8 {
    ((word >> (n * u8::BITS as usize)) as u32 & (u8::MAX as u32)) as u8
}

fn set_byte(word: &mut u32, n: usize, val: u8) {
    let mut tmp = word.to_le_bytes();
    tmp[n] = val;
    *word = u32::from_le_bytes(tmp)
}

/// definition of the xtime() function used in the multiplication of the Rijndael's (AES) finite field
/// See Section 4.2.1 of "FIPS 197, Advanced Encryption Standard (AES)" for details about the computation
fn ffmul_xtime(mut b: u8) -> u8 {
    let bit7: u8 = b & 0x80;
    let mask = ((bit7 as i8) >> 7) as u8;
    b <<= 1; // multiply b(x) by x
             // reduction if 7th bit == 1 ( b(x) - m(x) )
    b ^= mask & (AES_MODULO_POLYNOMIAL_COEF as u8); // take 8 bits only
    b
}

/// Implementation of the multiplication under the Rijndael's (AES) finite field
/// See Section 4.2.1 of "FIPS 197, Advanced Encryption Standard (AES)" for details about the computation
fn ffmul(mut a: u8, mut b: u8) -> u8 {
    let mut p: u8 = 0;
    // xtime() can be applied to a(x) 8 times at most
    // Beyond that, xtime() will always return 0
    for _ in 0..u8::BITS {
        p ^= a & u8::bit_to_mask(b & 1 != 0); // p ^= a if (b & 1 != 0)
        a = ffmul_xtime(a);
        b >>= 1;
    }
    p
}

/// Implementation of the SubWord() function used in the Key Expansion of AES algorithm
/// Perform S-box to a 32-bits word
/// See Section 5.2 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
fn sub_word(word: u32, s_box: &[u8; 256]) -> u32 {
    u32::from_ne_bytes(word.to_ne_bytes().map(|a| s_box[a as usize]))
}

/// Implementation of the RotWord() function used in the Key Expansion of AES algorithm
/// perform S-box to a 32-bits word
/// See Section 5.2 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
fn rot_word(word: u32) -> u32 {
    word.rotate_left(u8::BITS) // rotate left by 1 byte
}

/// Implementation of the xtime() function for 32-bits word used to
/// update the round constant word in key expansion
/// See Section 4.2 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
fn xtime_rcon(mut word: u32) -> u32 {
    let bit31: u32 = word & 0x80000000;
    let mask = ((bit31 as i32) >> 31) as u32;
    word <<= 1; // multiply by x
                // reduction if 31th bit != 0
    word ^= mask & (AES_MODULO_POLYNOMIAL_COEF as u32) << 24; // take 32 bits only
    word
}

impl State {
    /// Implementation of the SubBytes() transformation used in the AES algorithm
    /// See Section 5.1.1 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    fn sub_words(mut self, s_box: &[u8; 256]) -> Self {
        for i in 0..self.0.len() {
            self.0[i] = sub_word(self.0[i], s_box);
        }
        self
    }

    /// Implementation of the ShiftRows() transformation used in the AES algorithm
    /// See Section 5.1.2 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    fn shift_row(mut self) -> Self {
        // perform ShiftRows() on state
        for row in 0..BLK_COL_SIZE {
            let i = BLK_COL_SIZE - row - 1;
            for _ in 0..row {
                let col = self.0;
                // let (col0, col1, col2, col3) = (self.0[0], self.0[1], self.0[2], self.0[3]);
                let tmp = get_byte(col[0], i);
                set_byte(&mut self.0[0], i, get_byte(col[1], i));
                set_byte(&mut self.0[1], i, get_byte(col[2], i));
                set_byte(&mut self.0[2], i, get_byte(col[3], i));
                set_byte(&mut self.0[3], i, tmp);
            }
        }
        self
    }

    /// Implementation of the InvShiftRows() transformation used in the AES algorithm
    /// See Section 5.3.1 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    fn inv_shift_row(mut self) -> Self {
        // perform InvShiftRows() on out
        for row in 0..BLK_COL_SIZE {
            let i = BLK_COL_SIZE - row - 1;
            for _ in 0..row {
                let col = self.0;
                let tmp = get_byte(col[3], i);
                set_byte(&mut self.0[3], i, get_byte(col[2], i));
                set_byte(&mut self.0[2], i, get_byte(col[1], i));
                set_byte(&mut self.0[1], i, get_byte(col[0], i));
                set_byte(&mut self.0[0], i, tmp);
            }
        }
        self
    }

    /// Implementation of the AddRoundKey() transformation used in the AES algorithm
    /// See Section 5.1.4 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    /// Requirment: state and round_key must have size of BLK_ROW_SIZE
    fn add_round_key(mut self, round_key: &[u32]) -> Self {
        assert!(round_key.len() == BLK_ROW_SIZE);
        for i in 0..BLK_ROW_SIZE {
            self.0[i] ^= round_key[i];
        }
        self
    }

    /// Implementation of the MixColumns() transformation used in the AES algorithm
    /// See Section 5.1.3 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    fn mix_col(mut self) -> Self {
        // s'(x) = a(x) * s(x)
        // where a(x) = {03}x^3 + {01}x^2 + {01}x + {02}
        // matrix multiplcation of a(x) * s(x):
        for c in 0..BLK_ROW_SIZE {
            let col = self.0[c];
            let (s0, s1, s2, s3) = (
                get_byte(col, 3),
                get_byte(col, 2),
                get_byte(col, 1),
                get_byte(col, 0),
            );
            set_byte(&mut self.0[c], 3, ffmul(2, s0) ^ ffmul(3, s1) ^ s2 ^ s3);
            set_byte(&mut self.0[c], 2, s0 ^ ffmul(2, s1) ^ ffmul(3, s2) ^ s3);
            set_byte(&mut self.0[c], 1, s0 ^ s1 ^ ffmul(2, s2) ^ ffmul(3, s3));
            set_byte(&mut self.0[c], 0, ffmul(3, s0) ^ s1 ^ s2 ^ ffmul(2, s3));
        }
        self
    }

    /// Implementation of the InvMixColumns() transformation used in the AES algorithm
    /// See Section 5.1.3 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
    fn inv_mix_col(mut self) -> Self {
        // s'(x) = a^(-1)(x) * s(x)
        // where a^(-1)(x) = {0b}x^3 + {0d}x^2 + {09}x + {0e}
        // matrix multiplcation of a^(-1)(x) * s(x):
        for j in 0..BLK_COL_SIZE {
            let (s0, s1, s2, s3) = (
                get_byte(self.0[j], 3),
                get_byte(self.0[j], 2),
                get_byte(self.0[j], 1),
                get_byte(self.0[j], 0),
            );
            set_byte(
                &mut self.0[j],
                3,
                ffmul(0x0e, s0) ^ ffmul(0x0b, s1) ^ ffmul(0x0d, s2) ^ ffmul(0x09, s3),
            );
            set_byte(
                &mut self.0[j],
                2,
                ffmul(0x09, s0) ^ ffmul(0x0e, s1) ^ ffmul(0x0b, s2) ^ ffmul(0x0d, s3),
            );
            set_byte(
                &mut self.0[j],
                1,
                ffmul(0x0d, s0) ^ ffmul(0x09, s1) ^ ffmul(0x0e, s2) ^ ffmul(0x0b, s3),
            );
            set_byte(
                &mut self.0[j],
                0,
                ffmul(0x0b, s0) ^ ffmul(0x0d, s1) ^ ffmul(0x09, s2) ^ ffmul(0x0e, s3),
            );
        }
        self
    }
}

impl ToBytes for State {
    type Bytes = [u8; BLK_SIZE];

    fn to_be_bytes(&self) -> Self::Bytes {
        let mut bytes = [0u8; BLK_SIZE];
        for i in 0..BLK_ROW_SIZE {
            let tmp = self.0[i].to_be_bytes();
            for j in 0..BLK_COL_SIZE {
                bytes[i * BLK_ROW_SIZE + j] = tmp[j];
            }
        }
        bytes
    }

    fn to_le_bytes(&self) -> Self::Bytes {
        let mut bytes = [0u8; BLK_SIZE];
        for i in 0..BLK_ROW_SIZE {
            let tmp = self.0[BLK_ROW_SIZE - i - 1].to_le_bytes();
            for j in 0..BLK_COL_SIZE {
                bytes[i * BLK_ROW_SIZE + j] = tmp[j];
            }
        }
        bytes
    }
}

impl FromBytes for State {
    type Bytes = [u8; BLK_SIZE];

    fn from_be_bytes(bytes: &Self::Bytes) -> Self {
        let mut s = [0u32; BLK_ROW_SIZE];
        for i in 0..BLK_ROW_SIZE {
            let mut tmp = [0u8; BLK_COL_SIZE];
            for j in 0..BLK_COL_SIZE {
                tmp[j] = bytes[i * BLK_ROW_SIZE + j];
            }
            s[i] = u32::from_be_bytes(tmp);
        }
        Self(s)
    }

    fn from_le_bytes(bytes: &Self::Bytes) -> Self {
        let mut s = [0u32; BLK_ROW_SIZE];
        for i in 0..BLK_ROW_SIZE {
            let mut tmp = [0u8; BLK_COL_SIZE];
            for j in 0..BLK_COL_SIZE {
                tmp[j] = bytes[i * BLK_ROW_SIZE + j];
            }
            s[BLK_ROW_SIZE - i - 1] = u32::from_le_bytes(tmp);
        }
        Self(s)
    }
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // f.debug_tuple("State").field(&self.0).finish()
        let (s0, s1, s2, s3) = (self.0[0], self.0[1], self.0[2], self.0[3]);
        f.write_fmt(format_args!(
            "State {{\n\t{:02x} {:02x} {:02x} {:02x}\n\t{:02x} {:02x} {:02x} {:02x}\n\t{:02x} {:02x} {:02x} {:02x}\n\t{:02x} {:02x} {:02x} {:02x}\n}}",
            get_byte(s0, 0), get_byte(s1, 0), get_byte(s2, 0), get_byte(s3, 0),
            get_byte(s0, 1), get_byte(s1, 1), get_byte(s2, 1), get_byte(s3, 1),
            get_byte(s0, 2), get_byte(s1, 2), get_byte(s2, 2), get_byte(s3, 2),
            get_byte(s0, 3), get_byte(s1, 3), get_byte(s2, 3), get_byte(s3, 3),
        ))
    }
}

macro_rules! aes_key {
    ($key:ident, $key_size:expr) => {
        #[derive(Debug, Clone)]
        pub struct $key([u8; $key_size]);

        impl ToBytes for $key {
            type Bytes = [u8; $key_size];

            fn to_be_bytes(&self) -> Self::Bytes {
                self.0
            }

            fn to_le_bytes(&self) -> Self::Bytes {
                let mut le = [0u8; $key_size];
                (0..$key_size).for_each(|i| le[i] = self.0[$key_size - i]);
                le
            }
        }

        impl FromBytes for $key {
            type Bytes = [u8; $key_size];

            fn from_be_bytes(bytes: &Self::Bytes) -> Self {
                Self(bytes.clone())
            }

            fn from_le_bytes(bytes: &Self::Bytes) -> Self {
                let mut be = [0u8; $key_size];
                (0..$key_size).for_each(|i| be[i] = bytes[$key_size - i]);
                Self(be)
            }
        }

        impl Key<$key_size> for $key {
            const SIZE: usize = $key_size;
            fn from_slice(data: &[u8]) -> Self {
                let mut key_bytes = [0u8; $key_size];
                let mid = data.len().min($key_size);
                for i in 0..mid {
                    key_bytes[i] = data[i];
                }
                Self::from_ne_bytes(&key_bytes)
            }
        }
    };
}

macro_rules! impl_aes {
    ($k:ty, $key_size:expr, $nr:expr) => {
        impl AES<$key_size, $k> {
            /// Implementation of the KeyExpansion routine used in the AES algorithm to generate a key schedule
            /// See Section 5.2 of "FIPS 197, Advanced Encryption Standard (AES)" for more details
            fn key_expansion(key: &$k) -> [u32; BLK_SIZE * ($nr + 1) / WORD_SIZE] {
                let mut w = [0; BLK_SIZE * ($nr + 1) / WORD_SIZE];
                const NK: usize = $key_size / WORD_SIZE;
                const BLK_WORD_LEN: usize = BLK_SIZE / WORD_SIZE;
                let key_bytes = key.to_be_bytes();

                // "first Nk words of the expanded key are filled with the Cipher Key"
                for i in 0..NK {
                    // w[i] = net_to_host_endian<32>(reinterpret_cast<const uint32_t *>(key)[i]);
                    let (begin, end) = (i * U32_SIZE, (i + 1) * U32_SIZE);
                    w[i] = <u32 as PrimUint>::from_be_bytes(&key_bytes[begin..end]);
                }

                let mut rcon_i: u32 = 0x00800000;
                for i in NK..BLK_WORD_LEN * ($nr + 1) {
                    let mut temp = w[i - 1];
                    if i % NK == 0 {
                        rcon_i = xtime_rcon(rcon_i); // update Rcon using xtime()
                        temp = rot_word(temp);
                        temp = sub_word(temp, &S_BOX);
                        temp = temp ^ rcon_i;
                    } else if NK > 6 && i % NK == 4 {
                        temp = sub_word(temp, &S_BOX);
                    }
                    w[i] = temp ^ w[i - NK];
                }

                w
            }
        }

        impl Cipher<$key_size, $k> for AES<$key_size, $k> {
            type Input = [u8; BLK_SIZE];
            type Output = [u8; BLK_SIZE];

            fn encrypt(plaintext: &Self::Input, key: &$k) -> Self::Output {
                const BLK_WORD_LEN: usize = BLK_SIZE / WORD_SIZE;

                let init_state: State = State::from_be_bytes(plaintext);

                let w = Self::key_expansion(key); // generate keys

                // initial round
                let mut state = init_state.add_round_key(&w[0..BLK_WORD_LEN]);

                // round 1 to Nr-1
                state = (1..$nr).fold(state, |s, i| {
                    s.sub_words(&S_BOX)
                        .shift_row()
                        .mix_col()
                        .add_round_key(&w[(i * BLK_WORD_LEN)..(i * BLK_WORD_LEN + BLK_WORD_LEN)])
                });

                // final round
                state = state
                    .sub_words(&S_BOX)
                    .shift_row()
                    .add_round_key(&w[($nr * BLK_WORD_LEN)..($nr * BLK_WORD_LEN + BLK_WORD_LEN)]);

                state.to_be_bytes()
            }

            fn decrypt(ciphertext: &Self::Input, key: &$k) -> Self::Output {
                const BLK_WORD_LEN: usize = BLK_SIZE / WORD_SIZE;

                // state = in (16 Bytes)
                let mut state: State = State::from_be_bytes(ciphertext);

                let w = Self::key_expansion(key); // generate keys

                // initial round
                state = state.add_round_key(&w[($nr * BLK_WORD_LEN)..(($nr + 1) * BLK_WORD_LEN)]);

                // round 1 to Nr-1
                state = (1..$nr).rev().fold(state, |s, i| {
                    let keyblk_index = (i * BLK_WORD_LEN, (i + 1) * BLK_WORD_LEN);
                    s.inv_shift_row()
                        .sub_words(&INV_S_BOX)
                        .add_round_key(&w[keyblk_index.0..keyblk_index.1])
                        .inv_mix_col()
                });

                // final round
                state = state
                    .inv_shift_row()
                    .sub_words(&INV_S_BOX)
                    .add_round_key(&w[..BLK_ROW_SIZE]);

                // out = state (16 Bytes)
                state.to_be_bytes()
            }
        }

        impl BlockCipher<$key_size, $k, BLK_SIZE> for AES<$key_size, $k> {}
    };
}

macro_rules! impl_aes_helper {
    ($k:ty) => {
        impl_aes!($k, {<$k>::SIZE}, <$k>::SIZE / WORD_SIZE + 6);
    };
}

aes_key!(AES128Key, 16);
aes_key!(AES192Key, 24);
aes_key!(AES256Key, 32);

impl_aes_helper!(AES128Key);
impl_aes_helper!(AES192Key);
impl_aes_helper!(AES256Key);

pub type AES128 = AES<16, AES128Key>;
pub type AES192 = AES<24, AES192Key>;
pub type AES256 = AES<32, AES256Key>;

#[cfg(test)]
mod test {

    #[test]
    fn key_expansion_test1() {
        use super::*;
        let key: AES128Key = AES128Key([
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,
            0x4f, 0x3c,
        ]);
        assert_eq!(
            AES128::key_expansion(&key),
            [
                0x2b7e1516, 0x28aed2a6, 0xabf71588, 0x09cf4f3c, 0xa0fafe17, 0x88542cb1, 0x23a33939,
                0x2a6c7605, 0xf2c295f2, 0x7a96b943, 0x5935807a, 0x7359f67f, 0x3d80477d, 0x4716fe3e,
                0x1e237e44, 0x6d7a883b, 0xef44a541, 0xa8525b7f, 0xb671253b, 0xdb0bad00, 0xd4d1c6f8,
                0x7c839d87, 0xcaf2b8bc, 0x11f915bc, 0x6d88a37a, 0x110b3efd, 0xdbf98641, 0xca0093fd,
                0x4e54f70e, 0x5f5fc9f3, 0x84a64fb2, 0x4ea6dc4f, 0xead27321, 0xb58dbad2, 0x312bf560,
                0x7f8d292f, 0xac7766f3, 0x19fadc21, 0x28d12941, 0x575c006e, 0xd014f9a8, 0xc9ee2589,
                0xe13f0cc8, 0xb6630ca6,
            ]
        );
    }

    #[test]
    fn key_expansion_test2() {
        use super::*;
        let key: AES192Key = AES192Key([
            0x8e, 0x73, 0xb0, 0xf7, 0xda, 0x0e, 0x64, 0x52, 0xc8, 0x10, 0xf3, 0x2b, 0x80, 0x90,
            0x79, 0xe5, 0x62, 0xf8, 0xea, 0xd2, 0x52, 0x2c, 0x6b, 0x7b,
        ]);
        assert_eq!(
            AES192::key_expansion(&key),
            [
                0x8e73b0f7, 0xda0e6452, 0xc810f32b, 0x809079e5, 0x62f8ead2, 0x522c6b7b, 0xfe0c91f7,
                0x2402f5a5, 0xec12068e, 0x6c827f6b, 0x0e7a95b9, 0x5c56fec2, 0x4db7b4bd, 0x69b54118,
                0x85a74796, 0xe92538fd, 0xe75fad44, 0xbb095386, 0x485af057, 0x21efb14f, 0xa448f6d9,
                0x4d6dce24, 0xaa326360, 0x113b30e6, 0xa25e7ed5, 0x83b1cf9a, 0x27f93943, 0x6a94f767,
                0xc0a69407, 0xd19da4e1, 0xec1786eb, 0x6fa64971, 0x485f7032, 0x22cb8755, 0xe26d1352,
                0x33f0b7b3, 0x40beeb28, 0x2f18a259, 0x6747d26b, 0x458c553e, 0xa7e1466c, 0x9411f1df,
                0x821f750a, 0xad07d753, 0xca400538, 0x8fcc5006, 0x282d166a, 0xbc3ce7b5, 0xe98ba06f,
                0x448c773c, 0x8ecc7204, 0x01002202,
            ]
        );
    }

    #[test]
    fn key_expansion_test3() {
        use super::*;
        let key: AES256Key = AES256Key([
            0x60, 0x3d, 0xeb, 0x10, 0x15, 0xca, 0x71, 0xbe, 0x2b, 0x73, 0xae, 0xf0, 0x85, 0x7d,
            0x77, 0x81, 0x1f, 0x35, 0x2c, 0x07, 0x3b, 0x61, 0x08, 0xd7, 0x2d, 0x98, 0x10, 0xa3,
            0x09, 0x14, 0xdf, 0xf4,
        ]);
        assert_eq!(
            AES256::key_expansion(&key),
            [
                0x603deb10, 0x15ca71be, 0x2b73aef0, 0x857d7781, 0x1f352c07, 0x3b6108d7, 0x2d9810a3,
                0x0914dff4, 0x9ba35411, 0x8e6925af, 0xa51a8b5f, 0x2067fcde, 0xa8b09c1a, 0x93d194cd,
                0xbe49846e, 0xb75d5b9a, 0xd59aecb8, 0x5bf3c917, 0xfee94248, 0xde8ebe96, 0xb5a9328a,
                0x2678a647, 0x98312229, 0x2f6c79b3, 0x812c81ad, 0xdadf48ba, 0x24360af2, 0xfab8b464,
                0x98c5bfc9, 0xbebd198e, 0x268c3ba7, 0x09e04214, 0x68007bac, 0xb2df3316, 0x96e939e4,
                0x6c518d80, 0xc814e204, 0x76a9fb8a, 0x5025c02d, 0x59c58239, 0xde136967, 0x6ccc5a71,
                0xfa256395, 0x9674ee15, 0x5886ca5d, 0x2e2f31d7, 0x7e0af1fa, 0x27cf73c3, 0x749c47ab,
                0x18501dda, 0xe2757e4f, 0x7401905a, 0xcafaaae3, 0xe4d59b34, 0x9adf6ace, 0xbd10190d,
                0xfe4890d1, 0xe6188d0b, 0x046df344, 0x706c631e,
            ]
        );
    }

    #[test]
    fn aes_encrypt_test1() {
        /// "Appendix B – Cipher Example" of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
        use super::*;
        let key: AES128Key = AES128Key([
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,
            0x4f, 0x3c,
        ]);
        let input: [u8; 16] = [
            0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37,
            0x07, 0x34,
        ];
        let output: [u8; 16] = [
            0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb, 0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a,
            0x0b, 0x32,
        ];
        assert_eq!(AES128::encrypt(&input, &key), output);
    }

    #[test]
    fn aes128_test1() {
        /// "Appendix C – Example Vectors" of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
        use super::*;
        let key: AES128Key = AES128Key([
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f,
        ]);
        let input: [u8; 16] = [
            0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd,
            0xee, 0xff,
        ];
        let output: [u8; 16] = [
            0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04, 0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4,
            0xc5, 0x5a,
        ];
        assert_eq!(AES128::encrypt(&input, &key), output);
        assert_eq!(AES128::decrypt(&output, &key), input);
    }

    #[test]
    fn aes192_test1() {
        /// "Appendix C – Example Vectors" of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
        use super::*;
        let key: AES192Key = AES192Key([
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
        ]);
        let input: [u8; 16] = [
            0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd,
            0xee, 0xff,
        ];
        let output: [u8; 16] = [
            0xdd, 0xa9, 0x7c, 0xa4, 0x86, 0x4c, 0xdf, 0xe0, 0x6e, 0xaf, 0x70, 0xa0, 0xec, 0x0d,
            0x71, 0x91,
        ];
        assert_eq!(AES192::encrypt(&input, &key), output);
        assert_eq!(AES192::decrypt(&output, &key), input);
    }

    #[test]
    fn aes256_test1() {
        /// "Appendix C – Example Vectors" of https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.197.pdf
        use super::*;
        let key: AES256Key = AES256Key([
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
            0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b,
            0x1c, 0x1d, 0x1e, 0x1f,
        ]);
        let input: [u8; 16] = [
            0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd,
            0xee, 0xff,
        ];
        let output: [u8; 16] = [
            0x8e, 0xa2, 0xb7, 0xca, 0x51, 0x67, 0x45, 0xbf, 0xea, 0xfc, 0x49, 0x90, 0x4b, 0x49,
            0x60, 0x89,
        ];
        assert_eq!(AES256::encrypt(&input, &key), output);
        assert_eq!(AES256::decrypt(&output, &key), input);
    }
}
