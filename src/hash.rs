use sha2::{Digest, Sha224, Sha256, Sha384, Sha512};
// mod sha2;

pub trait CryptoHashTrait {
    type Output: AsRef<[u8]>;
    fn new(digest: CryptoHashType) -> Self;
    fn output_size(&self) -> usize;
    fn update(self, data: impl AsRef<[u8]>) -> Self;
    fn finalize(self) -> Self::Output;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CryptoHashType {
    Null,
    Sha224,
    Sha256,
    Sha384,
    Sha512,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CryptoHash {
    data: Vec<u8>,
    digest: CryptoHashType,
}

impl CryptoHash {
    pub const fn const_new(digest: CryptoHashType) -> Self {
        Self {
            data: Vec::new(),
            digest
        }
    }
}


impl CryptoHashTrait for CryptoHash {
    type Output = Vec<u8>;

    fn new(digest: CryptoHashType) -> Self {
        Self::const_new(digest)
    }

    fn output_size(&self) -> usize {
        match self.digest {
            CryptoHashType::Null => 0,
            CryptoHashType::Sha224 => Sha224::output_size(),
            CryptoHashType::Sha256 => Sha256::output_size(),
            CryptoHashType::Sha384 => Sha384::output_size(),
            CryptoHashType::Sha512 => Sha512::output_size(),
        }
    }

    fn update(mut self, data: impl AsRef<[u8]>) -> Self {
        self.data.extend_from_slice(data.as_ref());
        self
    }

    fn finalize(self) -> Self::Output {

        match self.digest {
            CryptoHashType::Null => Vec::<u8>::new(),
            CryptoHashType::Sha224 => {
                let mut v = Vec::<u8>::new();
                let mut digest = Sha224::new();
                digest.update(self.data.as_slice());
                v.extend_from_slice(digest.finalize().as_slice());
                v
            },
            CryptoHashType::Sha256 => {
                let mut v = Vec::<u8>::new();
                let mut digest = Sha256::new();
                digest.update(self.data.as_slice());
                v.extend_from_slice(digest.finalize().as_slice());
                v
            },
            CryptoHashType::Sha384 => {
                let mut v = Vec::<u8>::new();
                let mut digest = Sha256::new();
                digest.update(self.data.as_slice());
                v.extend_from_slice(digest.finalize().as_slice());
                v
            },
            CryptoHashType::Sha512 => {
                let mut v = Vec::<u8>::new();
                let mut digest = Sha512::new();
                digest.update(self.data.as_slice());
                v.extend_from_slice(digest.finalize().as_slice());
                v
            },
        }
    }
    
}
