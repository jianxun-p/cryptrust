
#[macro_export]
macro_rules! impl_trait_for_biguint_for_size {
    ($size:literal, $mac:ident) => {
        // $mac!(usize, { ($size as usize).div_ceil((usize::BITS / u8::BITS) as usize) }, $size);
        // $mac!(u8, { ($size as usize).div_ceil((u8::BITS / u8::BITS) as usize) }, $size);
        // $mac!(u16, { ($size as usize).div_ceil((u16::BITS / u8::BITS) as usize) }, $size);
        // $mac!(u32, { ($size as usize).div_ceil((u32::BITS / u8::BITS) as usize) }, $size);
        $mac!(u64, { ($size as usize).div_ceil((u64::BITS / u8::BITS) as usize) }, $size);
        // $mac!(u128, { ($size as usize).div_ceil((u128::BITS / u8::BITS) as usize) }, $size);
    };
}

#[macro_export]
macro_rules! impl_trait_for_biguint {
    ($mac:ident) => {
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(1, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(2, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(4, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(8, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(12, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(14, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(16, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(24, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(28, $mac);
        crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(32, $mac);
        crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(56, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(64, $mac);
        crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(96, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(112, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(128, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(224, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(256, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(448, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(512, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(896, $mac);
        // crate::finite_field::macro_helper::impl_trait_for_biguint_for_size!(1024, $mac);
    };
}


#[macro_export]
macro_rules! impl_trait_for_u8_arr {
    ($mac:ident) => {
        // $mac!(1);
        // $mac!(2);
        // $mac!(3);
        // $mac!(4);
        // $mac!(4);
        // $mac!(5);
        // $mac!(6);
        // $mac!(7);
        // $mac!(8);
        // $mac!(9);
        // $mac!(10);
        // $mac!(11);
        // $mac!(12);
        // $mac!(13);
        // $mac!(14);
        // $mac!(15);
        // $mac!(16);
        // $mac!(17);
        // $mac!(18);
        // $mac!(19);
        // $mac!(20);
        // $mac!(21);
        // $mac!(22);
        // $mac!(23);
        // $mac!(24);
        // $mac!(25);
        // $mac!(26);
        // $mac!(27);
        // $mac!(28);
        // $mac!(30);
        $mac!(32);
        // $mac!(34);
        // $mac!(35);
        // $mac!(36);
        // $mac!(38);
        // $mac!(40);
        // $mac!(42);
        // $mac!(44);
        // $mac!(45);
        // $mac!(46);
        $mac!(56);
        // $mac!(64);
        $mac!(96);
        // $mac!(112);
        // $mac!(128);
        // $mac!(224);
        // $mac!(256);
        // $mac!(448);
        // $mac!(512);
        // $mac!(896);
        // $mac!(1024);
    };
}



pub use impl_trait_for_biguint_for_size;
pub use impl_trait_for_biguint;
// pub use impl_trait_for_u8_arr;
