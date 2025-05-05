use std::ops::Index;
use std::ops::Range;
use std::slice::SliceIndex;

mod traits {
    pub trait Store: AsRef<str> {
        type Error;
        fn store(&self, _: &impl AsRef<str>) -> Result<impl Key, Self::Error>;
    }

    pub trait Key: AsRef<str> + AsRef<[u8]> + std::hash::Hash {}
}

pub use traits::Key as KeyT;
pub use traits::Store as StoreT;

pub use zero_copy::RefKey as Key;
pub use zero_copy::TinyKey;
pub use zero_copy::{AsciiStore as Store, NotAscii};

mod zero_copy {
    use super::{Index, Range};
    use super::{KeyT, StoreT};
    use concat_string::ConcatString;
    use std::slice::SliceIndex;
    use tinystr::TinyAsciiStr;

    pub struct AsciiStore(concat_string::ConcatString);
    #[derive(Debug, thiserror::Error)]
    pub struct NotAscii();

    impl fmt::Display for NotAscii {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("Non Ascii Bytes Found")
        }
    }
    /*
    impl Store for AsciiStore {
        fn store
    }
    */

    impl AsciiStore {
        fn new(str: Vec<u8>) -> Result<Self, NotAscii> {
            str.is_ascii()
                .then(|| {
                    Self(ConcatString::from(unsafe {
                        String::from_utf8_unchecked(str)
                    }))
                })
                .ok_or(NotAscii())
        }

        fn len(&self) -> usize {
            self.0.len()
        }

        pub unsafe fn store_unchecked(&self, new: &str) -> RefKey {
            let start = self.len() - 1;
            let end = start + new.len();
            self.0.push(new);
            RefKey(unsafe { self.0.get_unchecked(start..end) })
        }
    }

    impl AsRef<str> for AsciiStore {
        fn as_ref(&self) -> &str {
            &self.0
        }
    }

    impl StoreT for AsciiStore {
        type Error = NotAscii;
        fn store(&self, new: &impl AsRef<str>) -> Result<impl super::traits::Key, Self::Error> {
            new.as_ref()
                .is_ascii()
                .then(|| unsafe { self.store_unchecked(new.as_ref()) })
                .ok_or(NotAscii())
        }
    }

    impl<T: SliceIndex<str>> Index<T> for AsciiStore {
        type Output = T::Output;
        fn index(&self, index: T) -> &T::Output {
            // safe because we validate in new/try_from
            unsafe { self.0.get_unchecked(index) }
        }
    }

    impl TryFrom<Vec<u8>> for AsciiStore {
        type Error = NotAscii;
        fn try_from(str: Vec<u8>) -> Result<Self, NotAscii> {
            Self::new(str)
        }
    }

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct RefKey<'a>(&'a str);

    #[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
    pub struct TinyKey<const N: usize>(TinyAsciiStr<N>);

    impl<const N: usize> TinyKey<N> {
        unsafe fn new(slice: [u8; N]) -> Self {
            Self(unsafe { TinyAsciiStr::from_utf8_unchecked(slice) })
        }
    }

    impl<'a> RefKey<'a> {
        pub const fn new(parent: &'a str) -> Self {
            Self(parent)
        }
    }

    impl<const N: usize> AsRef<str> for TinyKey<N> {
        fn as_ref(&self) -> &str {
            self.0.as_ref()
        }
    }

    impl<const N: usize> AsRef<[u8]> for TinyKey<N> {
        fn as_ref(&self) -> &[u8] {
            self.0.as_bytes()
        }
    }

    impl AsRef<str> for RefKey<'_> {
        fn as_ref(&self) -> &str {
            self.0
        }
    }

    impl AsRef<[u8]> for RefKey<'_> {
        fn as_ref(&self) -> &[u8] {
            self.0.as_bytes()
        }
    }

    impl KeyT for RefKey<'_> {}

    impl<const N: usize> KeyT for TinyKey<N> {}

    use std::fmt::{self, Display, Formatter};

    impl Display for RefKey<'_> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    mod concat_string {

        use std::cell::UnsafeCell;
        pub struct ConcatString(UnsafeCell<String>);
        use std::ops::Index;
        use std::slice::SliceIndex;

        impl<I: SliceIndex<str>> Index<I> for ConcatString {
            type Output = <I as SliceIndex<str>>::Output;
            fn index(&self, i: I) -> &Self::Output {
                unsafe { self.borrow() }.index(i)
            }
        }

        impl ConcatString {
            unsafe fn borrow(&self) -> &str {
                unsafe { self.0.get().as_ref().unwrap_unchecked() }.as_ref()
            }

            #[allow(clippy::mut_from_ref)]
            const unsafe fn borrow_mut(&self) -> &mut String {
                unsafe { self.0.get().as_mut().unwrap_unchecked() }
            }

            pub fn push(&self, s: &str) {
                (unsafe { self.borrow_mut() }).push_str(s);
            }
        }
        use std::ops::Deref;
        impl Deref for ConcatString {
            type Target = str;
            fn deref(&self) -> &Self::Target {
                unsafe { self.borrow() }
            }
        }

        impl From<String> for ConcatString {
            fn from(string: String) -> Self {
                Self(UnsafeCell::new(string))
            }
        }
    }
}
/*
mod interning {
    use ascii::AsciiString;

    use ascii::AsciiStr;
    use std::cell::UnsafeCell;
    use tinystr::TinyAsciiStr;

    // key value pair
    // ValueVec

    pub struct StringTable<'a>(&'a ValueStore);

    // what do we borrow again?
    // all keys have to live as long as like one borrow of valuestore

    impl<'a> StringTable<'a> {
        pub const fn new(store: &'a ValueStore) -> Self {
            Self(store)
        }

        const fn key(&self, slice: &'a [u8]) -> Key<'a> {
            Key(slice)
        }

        pub fn get_or_intern(&self, str: &[u8]) -> Key<'a> {
            let slice = self
                .0
                .intern(unsafe { AsciiStr::from_ascii_unchecked(str) });
            self.key(slice)
        }

        pub fn constains(&self, str: &AsciiStr) -> bool {
            self.find(str).is_some()
        }

        pub fn find(&self, str: &AsciiStr) -> Option<Key> {
            self.0.find(str).map(|x| self.key(x))
        }
    }

    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    struct RawSlice {
        key: *const u8,
        len: usize,
    }

    impl RawSlice {
        const fn new(slice: &[u8]) -> Self {
            Self {
                len: slice.len(),
                key: slice.as_ptr(),
            }
        }

        const fn slice(&self) -> &[u8] {
            unsafe { std::slice::from_raw_parts(self.key, self.len) }
        }

        const fn str(&self) -> &str {
            unsafe { std::str::from_utf8_unchecked(self.slice()) }
        }

        fn ascii_str(&self) -> &AsciiStr {
            unsafe { AsciiStr::from_ascii_unchecked(self.slice()) }
        }
    }

    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct Key<'a>(&'a [u8]);
    use std::fmt::{self, Formatter};
    impl std::fmt::Display for Key<'_> {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            unsafe { std::str::from_utf8_unchecked(self.0).fmt(f) }
        }
    }

    impl Key<'_> {
        pub fn as_bytes(&self) -> &[u8] {
            self.0
        }
    }

    /*
    #[derive(Copy, Clone, PartialEq, Eq, Debug)]
    pub struct Key<'a> {
        slice: RawSlice,
        marker: std::marker::PhantomData<&'a StringTable>,
    }

    impl<'a> Key<'a> {
        const fn new(slice: RawSlice, _parent: &StringTable) -> Self {
            Self {
                slice: RawSlice,
                marker: std::marker::PhantomData,
            }
        }
    }
    */

    impl<const N: usize> From<&TinyAsciiStr<N>> for RawSlice {
        fn from(&other: &TinyAsciiStr<N>) -> Self {
            Self::new(other.as_bytes())
        }
    }

    impl From<&AsciiStr> for RawSlice {
        fn from(other: &AsciiStr) -> Self {
            Self::new(other.as_bytes())
        }
    }

    pub struct ValueStore(UnsafeCell<ValueStoreInner>);

    impl ValueStore {
        pub const fn new() -> Self {
            Self(UnsafeCell::new(ValueStoreInner::c_default()))
        }

        pub fn intern(&self, str: &AsciiStr) -> &[u8] {
            let inner = unsafe { self.0.get().as_mut().unwrap() };
            // safe because we're in a single threaded context (since keys aren't Sync)
            if let Some(str) = inner.find(str) {
                str
            } else {
                let inner = unsafe { self.0.get().as_mut().unwrap() };
                inner.insert(str)
            }
        }

        pub fn find(&self, str: &AsciiStr) -> Option<&[u8]> {
            // safe because we're in a single threaded context (since keys aren't Sync)
            let inner = unsafe { self.0.get().as_mut().unwrap() };
            inner.find(str)
        }

        pub fn intern_mut(&mut self, str: &AsciiStr) -> &[u8] {
            self.0.get_mut().insert(str)
        }
    }

    pub struct ValueStoreInner {
        t1: Vec<TinyAsciiStr<1>>,
        t2: Vec<TinyAsciiStr<2>>,
        t3: Vec<TinyAsciiStr<3>>,
        t4: Vec<TinyAsciiStr<4>>,
        t5: Vec<TinyAsciiStr<5>>,
        t6: Vec<TinyAsciiStr<6>>,
        t7: Vec<TinyAsciiStr<7>>,
        strs: Vec<Box<AsciiStr>>,
    }

    fn sized<const N: usize>(slice: &[u8]) -> Result<&[u8; N], std::array::TryFromSliceError> {
        <&[u8; N]>::try_from(slice)
    }

    unsafe fn sized_exact<const N: usize>(slice: &[u8]) -> &[u8; N] {
        unsafe { <&[u8; N]>::try_from(slice).unwrap_unchecked() }
    }

    fn search<'a, const N: usize>(slice: &'a [TinyAsciiStr<N>], target: &str) -> Option<&'a [u8]> {
        if let Some(x) = slice.iter().find(|&x| x == target) {
            Some(x.as_bytes())
        } else {
            None
        }
    }

    impl Default for ValueStoreInner {
        fn default() -> Self {
            Self::c_default()
        }
    }
    // boxed slice
    impl ValueStoreInner {
        const fn c_default() -> Self {
            Self {
                t1: Vec::new(),
                t2: Vec::new(),
                t3: Vec::new(),
                t4: Vec::new(),
                t5: Vec::new(),
                t6: Vec::new(),
                t7: Vec::new(),
                strs: Vec::new(),
            }
        }

        fn find(&self, str: &AsciiStr) -> Option<&[u8]> {
            let s = str.as_str();
            match str.len() {
                1 => search(&self.t1, s),
                2 => search(&self.t2, s),
                3 => search(&self.t3, s),
                4 => search(&self.t4, s),
                5 => search(&self.t5, s),
                6 => search(&self.t6, s),
                7 => search(&self.t7, s),
                _ => self
                    .strs
                    .iter()
                    .find(|&x| x.as_ref() == s)
                    .map(|x| x.as_ref().as_bytes()),
            }
        }

        fn insert(&mut self, chars: &AsciiStr) -> &[u8] {
            unsafe {
                match chars.as_bytes() {
                    [a] => {
                        self.t1.push(Self::new_tiny(&[a]));
                        Self::last(&self.t1)
                    }
                    [a, b] => {
                        self.t2.push(Self::new_tiny(&[a, b]));
                        Self::last(&self.t2)
                    }
                    [a, b, c] => {
                        self.t3.push(Self::new_tiny(&[a, b, c]));
                        Self::last(&self.t3)
                    }
                    [a, b, c, d] => {
                        self.t4.push(Self::new_tiny(&[a, b, c, d]));
                        Self::last(&self.t4)
                    }
                    [a, b, c, d, e] => {
                        self.t5.push(Self::new_tiny(&[a, b, c, d, e]));
                        Self::last(&self.t4)
                    }
                    [a, b, c, d, e, f] => {
                        self.t6.push(Self::new_tiny(&[a, b, c, d, e, f]));
                        Self::last(&self.t5)
                    }
                    [a, b, c, d, e, f, g] => {
                        self.t7.push(Self::new_tiny(&[a, b, c, d, e, f, g]));
                        Self::last(&self.t5)
                    }

                    other => {
                        let string: AsciiString = AsciiString::from_ascii_unchecked(other);
                        //AsciiString::new
                        self.strs.push(string.into_boxed_ascii_str());
                        Self::last_box(self.strs.last())
                    }
                }
            }
        }
        fn new_tiny<const N: usize>(slice: &[&u8; N]) -> TinyAsciiStr<N> {
            unsafe { TinyAsciiStr::from_utf8_unchecked(slice.map(|x| *x)) }
        }

        fn last<const N: usize>(vec: &Vec<TinyAsciiStr<N>>) -> &[u8] {
            vec.last().unwrap().as_bytes()
        }

        unsafe fn last_box(last: Option<&Box<impl AsRef<[u8]> + ?Sized>>) -> &[u8] {
            last.unwrap().as_ref().as_ref()
        }
    }
}

*/
