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

mod intern {
    use std::mem::MaybeUninit;
    use std::pin::Pin;
    use std::ptr::NonNull;
    // new setup: symtab contains prop, and symbols, keys are a pointer to symbols, symbols contain
    // a pointer to properties, properties uses typestate. Symbols are homogenously allocated in a
    // bump allocated vec of bytes

    type SymEntry = (Box<Attr>, Box<[u8]>);

    struct SymTab {
        ents: Box<[MaybeUninit<SymEntry>]>,
        size:usize
        next: Option<Box<SymTab>>,
    }

    impl SymTab{
        const DEFAULT_SIZE:usize = 256;
        fn new() -> Self{
            let mut vec = Vec::with_capacity(Self::DEFAULT_SIZE);
            for _ in 0..Self::DEFAULT_SIZE{
                vec.push(MaybeUninit::uninit());
            }


            Self{
                ents: vec.into_boxed_slice(),
                size:0,
                next:None,
            }
        }
    }
    /*
        union SymEntry {
            attr: ManuallyDrop<Box<Attr>>,
            byte: u8,
            next: ManuallyDrop<Option<Pin<Box<SymVec>>>>,
        }

        impl SymEntry {
            // unsafe bc you need to remember that it's an attr and drop it
            fn attr() -> Self {
                let attr = ManuallyDrop::new(Box::new(Attr {}));
                Self { attr }
            }

            unsafe fn drop_attr(self) {
                drop(ManuallyDrop::into_inner(unsafe { self.attr }))
            }

            unsafe fn drop_next(self) {
                match ManuallyDrop::into_inner(unsafe { self.next }) {
                    Some(x) => drop(Pin::into_inner(x)),
                    None => (),
                }
            }

            unsafe fn drop_box_ptr<T>(ptr: *mut T) {
                let bx = unsafe { Box::from_raw(ptr) };
                drop(bx)
            }
        }
    */

    struct Key(NonNull<SymEntry>);

    // properties of strings
    struct Attr {}
}

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
