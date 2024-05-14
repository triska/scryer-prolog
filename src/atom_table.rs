use crate::parser::ast::MAX_ARITY;
use crate::raw_block::*;
use crate::rcu::{Rcu, RcuRef};
use crate::types::*;

use std::cmp::Ordering;
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::Deref;
use std::ptr;
use std::slice;
use std::str;
use std::sync::Arc;
use std::sync::Mutex;
use std::sync::RwLock;
use std::sync::Weak;

use fxhash::FxBuildHasher;
use indexmap::IndexSet;

use scryer_modular_bitfield::prelude::*;

#[bitfield]
#[repr(u64)]
#[derive(Copy, Clone, Debug)]
pub struct AtomCell {
    name: B48,
    arity: B8,
    #[allow(unused)]
    f: bool,
    #[allow(unused)]
    m: bool,
    #[allow(unused)]
    is_inlined: bool,
    #[allow(unused)]
    tag: B5,
}

/*
impl PartialEq<Atom> for Atom {
    fn eq(&self, other: &Self) -> bool {
        let l = self.with_f(false).with_m(false);
        let r = other.with_f(false).with_m(false);

        l.as_u64() == r.as_u64()
    }
}
*/

const INLINED_ATOM_MAX_LEN: usize = 6;

const_assert!(INLINED_ATOM_MAX_LEN < mem::size_of::<AtomCell>());
const_assert!(mem::size_of::<AtomCell>() == 8);

const_assert!(INLINED_ATOM_MAX_LEN < mem::size_of::<Atom>());
const_assert!(mem::size_of::<Atom>() == 8);

impl AtomCell {
    #[inline]
    pub fn new_static(index: u64) -> Self {
        // upper 23 bits of index must be 0
        debug_assert!(index & !((1 << 49) - 1) == 0);
        AtomCell::new()
            .with_name(index)
            .with_arity(0u8)
            .with_m(false)
            .with_f(false)
            .with_is_inlined(false)
            .with_tag(HeapCellValueTag::Atom as u8)
    }

    #[inline]
    pub fn new_inlined(string: &str, arity: u8) -> Self {
        debug_assert!(string.len() <= INLINED_ATOM_MAX_LEN);

        let mut string_buf: [u8; 8] = [0u8; 8];
        string_buf[.. string.len()].copy_from_slice(string.as_bytes());
        let encoding = u64::from_le_bytes(string_buf);

        AtomCell::new()
            .with_name(encoding)
            .with_arity(arity)
            .with_m(false)
            .with_f(false)
            .with_is_inlined(true)
            .with_tag(HeapCellValueTag::Atom as u8)
    }

    #[inline]
    pub fn new_char_inlined(c: char) -> Self {
        let mut char_buf = [0u8;8];
        c.encode_utf8(&mut char_buf);

        let encoding = u64::from_le_bytes(char_buf);

        AtomCell::new()
            .with_name(encoding)
            .with_arity(0u8)
            .with_m(false)
            .with_f(false)
            .with_is_inlined(true)
            .with_tag(HeapCellValueTag::Atom as u8)
    }

    #[inline]
    pub fn build_with(atom_index: u64, arity: u8) -> Self {
        debug_assert!((arity as usize) <= MAX_ARITY);

        AtomCell::new()
            .with_name(atom_index >> 1)
            .with_arity(arity)
            .with_f(false)
            .with_m(false)
            .with_is_inlined(atom_index & 1 == 1)
            .with_tag(HeapCellValueTag::Atom as u8)
    }

    #[inline]
    pub fn get_name(self) -> Atom {
        Atom { index: (self.name() << 1) | self.is_inlined() as u64 }
    }

    #[inline]
    pub fn get_arity(self) -> usize {
        self.arity() as usize
    }

    #[inline]
    pub fn get_name_and_arity(self) -> (Atom, usize) {
        (self.get_name(), self.get_arity())
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Atom {
    pub index: u64,
}

include!(concat!(env!("OUT_DIR"), "/static_atoms.rs"));

// populate these in STRINGS so they can be used from build_functor
const _: Atom = atom!(".");
const _: Atom = atom!("[]");

impl<'a> From<&'a Atom> for Atom {
    #[inline]
    fn from(atom: &'a Atom) -> Self {
        *atom
    }
}

/*
impl From<bool> for Atom {
    #[inline]
    fn from(value: bool) -> Self {
        if value {
            atom!("true")
        } else {
            atom!("false")
        }
    }
}
*/

impl indexmap::Equivalent<Atom> for str {
    fn equivalent(&self, key: &Atom) -> bool {
        &*key.as_str() == self
    }
}

const ATOM_TABLE_INIT_SIZE: usize = 1 << 16;
const ATOM_TABLE_ALIGN: usize = 8;

#[inline(always)]
fn global_atom_table() -> &'static RwLock<Weak<AtomTable>> {
    #[cfg(feature = "rust_beta_channel")]
    {
        // const Weak::new will be stabilized in 1.73 which is currently in beta,
        // till then we need a OnceLock for initialization
        static GLOBAL_ATOM_TABLE: RwLock<Weak<AtomTable>> = RwLock::const_new(Weak::new());
        &GLOBAL_ATOM_TABLE
    }
    #[cfg(not(feature = "rust_beta_channel"))]
    {
        use std::sync::OnceLock;
        static GLOBAL_ATOM_TABLE: OnceLock<RwLock<Weak<AtomTable>>> = OnceLock::new();
        GLOBAL_ATOM_TABLE.get_or_init(|| RwLock::new(Weak::new()))
    }
}

#[inline(always)]
fn arc_atom_table() -> Option<Arc<AtomTable>> {
    global_atom_table().read().unwrap().upgrade()
}

impl RawBlockTraits for AtomTable {
    #[inline]
    fn init_size() -> usize {
        ATOM_TABLE_INIT_SIZE
    }

    #[inline]
    fn align() -> usize {
        ATOM_TABLE_ALIGN
    }
}

#[bitfield]
#[derive(Copy, Clone, Debug)]
struct AtomHeader {
    #[allow(unused)]
    m: bool,
    len: B50,
    #[allow(unused)]
    padding: B13,
}

impl AtomHeader {
    fn build_with(len: u64) -> Self {
        AtomHeader::new().with_len(len).with_m(false)
    }
}

impl Hash for Atom {
    #[inline]
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.as_str().hash(hasher)
        // hasher.write_usize(self.index)
    }
}

#[macro_export]
macro_rules! is_char {
    ($s:expr) => {
        !$s.is_empty() && $s.chars().nth(1).is_none()
    };
}

pub enum AtomString<'a> {
    Static(&'a str),
    Inlined([u8;8]),
    Dynamic(AtomTableRef<str>),
}

fn inlined_to_str<'a>(bytes: &'a [u8;8]) -> &'a str {
    let slice_len = if bytes[0] == 0 {
        1
    } else {
        bytes.iter().position(|&b| b == 0u8).unwrap_or(INLINED_ATOM_MAX_LEN)
    };

    unsafe {
        str::from_utf8_unchecked(&bytes[..slice_len])
    }
}

/*
impl AtomString<'_> {
    pub fn map<F>(self, f: F) -> Self
    where
        for<'a> F: FnOnce(&'a str) -> &'a str,
    {
        match self {
            Self::Static(reference) => Self::Static(f(reference)),
            Self::Dynamic(guard) => Self::Dynamic(AtomTableRef::map(guard, f)),
        }
    }
}
*/

impl std::fmt::Debug for AtomString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self.deref(), f)
    }
}

impl std::fmt::Display for AtomString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Display::fmt(self.deref(), f)
    }
}

impl std::ops::Deref for AtomString<'_> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Static(reference) => reference,
            Self::Inlined(inlined) => inlined_to_str(&inlined),
            Self::Dynamic(guard) => guard.deref(),
        }
    }
}

#[cfg(feature = "repl")]
impl rustyline::completion::Candidate for AtomString<'_> {
    fn display(&self) -> &str {
        self.deref()
    }

    fn replacement(&self) -> &str {
        self.deref()
    }
}

impl Atom {
    #[inline]
    fn new_uninlined(idx: u64) -> Self {
        Atom { index: idx << 1 }
    }

    #[inline]
    fn new_inlined(string: &str) -> Self {
        AtomCell::new_inlined(string, 0).get_name()
    }

    #[inline(always)]
    fn is_static(self) -> bool {
        if self.is_inlined() {
            true
        } else {
            (self.flat_index() as usize) < STRINGS.len()
        }
    }

    #[inline]
    pub(crate) fn flat_index(self) -> u64 {
        self.index >> 1
    }

    #[inline(always)]
    pub(crate) fn is_inlined(self) -> bool {
        self.index & 1 == 1
    }

    /*
    #[inline]
    pub(crate) fn as_cell(self) -> AtomCell {
        AtomCell::new()
            .with_name(self.flat_index())
            .with_arity(0u16)
            .with_f(false)
            .with_m(false)
            .with_is_inlined(self.is_inlined())
            .with_tag(HeapCellValueTag::Atom as u8)
    }
    */

    #[inline(always)]
    fn as_ptr(self) -> Option<AtomTableRef<u8>> {
        if self.is_static() {
            None
        } else {
            let atom_table =
                arc_atom_table().expect("We should only have an Atom while there is an AtomTable");
            unsafe {
                AtomTableRef::try_map(atom_table.buf(), |buf| {
                    (buf as *const u8)
                        .add(self.flat_index() as usize - STRINGS.len())
                        .as_ref()
                })
            }
        }
    }

    #[inline(always)]
    pub fn from(index: u64) -> Self {
        Self { index }
    }

    #[inline(always)]
    pub fn len(self) -> usize {
        if let Some(s) = self.inlined_str() {
            s.len()
        } else if self.is_static() {
            let index = self.flat_index();
            STRINGS[index as usize].len()
        } else {
            let ptr = self.as_ptr().unwrap();
            let ptr = ptr.deref() as *const u8 as *const AtomHeader;
            unsafe { ptr::read(ptr) }.len() as _
        }
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }

    pub fn as_char(self) -> Option<char> {
        let s = self.as_str();
        let mut it = s.chars();

        let c1 = it.next();
        let c2 = it.next();

        if c2.is_none() {
            c1
        } else {
            None
        }
    }

    #[inline]
    fn inlined_str<'a>(&self) -> Option<AtomString<'a>> {
        if self.is_inlined() {
            Some(AtomString::Inlined(self.flat_index().to_le_bytes()))
        } else {
            None
        }
    }

    #[inline]
    pub fn as_str(&self) -> AtomString<'static> {
        if let Some(s) = self.inlined_str() {
            s
        } else if self.is_static() {
            let index = self.flat_index() as usize;
            AtomString::Static(STRINGS[index])
        } else if let Some(ptr) = self.as_ptr() {
            AtomString::Dynamic(AtomTableRef::map(ptr, |ptr| {
                let header =
                    // Miri seems to hit this line a lot
                    unsafe { ptr::read::<AtomHeader>(ptr as *const u8 as *const AtomHeader) };
                let len = header.len() as usize;
                let buf = unsafe { (ptr as *const u8).add(mem::size_of::<AtomHeader>()) };

                unsafe { str::from_utf8_unchecked(slice::from_raw_parts(buf, len)) }
            }))
        } else {
            AtomString::Static(STRINGS[(self.index >> 1) as usize])
        }
    }

    pub fn defrock_brackets(&self, atom_tbl: &AtomTable) -> Self {
        let s = self.as_str();

        let sub_str = if s.starts_with('(') && s.ends_with(')') {
            &s['('.len_utf8()..s.len() - ')'.len_utf8()]
        } else {
            return *self;
        };

        AtomTable::build_with(atom_tbl, sub_str)
    }
}

unsafe fn write_to_ptr(string: &str, ptr: *mut u8) {
    ptr::write(ptr as *mut _, AtomHeader::build_with(string.len() as u64));
    let str_ptr = ptr.add(mem::size_of::<AtomHeader>());
    ptr::copy_nonoverlapping(string.as_ptr(), str_ptr, string.len());
}

impl PartialOrd for Atom {
    #[inline]
    fn partial_cmp(&self, other: &Atom) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Atom {
    #[inline]
    fn cmp(&self, other: &Atom) -> Ordering {
        self.as_str().cmp(&*other.as_str())
    }
}

#[derive(Debug)]
pub struct InnerAtomTable {
    block: RawBlock<AtomTable>,
    pub table: Rcu<IndexSet<Atom, FxBuildHasher>>,
}

#[derive(Debug)]
pub struct AtomTable {
    inner: Rcu<InnerAtomTable>,
    // this lock is taking during resizing
    update: Mutex<()>,
}

pub type AtomTableRef<M> = RcuRef<InnerAtomTable, M>;

fn populate_static_strings() -> IndexSet<Atom, FxBuildHasher> {
    let mut table = IndexSet::with_hasher(FxBuildHasher::default());

    for idx in 0 .. STRINGS.len() {
        table.insert(Atom::new_uninlined(idx as u64));
    }

    table
}

impl InnerAtomTable {
    fn new() -> Self {
        Self {
            block: RawBlock::new(),
            table: Rcu::new(populate_static_strings()),
        }
    }

    #[inline(always)]
    fn lookup_str(self: &InnerAtomTable, string: &str) -> Option<Atom> {
        STATIC_ATOMS_MAP
            .get(string)
            .cloned()
            .or_else(|| self.table.active_epoch().get(string).cloned())
    }
}

impl AtomTable {
    #[inline]
    pub fn new() -> Arc<Self> {
        let upgraded = global_atom_table().read().unwrap().upgrade();
        // don't inline upgraded, otherwise temporary will be dropped too late in case of None
        if let Some(atom_table) = upgraded {
            atom_table
        } else {
            let mut guard = global_atom_table().write().unwrap();
            // try to upgrade again in case we lost the race on the write lock
            if let Some(atom_table) = guard.upgrade() {
                atom_table
            } else {
                let atom_table = Arc::new(Self {
                    inner: Rcu::new(InnerAtomTable::new()),
                    update: Mutex::new(()),
                });
                *guard = Arc::downgrade(&atom_table);
                atom_table
            }
        }
    }

    #[inline]
    pub fn buf(&self) -> AtomTableRef<u8> {
        AtomTableRef::<InnerAtomTable>::map(self.inner.active_epoch(), |inner| {
            unsafe { inner.block.base.as_ref() }.unwrap()
        })
    }

    pub fn active_table(&self) -> RcuRef<IndexSet<Atom, FxBuildHasher>, IndexSet<Atom, FxBuildHasher>> {
        self.inner.active_epoch().table.active_epoch()
    }

    pub fn build_with(atom_table: &AtomTable, string: &str) -> Atom {
        if 0 < string.len() && string.len() <= INLINED_ATOM_MAX_LEN {
            return Atom::new_inlined(string);
        }

        loop {
            let mut block_epoch = atom_table.inner.active_epoch();
            let mut table_epoch = block_epoch.table.active_epoch();

            if let Some(atom) = block_epoch.lookup_str(string) {
                return atom;
            }

            // take a lock to prevent concurrent updates
            let update_guard = atom_table.update.lock().unwrap();

            let is_same_allocation =
                RcuRef::same_epoch(&block_epoch, &atom_table.inner.active_epoch());
            let is_same_atom_list =
                RcuRef::same_epoch(&table_epoch, &block_epoch.table.active_epoch());

            if !(is_same_allocation && is_same_atom_list) {
                // some other thread raced us between our lookup and
                // us aquring the update lock,
                // try again
                continue;
            }

            let size = mem::size_of::<AtomHeader>() + string.len();
            let align_offset = 8 * mem::align_of::<AtomHeader>();
            let size = (size & !(align_offset - 1)) + align_offset;

            unsafe {
                let len_ptr = loop {
                    let ptr = block_epoch.block.alloc(size);

                    if ptr.is_null() {
                        // garbage collection would go here
                        let new_block = block_epoch.block.grow_new().unwrap();
                        let new_table = Rcu::new(table_epoch.clone());
                        let new_alloc = InnerAtomTable {
                            block: new_block,
                            table: new_table,
                        };
                        atom_table.inner.replace(new_alloc);
                        block_epoch = atom_table.inner.active_epoch();
                        table_epoch = block_epoch.table.active_epoch();
                    } else {
                        break ptr;
                    }
                };

                let ptr_base = block_epoch.block.base as usize;

                write_to_ptr(string, len_ptr);

                let atom = AtomCell::new()
                    .with_name((STRINGS.len() + len_ptr as usize - ptr_base) as u64)
                    .with_arity(0)
                    .with_f(false)
                    .with_m(false)
                    .with_is_inlined(false)
                    .with_tag(HeapCellValueTag::Atom as u8)
                    .get_name();

                let mut table = table_epoch.clone();
                table.insert(atom);
                block_epoch.table.replace(table);

                // expicit drop to ensure we don't accidentally drop it early
                drop(update_guard);

                return atom;
            }
        }
    }
}

unsafe impl Send for AtomTable {}
unsafe impl Sync for AtomTable {}

/*
#[bitfield]
#[repr(u64)]
#[derive(Copy, Clone, Debug)]
pub struct AtomCell {
    name: B48,
    arity: B10,
    #[allow(unused)]
    f: bool,
    #[allow(unused)]
    m: bool,
    #[allow(unused)]
    inlined: bool,
    #[allow(unused)]
    tag: B3,
}

impl AtomCell {
    #[inline]
    pub fn build_with(name: u64, arity: u16, tag: HeapCellValueTag) -> Self {
        if arity > 0 {
            debug_assert!(arity as usize <= MAX_ARITY);

            AtomCell::new()
                .with_name(name)
                .with_arity(arity)
                .with_f(false)
                .with_tag(tag as u8)
        } else {
            AtomCell::new()
                .with_name(name)
                .with_f(false)
                .with_tag(tag as u8)
        }
    }

    #[inline]
    pub fn get_index(self) -> usize {
        self.name() as usize
    }

    #[inline]
    pub fn get_name(self) -> Atom {
        Atom::from((self.get_index() as u64) << 3)
    }

    #[inline]
    pub fn get_arity(self) -> usize {
        self.arity() as usize
    }

    #[inline]
    pub fn get_name_and_arity(self) -> (Atom, usize) {
        (Atom::from((self.get_index() as u64) << 3), self.get_arity())
    }
}
*/
