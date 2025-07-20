use crate::backing::*;
use bytemuck::{
    AnyBitPattern, NoUninit, Pod, PodInOption, TransparentWrapper, Zeroable, ZeroableInOption,
};
use std::fmt::{self, Debug, Display, Formatter};
use std::io;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ops::RangeBounds;

pub mod buddy;
pub mod free;
pub mod utils;

pub const MAGIC_BYTES: &[u8; 5] = b"\x80vial"; // start with an invalid UTF-8 sequence to try to avoid false positives on text files
pub const BLOCK_SIZE: usize = 1024;
pub const SUPERBLOCK_SIZE_BYTES: usize = BLOCK_SIZE * 64;
pub const FILE_VERSION: u8 = 1;

/// Marker byte for the type of a block.
///
/// These are stored out of line in superblock headers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable)]
#[repr(transparent)]
pub struct BlockMarker(pub u8);
impl BlockMarker {
    /// An uninitialized block acts like a free block, but instead of an explicit next pointer, it points to the next block within the superblock.
    pub const UNINIT: Self = Self(0);
    /// A free block is a block that was previously allocated but is now unused.
    pub const FREE: Self = Self(1);
    /// A buddy-allocated block is used for small allocations, capable of allocating up to 896 bytes but more efficient at smaller ones.
    pub const BUDDY: Self = Self(2);
    /// A miscellaneous block, returned directly from a call to [`alloc_block`](free::alloc_block).
    pub const MISC_USE: Self = Self(3);
    /// A reserved block, as a marker for the first element of block use arrays.
    pub const RESERVED: Self = Self(255);
    pub const fn slab(marker: u8) -> Self {
        Self(marker | 0b01000000)
    }
}
impl Display for BlockMarker {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::UNINIT => f.write_str("UNINIT"),
            Self::FREE => f.write_str("FREE"),
            Self::BUDDY => f.write_str("BUDDY"),
            Self::MISC_USE => f.write_str("MISC_USE"),
            Self::RESERVED => f.write_str("RESERVED"),
            _ => {
                if self.0 & 0xc0 == 0x40 {
                    write!(f, "SLAB | 0x{:>2x}", self.0 & 0x3f)
                } else {
                    write!(f, "0x{:>2x}", self.0)
                }
            }
        }
    }
}

/// Header information at the start of a file.
#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct FileHeader {
    pub magic_bytes: [u8; 5],
    pub version: u8,
    pub _padding: [u8; 2],
    pub first_free_block: Option<BlockId>,
    pub first_partial_buddy_block: Option<BlockId>,
}
impl FileHeader {
    pub const INIT: Self = Self {
        magic_bytes: *MAGIC_BYTES,
        version: FILE_VERSION,
        _padding: [0; _],
        first_free_block: Some(BlockId(NonZeroU32::new(1).unwrap())),
        first_partial_buddy_block: None,
    };
}

/// Header information for the start of the file, along with the first superblock usage.
#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct FullFileHeader {
    pub inner: FileHeader,
    pub block_usage: [BlockMarker; 64],
}
impl FullFileHeader {
    pub const INIT: Self = Self {
        inner: FileHeader::INIT,
        block_usage: empty_block_use(),
    };
}

/// The marker byte for a superblock.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable)]
#[repr(transparent)]
pub struct SuperblockType(pub u8);
impl SuperblockType {
    /// For a non-special block, the marker is `0x80`, the first magic byte.
    pub const NORMAL: Self = Self(MAGIC_BYTES[0]);
    pub const fn is_normal(self) -> bool {
        self.0 == MAGIC_BYTES[0]
    }
}

const fn empty_block_use() -> [BlockMarker; 64] {
    let mut out = [BlockMarker::UNINIT; 64];
    out[0] = BlockMarker::RESERVED;
    out
}

/// The header of a superblock.
///
/// This has the same first byte and offset of block markers as a [`FullFileHeader`] to make the first block compatible as a superblock.
#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct SuperblockHeader {
    pub ty: SuperblockType,
    pub _padding: [u8; HEADER_SIZE - 1],
    pub block_usage: [BlockMarker; 64],
}
impl SuperblockHeader {
    pub const NORMAL_EMPTY: Self = Self {
        ty: SuperblockType::NORMAL,
        _padding: [0; _],
        block_usage: empty_block_use(),
    };
    pub const fn using_one(marker: BlockMarker) -> Self {
        let mut this = Self::NORMAL_EMPTY;
        this.block_usage[1] = marker;
        this
    }
}

/// A type-safe block ID.
///
/// Block 0 would refer to the block containing the file header, but that should never be referred to by any other blocks,
/// so we can use a non-zero index for additional niching.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, NoUninit)]
#[repr(transparent)]
pub struct BlockId(pub NonZeroU32);
unsafe impl PodInOption for BlockId {}
unsafe impl ZeroableInOption for BlockId {}
impl Display for BlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}
impl BlockId {
    /// Get the index of a position offset in this block.
    pub fn in_self(self, offset: u32) -> io::Result<NonZeroU32> {
        (self.0.get().checked_mul(BLOCK_SIZE as u32))
            .map(|o| o | offset)
            .and_then(NonZeroU32::new)
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::FileTooLarge,
                    "Excessively large file (more than 2^32 bytes) can't be indexed",
                )
            })
    }
    /// Get an [`Id`] for an offset in this block.
    #[inline(always)]
    pub fn make_id<T>(self, offset: u32) -> io::Result<Id<T>> {
        self.in_self(offset).map(Id::new)
    }
}

/// A type-safe index for a value.
pub struct Id<T> {
    pub index: NonZeroU32,
    pub _marker: PhantomData<fn(&mut [u8]) -> &mut T>,
}
impl<T> Debug for Id<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("Id")
            .field("index", &self.index)
            .field("type", &std::any::type_name::<T>())
            .finish()
    }
}
impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Id<T> {}
unsafe impl<T: 'static> NoUninit for Id<T> {}
unsafe impl<T: 'static> PodInOption for Id<T> {}
unsafe impl<T> ZeroableInOption for Id<T> {}
impl<T> Id<T> {
    /// Create a new ID.
    ///
    /// This is entirely transparent, with no validation.
    pub const fn new(index: NonZeroU32) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }
    /// Change the type of the ID.
    ///
    /// This doesn't validate the new alignment.
    pub const fn transmute<U>(self) -> Id<U> {
        Id::new(self.index)
    }
    /// Validate an ID to make sure it points to a valid value.
    ///
    /// This checks alignment, that it points to a nonzero block, that it's in range, and that the value it points to isn't in a (recognized) block's header.
    pub fn validate(&self, mem: &(impl WithoutHeader + ?Sized)) -> io::Result<()> {
        fn inner(
            index: NonZeroU32,
            align: usize,
            name: &'static str,
            mem: &NonHeaderBytes,
        ) -> io::Result<()> {
            if !index.get().is_multiple_of(align as u32) {
                return Err(io::Error::other(format!(
                    "ID {index} is not aligned to {name}'s required {align}-byte alignment"
                )));
            }
            let this = Id::<()>::new(index);
            let blk = this.block()?;
            let Some(marker) = utils::block_type(blk, mem)? else {
                return Ok(());
            };
            match marker {
                BlockMarker::UNINIT | BlockMarker::FREE => {
                    return Err(io::Error::other(format!(
                        "ID {index} points into a free block"
                    )));
                }
                BlockMarker::BUDDY => {
                    if this.offset() < 128 {
                        return Err(io::Error::other(format!(
                            "ID {index} points inside the header of a buddy-allocated block"
                        )));
                    }
                }
                _ => {}
            }
            Ok(())
        }
        inner(
            self.index,
            align_of::<T>(),
            std::any::type_name::<T>(),
            mem.guard(),
        )
    }
    /// Get the owning block for this ID. There should never be IDs pointing to block 0, but that's a valid layout, so this method is fallible.
    pub fn block(&self) -> io::Result<BlockId> {
        let blk = self.index.get() / BLOCK_SIZE as u32;
        NonZeroU32::new(blk)
            .map(BlockId)
            .ok_or_else(|| io::Error::other(format!("ID {} belongs to block 0", self.index)))
    }
    /// Offset of this position within a block.
    pub fn offset(&self) -> u32 {
        self.index.get() % BLOCK_SIZE as u32
    }
}

/// Load the header of a file, or create one if the store is empty.
///
/// This returns errors if the operation fails or the header is invalid.
pub fn check_file_header(store: &mut impl BackingStore) -> io::Result<()> {
    if store.bytes_mut().is_empty() {
        store.resize(SUPERBLOCK_SIZE_BYTES)?;
        let bytes = store.bytes_mut();
        bytes[..size_of::<FullFileHeader>()]
            .copy_from_slice(bytemuck::bytes_of(&FullFileHeader::INIT));
        return Ok(());
    }
    let head = store.bytes_mut().get_mut(..HEADER_SIZE).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Input data is too small for a file header",
        )
    })?;
    let header = bytemuck::from_bytes_mut::<FileHeader>(head);
    if header.magic_bytes != *MAGIC_BYTES {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Magic bytes don't match",
        ));
    }
    if header.version > FILE_VERSION {
        return Err(io::Error::other(format!(
            "File version is {}, but this build of Vial only expects up to {}",
            header.version, FILE_VERSION
        )));
    }
    Ok(())
}

/// Load the file header and remainder of the file.
///
/// This doesn't do any validation on the file, only
pub fn file_header(
    store: &mut (impl ByteAccessMut + ?Sized),
) -> io::Result<(&mut FileHeader, &mut NonHeaderBytes)> {
    let (head, rest) = NonHeaderBytes::split_mut(store.bytes_mut()).ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            "Input data is too small for a file header",
        )
    })?;
    let header = bytemuck::from_bytes_mut::<FileHeader>(head);
    Ok((header, rest))
}

/// Load the block header of a given type, and return the rest of the block.
#[inline(always)]
pub fn block_header<T: NoUninit + AnyBitPattern>(data: &mut [u8]) -> (&mut T, &mut [u8]) {
    let (head, tail) = data.split_at_mut(size_of::<T>());
    (bytemuck::from_bytes_mut(head), tail)
}

/// A trait that allows for a guard against the first `size_of::<FileHeader>()` bytes of the file.
///
/// This is implemented for all [`ByteAccessMut`], along with [`NonHeaderBytes`].
pub trait WithoutHeader {
    /// Get the rest of the file as a slice.
    fn borrow(&self) -> &[u8];
    /// Get the rest of the file as a mutable slice.
    fn borrow_mut(&mut self) -> &mut [u8];
    /// Get a [`NonHeaderBytes`] reference, erasing the type.
    #[inline(always)]
    fn guard(&self) -> &NonHeaderBytes {
        NonHeaderBytes::wrap_ref(self.borrow())
    }
    /// Get a mutable [`NonHeaderBytes`] reference, erasing the type.
    #[inline(always)]
    fn guard_mut(&mut self) -> &mut NonHeaderBytes {
        NonHeaderBytes::wrap_mut(self.borrow_mut())
    }
}

const HEADER_SIZE: usize = size_of::<FileHeader>();

/// An implementation of [`WithoutHeader`] that's already guarded against.
#[derive(TransparentWrapper)]
#[repr(transparent)]
pub struct NonHeaderBytes([u8]);
impl NonHeaderBytes {
    /// Split the first `size_of::<FileHeader>()` bytes off from a slice, returning that and the rest of the file as a guard.
    pub fn split(data: &[u8]) -> Option<(&[u8; HEADER_SIZE], &Self)> {
        data.split_first_chunk()
            .map(|(h, t)| (h, Self::wrap_ref(t)))
    }
    /// Split the first `size_of::<FileHeader>()` bytes off from a mutable slice, returning that and the rest of the file as a guard.
    pub fn split_mut(data: &mut [u8]) -> Option<(&mut [u8; HEADER_SIZE], &mut Self)> {
        data.split_first_chunk_mut()
            .map(|(h, t)| (h, Self::wrap_mut(t)))
    }
}
impl WithoutHeader for NonHeaderBytes {
    #[inline(always)]
    fn borrow(&self) -> &[u8] {
        &self.0
    }
    #[inline(always)]
    fn borrow_mut(&mut self) -> &mut [u8] {
        &mut self.0
    }
}
impl<T: ByteAccessMut + ?Sized> WithoutHeader for T {
    #[inline(always)]
    fn borrow(&self) -> &[u8] {
        self.bytes().get(HEADER_SIZE..).unwrap_or(&[])
    }
    #[inline(always)]
    fn borrow_mut(&mut self) -> &mut [u8] {
        self.bytes_mut().get_mut(HEADER_SIZE..).unwrap_or(&mut [])
    }
}

/// Extension methods for [`WithoutHeader`] types that mimic the API of `[u8]`.
///
/// Out of laziness, the `get*` family of functions have been split into `get_index*` and `get_range*`.
#[allow(clippy::missing_safety_doc)]
pub trait WithoutHeaderExt: WithoutHeader {
    #[inline(always)]
    fn as_ptr(&self) -> *const u8 {
        self.borrow().as_ptr().wrapping_sub(HEADER_SIZE)
    }
    #[inline(always)]
    fn as_mut_ptr(&mut self) -> *mut u8 {
        self.borrow_mut().as_mut_ptr().wrapping_sub(HEADER_SIZE)
    }
    #[inline(always)]
    fn len(&self) -> usize {
        self.borrow().len() + HEADER_SIZE
    }
    #[inline(always)]
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    #[inline(always)]
    fn get_index(&self, index: usize) -> Option<&u8> {
        self.borrow().get(index + HEADER_SIZE)
    }
    #[inline(always)]
    fn get_range(&self, range: impl RangeBounds<usize>) -> Option<&[u8]> {
        let start = range.start_bound();
        let end = range.end_bound();
        self.borrow().get((
            start.map(|b| *b + HEADER_SIZE),
            end.map(|b| *b + HEADER_SIZE),
        ))
    }
    #[inline(always)]
    unsafe fn get_index_unchecked(&self, index: usize) -> &u8 {
        unsafe { self.borrow().get_unchecked(index + HEADER_SIZE) }
    }
    #[inline(always)]
    unsafe fn get_range_unchecked(&self, range: impl RangeBounds<usize>) -> &[u8] {
        let start = range.start_bound();
        let end = range.end_bound();
        unsafe {
            self.borrow().get_unchecked((
                start.map(|b| *b + HEADER_SIZE),
                end.map(|b| *b + HEADER_SIZE),
            ))
        }
    }
    #[inline(always)]
    fn get_index_mut(&mut self, index: usize) -> Option<&mut u8> {
        self.borrow_mut().get_mut(index + HEADER_SIZE)
    }
    #[inline(always)]
    fn get_range_mut(&mut self, range: impl RangeBounds<usize>) -> Option<&mut [u8]> {
        let start = range.start_bound();
        let end = range.end_bound();
        self.borrow_mut().get_mut((
            start.map(|b| *b + HEADER_SIZE),
            end.map(|b| *b + HEADER_SIZE),
        ))
    }
    #[inline(always)]
    unsafe fn get_index_unchecked_mut(&mut self, index: usize) -> &mut u8 {
        unsafe { self.borrow_mut().get_unchecked_mut(index + HEADER_SIZE) }
    }
    #[inline(always)]
    unsafe fn get_range_unchecked_mut(&mut self, range: impl RangeBounds<usize>) -> &mut [u8] {
        let start = range.start_bound();
        let end = range.end_bound();
        unsafe {
            self.borrow_mut().get_unchecked_mut((
                start.map(|b| *b + HEADER_SIZE),
                end.map(|b| *b + HEADER_SIZE),
            ))
        }
    }
}
impl<T: WithoutHeader + ?Sized> WithoutHeaderExt for T {}
