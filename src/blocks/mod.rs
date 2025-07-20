use crate::backing::*;
use bytemuck::{AnyBitPattern, NoUninit, Pod, PodInOption, Zeroable, ZeroableInOption};
use std::fmt::{self, Debug, Display, Formatter};
use std::io;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::ops::{Deref, DerefMut};

pub mod buddy;
pub mod free;

pub const MAGIC_BYTES: &[u8; 5] = b"\x80vial"; // start with an invalid UTF-8 sequence to try to avoid false positives on text files
pub const BLOCK_SIZE: usize = 1024;
pub const SUPERBLOCK_SIZE_BYTES: usize = BLOCK_SIZE * 64;
pub const VERSION: u8 = 1;

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

#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct FileHeaderInner {
    pub magic_bytes: [u8; 5],
    pub version: u8,
    pub _padding: [u8; 2],
    pub first_free_block: Option<BlockId>,
    pub first_partial_buddy_block: Option<BlockId>,
}
impl FileHeaderInner {
    pub const INIT: Self = Self {
        magic_bytes: *MAGIC_BYTES,
        version: VERSION,
        _padding: [0; 2],
        first_free_block: Some(BlockId(NonZeroU32::new(1).unwrap())),
        first_partial_buddy_block: None,
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct FileHeader {
    pub inner: FileHeaderInner,
    pub block_usage: [BlockMarker; 64],
}
impl FileHeader {
    pub const INIT: Self = Self {
        inner: FileHeaderInner::INIT,
        block_usage: empty_block_use(),
    };
}
impl Deref for FileHeader {
    type Target = FileHeaderInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl DerefMut for FileHeader {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Pod, Zeroable)]
#[repr(transparent)]
pub struct SuperblockType(pub u8);
impl SuperblockType {
    pub const NORMAL: Self = Self(255);
    pub const fn is_normal(&self) -> bool {
        self.0 == 255
    }
}

const fn empty_block_use() -> [BlockMarker; 64] {
    let mut out = [BlockMarker::UNINIT; 64];
    out[0] = BlockMarker::RESERVED;
    out
}

#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct SuperblockHeader {
    pub ty: SuperblockType,
    pub _padding: [u8; size_of::<FileHeaderInner>() - 1],
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
    pub fn in_self(&self, offset: u32) -> io::Result<NonZeroU32> {
        NonZeroU32::new((self.0.get() * BLOCK_SIZE as u32) | offset).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::FileTooLarge,
                "Excessively large file (more than 2^32 bytes) can't be indexed",
            )
        })
    }
    #[inline(always)]
    pub fn make_id<T>(&self, offset: u32) -> io::Result<Id<T>> {
        self.in_self(offset).map(Id::new)
    }
    fn block_type_index(&self) -> io::Result<(usize, usize)> {
        let i = self.0.get() as usize;
        let block_start = (i & !0x3f) * BLOCK_SIZE;
        let offset = (i >> 6).checked_sub(1).ok_or_else(|| {
            io::Error::other(format!(
                "Block ID {i} points inside a file or superblock header"
            ))
        })?;
        Ok((block_start, size_of::<FileHeaderInner>() + offset))
    }
    /// Get the block type if this block is in a normal superblock, or `None` if it's in a special-use one.
    #[inline(always)]
    pub fn block_type(&self, mem: impl ByteAccess) -> io::Result<Option<BlockMarker>> {
        fn inner(this: BlockId, mem: &[u8]) -> io::Result<Option<BlockMarker>> {
            let (super_start, offset) = this.block_type_index()?;
            let v = mem.get(super_start + offset).ok_or_else(|| {
                io::Error::other(format!(
                    "Block ID {} is out of range for a file of {} bytes",
                    this,
                    mem.len(),
                ))
            })?;
            // already range checked
            let block_kind = unsafe { SuperblockType(*mem.get_unchecked(super_start)) };
            Ok(block_kind.is_normal().then_some(BlockMarker(*v)))
        }
        inner(*self, mem.bytes())
    }
    /// Like [`Self::block_type`], but returns a mutable reference to the block marker.
    #[inline(always)]
    pub fn block_type_mut<'a>(
        &self,
        mem: &'a mut (impl ByteAccessMut + ?Sized),
    ) -> io::Result<Option<&'a mut BlockMarker>> {
        fn inner(this: BlockId, mem: &mut [u8]) -> io::Result<Option<&mut BlockMarker>> {
            let (super_start, offset) = this.block_type_index()?;
            let len = mem.len();
            let mp = mem as *mut [u8] as *mut u8;
            let v = mem.get_mut(super_start + offset).ok_or_else(|| {
                io::Error::other(format!(
                    "Block ID {this} is out of range for a file of {len} bytes"
                ))
            })?;
            // already range checked
            let block_kind = unsafe { SuperblockType(*mp.add(super_start)) };
            Ok(block_kind.is_normal().then_some(bytemuck::cast_mut(v)))
        }
        inner(*self, mem.bytes_mut())
    }
    /// Load the block data and block marker for this block.
    #[inline(always)]
    pub fn block_type_and_data<'a>(
        &self,
        mem: &'a mut (impl ByteAccessMut + ?Sized),
    ) -> io::Result<(Option<&'a mut BlockMarker>, &'a mut [u8; BLOCK_SIZE])> {
        fn inner(
            this: BlockId,
            mem: &mut [u8],
        ) -> io::Result<(Option<&mut BlockMarker>, &mut [u8; BLOCK_SIZE])> {
            if this.0.get() >> 6 == 0 {
                return Err(io::Error::other("BlockId points to a superblock header"));
            }
            let (super_start, offset) = this.block_type_index()?;
            let len = mem.len();
            let mp = mem as *mut [u8] as *mut u8;
            let start = this.0.get() as usize * BLOCK_SIZE;
            if start + BLOCK_SIZE >= len {
                return Err(io::Error::other(format!(
                    "Block id {this} out of range for storage of {len} bytes",
                )));
            }
            unsafe {
                let v = &mut *mp.add(super_start).add(offset);
                let block_kind = SuperblockType(*mp.add(super_start));
                let data = mp.add(start).cast();
                Ok((
                    block_kind.is_normal().then_some(bytemuck::cast_mut(v)),
                    &mut *data,
                ))
            }
        }
        inner(*self, mem.bytes_mut())
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
    pub const fn new(index: NonZeroU32) -> Self {
        Self {
            index,
            _marker: PhantomData,
        }
    }
    /// Change the type of the ID.
    pub const fn transmute<U>(self) -> Id<U> {
        Id::new(self.index)
    }
    /// Validate an ID to make sure it points to a valid value.
    ///
    /// This checks alignment, that it points to a nonzero block, that it's in range, and that the value it points to isn't in a (recognized) block's header.
    pub fn validate(&self, mem: impl BackingStore) -> io::Result<()> {
        fn inner(
            index: NonZeroU32,
            align: usize,
            name: &'static str,
            mem: &[u8],
        ) -> io::Result<()> {
            if !index.get().is_multiple_of(align as u32) {
                return Err(io::Error::other(format!(
                    "ID {index} is not aligned to {name}'s required {align}-byte alignment"
                )));
            }
            let this = Id::<()>::new(index);
            let blk = this.block()?;
            let Some(marker) = blk.block_type(mem)? else {
                return Ok(());
            };
            match marker {
                BlockMarker::FREE => {
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
            mem.bytes(),
        )
    }
    /// Get the owning block for this ID. There should never be IDs pointing to block 0, but that's a valid layout, so this method is fallible.
    pub fn block(&self) -> io::Result<BlockId> {
        let blk = self.index.get() / BLOCK_SIZE as u32;
        NonZeroU32::new(blk)
            .map(BlockId)
            .ok_or_else(|| io::Error::other(format!("ID {} belongs to block 0", self.index)))
    }
    pub fn offset(&self) -> u32 {
        self.index.get() % BLOCK_SIZE as u32
    }
}

/// Load the header of a file, or create one if the store is empty.
///
/// This returns errors if the operation fails or the header is invalid.
pub fn file_header(store: &mut impl BackingStore) -> io::Result<&mut FileHeader> {
    if store.bytes_mut().is_empty() {
        store.resize(SUPERBLOCK_SIZE_BYTES)?;
        let header = bytemuck::from_bytes_mut(&mut store.bytes_mut()[..size_of::<FileHeader>()]);
        *header = FileHeader::INIT;
        return Ok(header);
    }
    let header: &mut FileHeader = bytemuck::from_bytes_mut(
        store
            .bytes_mut()
            .get_mut(..size_of::<FileHeader>())
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    "Input data is too small for a file header",
                )
            })?,
    );
    if header.magic_bytes != *MAGIC_BYTES {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "Magic bytes don't match",
        ));
    }
    if header.version > VERSION {
        return Err(io::Error::other(format!(
            "File version is {}, but this build of Vial only expects up to {}",
            header.version, VERSION
        )));
    }
    Ok(header)
}

pub fn block_header<T: NoUninit + AnyBitPattern>(data: &mut [u8]) -> (&mut T, &mut [u8]) {
    let (head, tail) = data.split_at_mut(size_of::<T>());
    (bytemuck::from_bytes_mut(head), tail)
}

/// Load the data in a block.
///
/// This can only fail if the block ID is out of range.
pub fn load_block(
    store: &mut (impl ByteAccessMut + ?Sized),
    id: BlockId,
) -> io::Result<&mut [u8; BLOCK_SIZE]> {
    let start = id.0.get() as usize * BLOCK_SIZE;
    let end = start + BLOCK_SIZE;
    let len = store.bytes().len();
    store
        .bytes_mut()
        .get_mut(start..end)
        .map(bytemuck::from_bytes_mut)
        .ok_or_else(|| {
            io::Error::other(format!(
                "Block id {id} out of range for storage of {len} bytes",
            ))
        })
}
