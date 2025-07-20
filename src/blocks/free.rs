//! Free-block operations.
//!
//! This handles the allocation and deallocation of blocks.

use super::*;

/// Header information for a free block.
#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct FreeBlock {
    pub next_free: Option<BlockId>,
}

/// Allocate a block, reusing one from the free list if possible.
///
/// The contents of this block's memory could have anything in them,
pub fn alloc_block(
    store: &mut impl BackingStore,
    new_marker: BlockMarker,
) -> io::Result<(BlockId, &mut [u8; BLOCK_SIZE])> {
    let len = store.bytes().len();
    let (header, rest) = file_header(store)?;
    if let Some(block_id) = header.first_free_block {
        let (marker, block) = utils::block_type_and_data(block_id, rest)?;
        let marker =
            marker.ok_or_else(|| io::Error::other("Free block is in a special-use superblock!"))?;
        match *marker {
            BlockMarker::FREE => {
                let (block_header, _) = block_header::<FreeBlock>(block);
                let next = block_header.next_free;
                header.first_free_block = next;
            }
            BlockMarker::UNINIT => {
                let Some(next) = block_id.0.checked_add(1) else {
                    return Err(io::Error::new(
                        io::ErrorKind::FileTooLarge,
                        "Attempted to allocate more than 2^32 blocks",
                    ));
                };
                header.first_free_block = (next.get() & 0x3f != 0).then_some(BlockId(next));
            }
            _ => {
                return Err(io::Error::other(format!(
                    "Unexpected block marker {marker}"
                )));
            }
        };
        *marker = new_marker;
        Ok((block_id, unsafe { &mut *(block as *mut [u8; BLOCK_SIZE]) })) // I want polonius already
    } else {
        if !len.is_multiple_of(SUPERBLOCK_SIZE_BYTES) {
            return Err(io::Error::other(format!(
                "File length {len} isn't aligned to the superblock size {SUPERBLOCK_SIZE_BYTES}"
            )));
        }
        let new_len = len + SUPERBLOCK_SIZE_BYTES;
        let block_id = BlockId(unsafe {
            NonZeroU32::new_unchecked((new_len / BLOCK_SIZE + 1).try_into().map_err(|_| {
                io::Error::new(
                    io::ErrorKind::FileTooLarge,
                    "Attempted to allocate more than 2^32 blocks",
                )
            })?)
        });
        store.resize(new_len)?;
        let mem = store.bytes_mut();
        unsafe {
            std::ptr::copy_nonoverlapping(
                &SuperblockHeader::using_one(new_marker) as *const _ as *const u8,
                mem.as_mut_ptr().add(len),
                size_of::<SuperblockHeader>(),
            );
            (*(mem.as_mut_ptr() as *mut FileHeader)).first_free_block =
                Some(BlockId(NonZeroU32::new_unchecked(block_id.0.get() + 1)));
        }
        let block = utils::block_data_mut(block_id, store)?;
        Ok((block_id, block))
    }
}

/// Free a block that's no longer in use.
///
/// This will add it to a free list so it can be reused. Blocks are reused in a LIFO order. This doesn't do
/// anything concerning blocks pointing to this block.
pub fn free_block(store: &mut impl BackingStore, id: BlockId) -> io::Result<()> {
    let (header, rest) = file_header(store)?;
    let next = header.first_free_block.replace(id);
    let (marker, block) = utils::block_type_and_data(id, rest)?;
    if let Some(marker) = marker {
        if next.is_some_and(|i| i.0.get() - 1 == id.0.get()) {
            *marker = BlockMarker::UNINIT;
        } else {
            *marker = BlockMarker::FREE;
            block[..size_of::<FreeBlock>()]
                .copy_from_slice(bytemuck::bytes_of(&FreeBlock { next_free: next }));
        }
        Ok(())
    } else {
        Err(io::Error::other(
            "Attempted to free a block in a special superblock",
        ))
    }
}
