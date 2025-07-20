//! Block access utilities
//!
//! These functions access data in blocks.

use super::*;

fn block_type_index(this: BlockId) -> io::Result<(usize, usize)> {
    let i = this.0.get() as usize;
    let block_start = (i & !0x3f) * BLOCK_SIZE;
    let offset = (i >> 6).checked_sub(1).ok_or_else(|| {
        io::Error::other(format!(
            "Block ID {i} points inside a file or superblock header"
        ))
    })?;
    Ok((block_start, size_of::<FileHeader>() + offset))
}
/// Get the block type if a block is in a normal superblock, or `None` if it's in a special-use one.
#[inline(always)]
pub fn block_type(
    block: BlockId,
    mem: &(impl WithoutHeader + ?Sized),
) -> io::Result<Option<BlockMarker>> {
    fn inner(this: BlockId, mem: &NonHeaderBytes) -> io::Result<Option<BlockMarker>> {
        let (super_start, offset) = block_type_index(this)?;
        let v = mem.get_index(super_start + offset).ok_or_else(|| {
            io::Error::other(format!(
                "Block ID {} is out of range for a file of {} bytes",
                this,
                mem.len(),
            ))
        })?;
        // already range checked
        let block_kind = unsafe { SuperblockType(*mem.get_index_unchecked(super_start)) };
        Ok(block_kind.is_normal().then_some(BlockMarker(*v)))
    }
    inner(block, mem.guard())
}
/// Like [`block_type`], but returns a mutable reference to the block marker.
#[inline(always)]
pub fn block_type_mut(
    block: BlockId,
    mem: &mut (impl WithoutHeader + ?Sized),
) -> io::Result<Option<&mut BlockMarker>> {
    fn inner(this: BlockId, mem: &mut NonHeaderBytes) -> io::Result<Option<&mut BlockMarker>> {
        let (super_start, offset) = block_type_index(this)?;
        let len = mem.len();
        let mp = mem.as_mut_ptr();
        let v = mem.get_index_mut(super_start + offset).ok_or_else(|| {
            io::Error::other(format!(
                "Block ID {this} is out of range for a file of {len} bytes"
            ))
        })?;
        // already range checked
        let block_kind = unsafe { SuperblockType(*mp.add(super_start)) };
        Ok(block_kind.is_normal().then_some(bytemuck::cast_mut(v)))
    }
    inner(block, mem.guard_mut())
}
/// Load the block data and block marker for a block.
///
/// This is safe because the marker is stored out of line, in the block header.
/// If this block is in a special-use superblock, then no block marker is returned.
#[inline(always)]
pub fn block_type_and_data(
    block: BlockId,
    mem: &mut (impl WithoutHeader + ?Sized),
) -> io::Result<(Option<&mut BlockMarker>, &mut [u8; BLOCK_SIZE])> {
    fn inner(
        this: BlockId,
        mem: &mut NonHeaderBytes,
    ) -> io::Result<(Option<&mut BlockMarker>, &mut [u8; BLOCK_SIZE])> {
        if this.0.get() >> 6 == 0 {
            return Err(io::Error::other("BlockId points to a superblock header"));
        }
        let (super_start, offset) = block_type_index(this)?;
        let len = mem.len();
        let mp = mem.as_mut_ptr();
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
    inner(block, mem.guard_mut())
}

/// Get a shared reference to the data for a block.
#[inline(always)]
pub fn block_data(
    block: BlockId,
    mem: &(impl WithoutHeader + ?Sized),
) -> io::Result<&[u8; BLOCK_SIZE]> {
    fn inner(this: BlockId, mem: &NonHeaderBytes) -> io::Result<&[u8; BLOCK_SIZE]> {
        let start = this.0.get() as usize * BLOCK_SIZE;
        let end = start + BLOCK_SIZE;
        let len = mem.len();
        mem.get_range(start..end)
            .map(bytemuck::from_bytes)
            .ok_or_else(|| {
                io::Error::other(format!(
                    "Block id {this} out of range for storage of {len} bytes",
                ))
            })
    }
    inner(block, mem.guard())
}

/// Get a mutable reference to the data for a block.
#[inline(always)]
pub fn block_data_mut(
    block: BlockId,
    mem: &mut (impl WithoutHeader + ?Sized),
) -> io::Result<&mut [u8; BLOCK_SIZE]> {
    fn inner(this: BlockId, mem: &mut NonHeaderBytes) -> io::Result<&mut [u8; BLOCK_SIZE]> {
        let start = this.0.get() as usize * BLOCK_SIZE;
        let end = start + BLOCK_SIZE;
        let len = mem.len();
        mem.get_range_mut(start..end)
            .map(bytemuck::from_bytes_mut)
            .ok_or_else(|| {
                io::Error::other(format!(
                    "Block id {this} out of range for storage of {len} bytes",
                ))
            })
    }
    inner(block, mem.guard_mut())
}
