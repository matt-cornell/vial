use super::*;

/// Header information for buddy-allocated blocks.
#[derive(Debug, Clone, Copy, PartialEq, Pod, Zeroable)]
#[repr(C)]
pub struct BuddyHeader {
    pub next: Option<BlockId>,
    pub _padding: [u8; 12],
    pub bitset: [u128; 7],
}

const fn bitmask(mut depth: u8) -> u128 {
    let mut out = (1u128 << (1 << (6 - depth))) - 1;
    while depth > 0 {
        out |= out << (1 << (7 - depth));
        depth -= 1;
    }
    !out
}

const BITMASKS: [u128; 7] = [
    bitmask(0), // ...10101010
    bitmask(1), // ...11001100
    bitmask(2), // ...11110000
    bitmask(3), // you see the pattern
    bitmask(4),
    bitmask(5),
    bitmask(6),
];

/// Allocate memory using buddy-allocator blocks.
///
/// For allocations smaller than 128 bytes, this will round up to the nearest power of two, then allocate
/// memory with that alignment within one of these blocks. For allocations greater than 128 bytes, they will
/// be rounded up to the nearest multiple of 128 and allocated in a contiguous span of blocks. Allocations
/// require a power-of-two sized space to be free, but don't claim all of the space.
///
/// This will fail for zero-sized allocations and allocations of more than 896 (7 × 128) bytes.
pub fn buddy_alloc<S: BackingStore>(store: &mut S, size: usize) -> io::Result<Id<()>> {
    if size == 0 {
        return Err(io::Error::other(
            "Attempted to buddy-allocate a zero-sized value",
        ));
    }
    if size > 7 * 128 {
        return Err(io::Error::other(format!(
            "Can't allocate more that 896 bytes through buddy allocation ({size} requested)"
        )));
    }
    let (header, rest) = file_header(store)?;
    let mut last = None;
    let mut block = header.first_partial_buddy_block;
    let mut ret = None;
    while let Some(blk) = block {
        let mut remove_block = true;
        let data = utils::block_data_mut(blk, rest)?;
        let (blk_header, _) = block_header::<BuddyHeader>(data);
        if blk_header.bitset.iter().all(|i| *i == 0) {
            let next = blk_header.next;
            let next_free = header.first_free_block.replace(blk);
            *block_header::<free::FreeBlock>(data).0 = free::FreeBlock { next_free };
            *utils::block_type_mut(blk, rest)?.ok_or_else(|| {
                io::Error::other("Buddy block (being freed) is in a special superblock")
            })? = BlockMarker::FREE;
            if let Some(last) = last {
                block_header::<BuddyHeader>(utils::block_data_mut(last, rest)?)
                    .0
                    .next = next;
            } else {
                header.first_partial_buddy_block = next;
            }
            block = next;
            continue;
        }
        match size {
            0..=64 => {
                let mut it = blk_header.bitset.iter_mut();
                for (n, bits) in it.by_ref().enumerate() {
                    let mut i = 0;
                    let mut rem = size;
                    let mut b = *bits;
                    // this loop coalesces power-of-two blocks such that all bits but the rightmost are 1, and the rightmost is 0 iff all of the bits in the original were 0
                    while rem > 1 {
                        let mask = BITMASKS[i];
                        b |= (b & mask) >> (1 << i); // filter for the upper half, shift it over the lower half, then bitwise or to combine
                        b |= mask; // keep the upper half of each pair all 1s
                        rem >>= 1;
                        i += 1;
                    }
                    let mask = (1 << size) - 1;
                    let offset = b.trailing_ones();
                    if offset < 128 {
                        *bits |= mask << offset;
                        ret = Some(blk.make_id(((n as u32 + 1) << 7) + offset)?);
                        break;
                    }
                    if *bits != u128::MAX {
                        remove_block = false;
                    }
                }
                if remove_block {
                    remove_block = it.all(|i| *i == u128::MAX);
                }
            }
            65..=128 => {
                let mut it = blk_header.bitset.iter_mut();
                for (n, bits) in it.by_ref().enumerate() {
                    match *bits {
                        0 => {
                            *bits = (1 << size) - 1;
                            ret = Some(blk.make_id((n as u32 + 1) << 7)?);
                        }
                        u128::MAX => {}
                        _ => remove_block = false,
                    }
                }
                if remove_block {
                    remove_block = it.all(|i| *i == u128::MAX);
                }
            }
            129.. => {
                let needed = size.div_ceil(128);
                let mut start = 0;
                let mut seen = 0;
                let mut it = blk_header.bitset.into_iter();
                for (n, bits) in it.by_ref().enumerate() {
                    match bits {
                        0 => {
                            seen += 1;
                            if seen == 1 {
                                start = n;
                            }
                            if seen == needed {
                                break;
                            }
                        }
                        u128::MAX => seen = 0,
                        _ => {
                            remove_block = false;
                            seen = 0;
                        }
                    }
                }
                if remove_block {
                    remove_block = it.all(|i| i == u128::MAX);
                }
                if seen == needed {
                    let last = start + needed - 1;
                    let rem = size % 128;
                    if rem == 0 {
                        blk_header.bitset[start..=last].fill(u128::MAX);
                    } else {
                        blk_header.bitset[start..last].fill(u128::MAX);
                        blk_header.bitset[last] = (1 << (size % 128)) - 1;
                    }
                    ret = Some(blk.make_id((start as u32 + 1) << 7)?);
                }
            }
        }
        if ret.is_some() {
            if remove_block {
                let next = blk_header.next.take();
                if let Some(prev) = last {
                    if prev == blk {
                        return Err(io::Error::other(
                            "Self-referential block, this should never happen naturally!",
                        ));
                    }
                    block_header::<BuddyHeader>(utils::block_data_mut(prev, rest)?)
                        .0
                        .next = next;
                } else {
                    header.first_partial_buddy_block = next;
                }
            }
            break;
        }
        last = std::mem::replace(&mut block, blk_header.next);
    }
    ret.ok_or(()).or_else(|_| {
        let sp = store as *mut S;
        let (blk, data) = free::alloc_block(store, BlockMarker::BUDDY)?;
        let mut bitset = [0; 7];
        let mut size = size;
        for i in &mut bitset {
            if let Some(s) = size.checked_sub(128) {
                size = s;
                *i = u128::MAX;
            } else {
                *i = (1 << size) - 1;
            }
        }
        let next = unsafe {
            file_header(&mut *sp)?
                .0
                .first_partial_buddy_block
                .replace(blk)
        };
        *block_header(data).0 = BuddyHeader {
            next,
            _padding: [0; 12],
            bitset,
        };
        blk.make_id(128)
    })
}

/// Free an allocation from [`buddy_alloc`].
///
/// Allocation state is just tracked through a bitset, so allocations can be freely split and merged, and
/// a range that wasn't allocated can be freed without issue (although that'd likely be caused by an error
/// somewhere else).
///
/// If this block is emptied, it'll be freed upon the next allocation.
pub fn buddy_free(store: &mut impl BackingStore, id: Id<()>, len: usize) -> io::Result<()> {
    if len == 0 {
        return Ok(());
    }
    let off = id.offset() as usize;
    if off < 128 {
        return Err(io::Error::other(format!(
            "Invalid ID {} to free, ID must be >= 128 mod 1024",
            id.index
        )));
    }
    if off + len > 1024 {
        return Err(io::Error::other(format!(
            "Freed range ({}+{}) extends past the end of a block",
            id.index, len,
        )));
    }
    let blk = id.block()?;
    let bytes = store.bytes_mut();
    let Some((header_data, rest)) = NonHeaderBytes::split_mut(bytes) else {
        return Err(io::Error::other(format!(
            "Block id {blk} out of range for storage of {} bytes",
            bytes.len(),
        )));
    };
    let (marker, data) = utils::block_type_and_data(blk, rest)?;
    match marker.copied() {
        None => return Err(io::Error::other("Buddy block is in a special superblock")),
        Some(BlockMarker::BUDDY) => {}
        Some(marker) => {
            return Err(io::Error::other(format!(
                "Unexpected block marker {marker}"
            )));
        }
    }
    let (header, _) = block_header::<BuddyHeader>(data);
    let was_full = header.bitset.iter().all(|i| *i == u128::MAX); // this could probably be optimized 
    let end = off + len;
    let start_idx = (off >> 7) - 1;
    let end_idx = (end >> 7) - 1;
    let start_bit = off & 127;
    let end_bit = end & 127;
    if start_idx == end_idx {
        header.bitset[start_idx] &= ((1 << start_bit) - 1) | !((1 << end_bit) - 1);
    } else {
        header.bitset[start_idx] &= (1 << start_bit) - 1;
        header.bitset[end_idx] &= !((1 << end_bit) - 1);
        header.bitset[(start_idx + 1)..end_idx].fill(0);
    }
    if was_full {
        let file = bytemuck::from_bytes_mut::<FileHeader>(header_data);
        header.next = file.first_partial_buddy_block.replace(blk);
    }
    Ok(())
}
