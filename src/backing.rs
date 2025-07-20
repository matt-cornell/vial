use memmap2::MmapMut;
use std::fs::File;
use std::io;

/// A backing store for memory operations.
///
/// The bytes that it provides should be aligned to the maximum alignment.
///
/// # Safety
/// After a successful call to [`Self::resize`], [`bytes`](ByteAccess::bytes) and [`bytes_mut`](ByteAccessMut::bytes_mut) must return slices of at least the given length.
pub unsafe trait BackingStore: ByteAccessMut {
    /// Resize the store to have at least the specified size.
    fn resize(&mut self, len: usize) -> io::Result<()>;
}

/// A memory-mapped file, along with a handle to the original file.
#[derive(Debug)]
pub struct MappedFile {
    file: File,
    pub map: MmapMut,
}
impl MappedFile {
    /// Memory-map a file. This file should be opened for reading and writing.
    ///
    /// # Safety
    /// See [`MmapMut::map_mut`].
    pub unsafe fn new(file: File) -> io::Result<Self> {
        Ok(Self {
            map: unsafe { MmapMut::map_mut(&file)? },
            file,
        })
    }
    /// Create a temporary file and memory-map it.
    pub fn anon() -> io::Result<Self> {
        unsafe { Self::new(tempfile::tempfile()?) }
    }
}
impl ByteAccess for MappedFile {
    fn bytes(&self) -> &[u8] {
        debug_assert!(self.map.as_ptr().cast::<usize>().is_aligned());
        &self.map
    }
}
impl ByteAccessMut for MappedFile {
    fn bytes_mut(&mut self) -> &mut [u8] {
        debug_assert!(self.map.as_ptr().cast::<usize>().is_aligned());
        &mut self.map
    }
}
unsafe impl BackingStore for MappedFile {
    fn resize(&mut self, len: usize) -> io::Result<()> {
        self.file.set_len(len as _)?;
        self.map = unsafe { MmapMut::map_mut(&self.file)? };
        Ok(())
    }
}

impl ByteAccess for Vec<usize> {
    fn bytes(&self) -> &[u8] {
        bytemuck::cast_slice(self)
    }
}
impl ByteAccessMut for Vec<usize> {
    fn bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::cast_slice_mut(self)
    }
}
unsafe impl BackingStore for Vec<usize> {
    fn resize(&mut self, len: usize) -> io::Result<()> {
        Vec::resize(self, len.div_ceil(size_of::<usize>()), 0);
        Ok(())
    }
}

pub trait ByteAccess {
    fn bytes(&self) -> &[u8];
}
pub trait ByteAccessMut: ByteAccess {
    fn bytes_mut(&mut self) -> &mut [u8];
}
impl<T: ByteAccess + ?Sized> ByteAccess for &T {
    #[inline(always)]
    fn bytes(&self) -> &[u8] {
        T::bytes(self)
    }
}
impl<T: ByteAccess + ?Sized> ByteAccess for &mut T {
    #[inline(always)]
    fn bytes(&self) -> &[u8] {
        T::bytes(self)
    }
}
impl<T: ByteAccessMut + ?Sized> ByteAccessMut for &mut T {
    #[inline(always)]
    fn bytes_mut(&mut self) -> &mut [u8] {
        T::bytes_mut(self)
    }
}
impl ByteAccess for [u8] {
    fn bytes(&self) -> &[u8] {
        self
    }
}
impl ByteAccessMut for [u8] {
    fn bytes_mut(&mut self) -> &mut [u8] {
        self
    }
}
