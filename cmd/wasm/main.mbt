// WASM entry point for sqlc-gen-moonbit (WASI Preview 1)

// WASI Preview 1 FFI declarations
///|
extern "wasm" fn wasi_fd_read(fd : Int, iovs : Int, iovs_len : Int, nread : Int) -> Int =
  "wasi_snapshot_preview1" "fd_read"

///|
extern "wasm" fn wasi_fd_write(fd : Int, iovs : Int, iovs_len : Int, nwritten : Int) -> Int =
  "wasi_snapshot_preview1" "fd_write"

// Memory intrinsics - using inline WAT to avoid moonbit.* imports
///|
extern "wasm" fn wasm_memory_size() -> Int =
  #|(func (result i32) (memory.size))

///|
extern "wasm" fn wasm_memory_grow(pages : Int) -> Int =
  #|(func (param i32) (result i32) (memory.grow (local.get 0)))

///|
extern "wasm" fn store32(ptr : Int, value : Int) =
  #|(func (param i32) (param i32) (i32.store (local.get 0) (local.get 1)))

///|
extern "wasm" fn load32(ptr : Int) -> Int =
  #|(func (param i32) (result i32) (i32.load (local.get 0)))

///|
extern "wasm" fn store8(ptr : Int, value : Int) =
  #|(func (param i32) (param i32) (i32.store8 (local.get 0) (local.get 1)))

///|
extern "wasm" fn load8_u(ptr : Int) -> Int =
  #|(func (param i32) (result i32) (i32.load8_u (local.get 0)))

// Global heap pointer using Ref
let heap_ptr : Ref[Int] = Ref::new(65536)

///|
fn alloc(size : Int) -> Int {
  let ptr = heap_ptr.val
  // Align to 8 bytes
  heap_ptr.val = ptr + ((size + 7) & -8)
  // Grow memory if needed
  let pages_needed = (heap_ptr.val + 65535) / 65536
  let current_pages = wasm_memory_size()
  if pages_needed > current_pages {
    let _ = wasm_memory_grow(pages_needed - current_pages)
  }
  ptr
}

///|
fn read_all_stdin() -> Bytes {
  let chunks : Array[FixedArray[Byte]] = []
  let mut total_size = 0
  let chunk_size = 4096

  // Allocate iovec structure: [ptr: i32, len: i32]
  let iovec_ptr = alloc(8)
  let nread_ptr = alloc(4)
  let buffer_ptr = alloc(chunk_size)

  while true {
    // Set up iovec
    store32(iovec_ptr, buffer_ptr)
    store32(iovec_ptr + 4, chunk_size)

    // Call fd_read on stdin (fd=0)
    let errno = wasi_fd_read(0, iovec_ptr, 1, nread_ptr)
    if errno != 0 {
      break // Error
    }

    let bytes_read = load32(nread_ptr)
    if bytes_read == 0 {
      break // EOF
    }

    // Copy bytes to a chunk
    let chunk : FixedArray[Byte] = FixedArray::make(bytes_read, b'\x00')
    for i = 0; i < bytes_read; i = i + 1 {
      chunk[i] = load8_u(buffer_ptr + i).to_byte()
    }
    chunks.push(chunk)
    total_size = total_size + bytes_read
  }

  // Combine all chunks into a single FixedArray then convert to Bytes
  let result_arr : FixedArray[Byte] = FixedArray::make(total_size, b'\x00')
  let mut offset = 0
  for chunk in chunks {
    for i = 0; i < chunk.length(); i = i + 1 {
      result_arr[offset] = chunk[i]
      offset = offset + 1
    }
  }
  Bytes::from_fixedarray(result_arr)
}

///|
fn write_all_stdout(data : Bytes) -> Unit {
  let len = data.length()
  if len == 0 {
    return
  }

  // Copy data to linear memory
  let buffer_ptr = alloc(len)
  for i = 0; i < len; i = i + 1 {
    store8(buffer_ptr + i, data[i].to_int())
  }

  // Allocate iovec and nwritten
  let iovec_ptr = alloc(8)
  let nwritten_ptr = alloc(4)

  let mut written = 0
  while written < len {
    store32(iovec_ptr, buffer_ptr + written)
    store32(iovec_ptr + 4, len - written)

    // Call fd_write on stdout (fd=1)
    let errno = wasi_fd_write(1, iovec_ptr, 1, nwritten_ptr)
    if errno != 0 {
      break // Error
    }

    let bytes_written = load32(nwritten_ptr)
    if bytes_written == 0 {
      break
    }
    written = written + bytes_written
  }
}

///|
fn main {
  // Read stdin
  let input = read_all_stdin()

  // Process request using codegen library
  let output = @codegen.process_request(input)

  // Write to stdout
  write_all_stdout(output)
}
