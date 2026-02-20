// Native-only FFI declarations for stdin/stdout handling

///|
extern "C" fn moonbit_read_stdin_size() -> Int64 = "moonbit_read_stdin_size"

///|
#borrow(dest)
extern "C" fn moonbit_copy_stdin_data(dest : Bytes) -> Unit = "moonbit_copy_stdin_data"

///|
#borrow(data)
extern "C" fn moonbit_write_all_stdout(data : Bytes, len : Int64) -> Int = "moonbit_write_all_stdout"

///|
fn read_all_stdin() -> Bytes {
  // Use C FFI to read all data from stdin
  let size = moonbit_read_stdin_size()
  if size <= 0L {
    Bytes::new(0)
  } else {
    let buffer = Bytes::make(size.to_int(), 0)
    moonbit_copy_stdin_data(buffer)
    buffer
  }
}

///|
fn write_all_stdout(data : Bytes) -> Unit {
  let _ = moonbit_write_all_stdout(data, data.length().to_int64())
}
