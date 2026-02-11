// Stub implementations for non-native targets (package is native-only)

///|
fn read_all_stdin() -> Bytes {
  Bytes::new(0)
}

///|
fn write_all_stdout(_data : Bytes) -> Unit {

}
