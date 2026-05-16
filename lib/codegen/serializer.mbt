// Protobuf response serialization

///|
pub fn serialize_response(response : @plugin.GenerateResponse) -> Bytes {
  // Use Array to accumulate bytes dynamically (no fixed size)
  let bytes : Array[Byte] = []

  // Write each file
  for file in response.files {
    // Tag for files field (field 1, wire type 2 = length-delimited)
    bytes.push(b'\x0a')

    // Calculate file message size from actual bytes. Generated size_of can lag
    // behind runtime Bytes semantics and corrupt large sqlc plugin responses.
    let file_size = file_message_size(file)

    // Write file size as varint
    let mut sz = file_size
    while sz >= 0x80U {
      bytes.push(((sz & 0x7FU) | 0x80U).to_byte())
      sz = sz >> 7
    }
    bytes.push(sz.to_byte())

    // Write name field (field 1)
    bytes.push(b'\x0a')
    let name_bytes = string_to_utf8(file.name)
    let name_len = name_bytes.length()
    let mut nl = name_len.reinterpret_as_uint()
    while nl >= 0x80U {
      bytes.push(((nl & 0x7FU) | 0x80U).to_byte())
      nl = nl >> 7
    }
    bytes.push(nl.to_byte())
    for i in 0..<name_len {
      bytes.push(name_bytes[i])
    }

    // Write contents field (field 2)
    bytes.push(b'\x12')
    let contents = file.contents
    let contents_len = contents.length()
    let mut cl = contents_len.reinterpret_as_uint()
    while cl >= 0x80U {
      bytes.push(((cl & 0x7FU) | 0x80U).to_byte())
      cl = cl >> 7
    }
    bytes.push(cl.to_byte())
    for i in 0..<contents_len {
      bytes.push(contents[i])
    }
  }
  Bytes::from_array(bytes)
}

///|
fn file_message_size(file : @plugin.File) -> UInt {
  let name_len = string_to_utf8(file.name).length().reinterpret_as_uint()
  let contents_len = file.contents.length().reinterpret_as_uint()
  1U +
  varint_size(name_len) +
  name_len +
  1U +
  varint_size(contents_len) +
  contents_len
}

///|
fn varint_size(value : UInt) -> UInt {
  let mut size = 1U
  let mut v = value
  while v >= 0x80U {
    size = size + 1U
    v = v >> 7
  }
  size
}
