// Protobuf response serialization

///|
pub fn serialize_response(response : @plugin.GenerateResponse) -> Bytes {
  // Use Array to accumulate bytes dynamically (no fixed size)
  let bytes : Array[Byte] = []

  // Write each file
  for file in response.files {
    // Tag for files field (field 1, wire type 2 = length-delimited)
    bytes.push(b'\x0a')

    // Calculate file message size
    let file_size = @protobuf.size_of(file)

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
