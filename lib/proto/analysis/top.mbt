///|
pub(all) struct Identifier {
  mut catalog : String
  mut schema : String
  mut name : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Identifier with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.catalog)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.schema)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.name)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for Identifier with default() -> Identifier {
  Identifier::{
    catalog: String::default(),
    schema: String::default(),
    name: String::default(),
  }
}

///|
pub fn Identifier::new(
  catalog : String,
  schema : String,
  name : String,
) -> Identifier {
  Identifier::{ catalog, schema, name }
}

///|
pub impl @protobuf.Read for Identifier with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Identifier raise {
  let msg = Identifier::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.catalog = reader |> @protobuf.read_string()
        (2, _) => msg.schema = reader |> @protobuf.read_string()
        (3, _) => msg.name = reader |> @protobuf.read_string()
        (_, wire) => reader |> @protobuf.read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.Write for Identifier with write(
  self : Identifier,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.catalog)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.schema)
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_string(self.name)
}

///|
pub impl ToJson for Identifier with to_json(self) {
  let json : Map[String, Json] = {}
  if self.catalog != Default::default() {
    json["catalog"] = self.catalog.to_json()
  }
  if self.schema != Default::default() {
    json["schema"] = self.schema.to_json()
  }
  if self.name != Default::default() {
    json["name"] = self.name.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Identifier with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Identifier raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Identifier"))
  }
  let message = Identifier::default()
  for key, value in obj {
    match (key, value) {
      ("catalog", value) => message.catalog = @json.from_json(value, path~)
      ("schema", value) => message.schema = @json.from_json(value, path~)
      ("name", value) => message.name = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Identifier with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Identifier raise {
  let msg = Identifier::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.catalog = reader |> @protobuf.async_read_string()
        (2, _) => msg.schema = reader |> @protobuf.async_read_string()
        (3, _) => msg.name = reader |> @protobuf.async_read_string()
        (_, wire) => reader |> @protobuf.async_read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.AsyncWrite for Identifier with write(
  self : Identifier,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.catalog)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.schema)
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_string(self.name)
}

///|
pub(all) struct Column {
  mut name : String
  mut original_name : String
  mut data_type : String
  mut not_null : Bool
  mut unsigned : Bool
  mut is_array : Bool
  mut array_dims : Int
  mut comment : String
  mut length : Int
  mut is_named_param : Bool
  mut is_func_call : Bool
  mut scope : String
  mut table : Identifier
  mut table_alias : String
  mut type_ : Identifier
  mut embed_table : Identifier
  mut is_sqlc_slice : Bool
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Column with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.name)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.original_name)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.data_type)
      @protobuf.size_of(size) + size
    }
  size += 1U + @protobuf.size_of(self.not_null)
  size += 1U + @protobuf.size_of(self.unsigned)
  size += 1U + @protobuf.size_of(self.is_array)
  size += 1U + @protobuf.size_of(self.array_dims)
  size += 1U +
    {
      let size = @protobuf.size_of(self.comment)
      @protobuf.size_of(size) + size
    }
  size += 1U + @protobuf.size_of(self.length)
  size += 1U + @protobuf.size_of(self.is_named_param)
  size += 1U + @protobuf.size_of(self.is_func_call)
  size += 1U +
    {
      let size = @protobuf.size_of(self.scope)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.table)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.table_alias)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.type_)
      @protobuf.size_of(size) + size
    }
  size += 2U +
    {
      let size = @protobuf.size_of(self.embed_table)
      @protobuf.size_of(size) + size
    }
  size += 2U + @protobuf.size_of(self.is_sqlc_slice)
  size
}

///|
pub impl Default for Column with default() -> Column {
  Column::{
    name: String::default(),
    original_name: String::default(),
    data_type: String::default(),
    not_null: Bool::default(),
    unsigned: Bool::default(),
    is_array: Bool::default(),
    array_dims: Int::default(),
    comment: String::default(),
    length: Int::default(),
    is_named_param: Bool::default(),
    is_func_call: Bool::default(),
    scope: String::default(),
    table: Identifier::default(),
    table_alias: String::default(),
    type_: Identifier::default(),
    embed_table: Identifier::default(),
    is_sqlc_slice: Bool::default(),
  }
}

///|
pub fn Column::new(
  name : String,
  original_name : String,
  data_type : String,
  not_null : Bool,
  unsigned : Bool,
  is_array : Bool,
  array_dims : Int,
  comment : String,
  length : Int,
  is_named_param : Bool,
  is_func_call : Bool,
  scope : String,
  table : Identifier,
  table_alias : String,
  type_ : Identifier,
  embed_table : Identifier,
  is_sqlc_slice : Bool,
) -> Column {
  Column::{
    name,
    original_name,
    data_type,
    not_null,
    unsigned,
    is_array,
    array_dims,
    comment,
    length,
    is_named_param,
    is_func_call,
    scope,
    table,
    table_alias,
    type_,
    embed_table,
    is_sqlc_slice,
  }
}

///|
pub impl @protobuf.Read for Column with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Column raise {
  let msg = Column::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.name = reader |> @protobuf.read_string()
        (2, _) => msg.original_name = reader |> @protobuf.read_string()
        (3, _) => msg.data_type = reader |> @protobuf.read_string()
        (4, _) => msg.not_null = reader |> @protobuf.read_bool()
        (5, _) => msg.unsigned = reader |> @protobuf.read_bool()
        (6, _) => msg.is_array = reader |> @protobuf.read_bool()
        (7, _) => msg.array_dims = reader |> @protobuf.read_int32()
        (8, _) => msg.comment = reader |> @protobuf.read_string()
        (9, _) => msg.length = reader |> @protobuf.read_int32()
        (10, _) => msg.is_named_param = reader |> @protobuf.read_bool()
        (11, _) => msg.is_func_call = reader |> @protobuf.read_bool()
        (12, _) => msg.scope = reader |> @protobuf.read_string()
        (13, _) => msg.table = (reader |> @protobuf.read_message() : Identifier)
        (14, _) => msg.table_alias = reader |> @protobuf.read_string()
        (15, _) => msg.type_ = (reader |> @protobuf.read_message() : Identifier)
        (16, _) =>
          msg.embed_table = (reader |> @protobuf.read_message() : Identifier)
        (17, _) => msg.is_sqlc_slice = reader |> @protobuf.read_bool()
        (_, wire) => reader |> @protobuf.read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.Write for Column with write(
  self : Column,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.name)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.original_name)
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_string(self.data_type)
  writer |> @protobuf.write_varint(32UL)
  writer |> @protobuf.write_bool(self.not_null)
  writer |> @protobuf.write_varint(40UL)
  writer |> @protobuf.write_bool(self.unsigned)
  writer |> @protobuf.write_varint(48UL)
  writer |> @protobuf.write_bool(self.is_array)
  writer |> @protobuf.write_varint(56UL)
  writer |> @protobuf.write_int32(self.array_dims)
  writer |> @protobuf.write_varint(66UL)
  writer |> @protobuf.write_string(self.comment)
  writer |> @protobuf.write_varint(72UL)
  writer |> @protobuf.write_int32(self.length)
  writer |> @protobuf.write_varint(80UL)
  writer |> @protobuf.write_bool(self.is_named_param)
  writer |> @protobuf.write_varint(88UL)
  writer |> @protobuf.write_bool(self.is_func_call)
  writer |> @protobuf.write_varint(98UL)
  writer |> @protobuf.write_string(self.scope)
  writer |> @protobuf.write_varint(106UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.table))
  @protobuf.Write::write(self.table, writer)
  writer |> @protobuf.write_varint(114UL)
  writer |> @protobuf.write_string(self.table_alias)
  writer |> @protobuf.write_varint(122UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.type_))
  @protobuf.Write::write(self.type_, writer)
  writer |> @protobuf.write_varint(130UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.embed_table))
  @protobuf.Write::write(self.embed_table, writer)
  writer |> @protobuf.write_varint(136UL)
  writer |> @protobuf.write_bool(self.is_sqlc_slice)
}

///|
pub impl ToJson for Column with to_json(self) {
  let json : Map[String, Json] = {}
  if self.name != Default::default() {
    json["name"] = self.name.to_json()
  }
  if self.original_name != Default::default() {
    json["originalName"] = self.original_name.to_json()
  }
  if self.data_type != Default::default() {
    json["dataType"] = self.data_type.to_json()
  }
  if self.not_null != Default::default() {
    json["notNull"] = self.not_null.to_json()
  }
  if self.unsigned != Default::default() {
    json["unsigned"] = self.unsigned.to_json()
  }
  if self.is_array != Default::default() {
    json["isArray"] = self.is_array.to_json()
  }
  if self.array_dims != Default::default() {
    json["arrayDims"] = self.array_dims.to_json()
  }
  if self.comment != Default::default() {
    json["comment"] = self.comment.to_json()
  }
  if self.length != Default::default() {
    json["length"] = self.length.to_json()
  }
  if self.is_named_param != Default::default() {
    json["isNamedParam"] = self.is_named_param.to_json()
  }
  if self.is_func_call != Default::default() {
    json["isFuncCall"] = self.is_func_call.to_json()
  }
  if self.scope != Default::default() {
    json["scope"] = self.scope.to_json()
  }
  if self.table != Default::default() {
    json["table"] = self.table.to_json()
  }
  if self.table_alias != Default::default() {
    json["tableAlias"] = self.table_alias.to_json()
  }
  if self.type_ != Default::default() {
    json["type"] = self.type_.to_json()
  }
  if self.embed_table != Default::default() {
    json["embedTable"] = self.embed_table.to_json()
  }
  if self.is_sqlc_slice != Default::default() {
    json["isSqlcSlice"] = self.is_sqlc_slice.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Column with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Column raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Column"))
  }
  let message = Column::default()
  for key, value in obj {
    match (key, value) {
      ("name", value) => message.name = @json.from_json(value, path~)
      ("originalName", value) =>
        message.original_name = @json.from_json(value, path~)
      ("dataType", value) => message.data_type = @json.from_json(value, path~)
      ("notNull", value) => message.not_null = @json.from_json(value, path~)
      ("unsigned", value) => message.unsigned = @json.from_json(value, path~)
      ("isArray", value) => message.is_array = @json.from_json(value, path~)
      ("arrayDims", value) => message.array_dims = @json.from_json(value, path~)
      ("comment", value) => message.comment = @json.from_json(value, path~)
      ("length", value) => message.length = @json.from_json(value, path~)
      ("isNamedParam", value) =>
        message.is_named_param = @json.from_json(value, path~)
      ("isFuncCall", value) =>
        message.is_func_call = @json.from_json(value, path~)
      ("scope", value) => message.scope = @json.from_json(value, path~)
      ("table", value) => message.table = @json.from_json(value, path~)
      ("tableAlias", value) =>
        message.table_alias = @json.from_json(value, path~)
      ("type", value) => message.type_ = @json.from_json(value, path~)
      ("embedTable", value) =>
        message.embed_table = @json.from_json(value, path~)
      ("isSqlcSlice", value) =>
        message.is_sqlc_slice = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Column with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Column raise {
  let msg = Column::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.name = reader |> @protobuf.async_read_string()
        (2, _) => msg.original_name = reader |> @protobuf.async_read_string()
        (3, _) => msg.data_type = reader |> @protobuf.async_read_string()
        (4, _) => msg.not_null = reader |> @protobuf.async_read_bool()
        (5, _) => msg.unsigned = reader |> @protobuf.async_read_bool()
        (6, _) => msg.is_array = reader |> @protobuf.async_read_bool()
        (7, _) => msg.array_dims = reader |> @protobuf.async_read_int32()
        (8, _) => msg.comment = reader |> @protobuf.async_read_string()
        (9, _) => msg.length = reader |> @protobuf.async_read_int32()
        (10, _) => msg.is_named_param = reader |> @protobuf.async_read_bool()
        (11, _) => msg.is_func_call = reader |> @protobuf.async_read_bool()
        (12, _) => msg.scope = reader |> @protobuf.async_read_string()
        (13, _) =>
          msg.table = (reader |> @protobuf.async_read_message() : Identifier)
        (14, _) => msg.table_alias = reader |> @protobuf.async_read_string()
        (15, _) =>
          msg.type_ = (reader |> @protobuf.async_read_message() : Identifier)
        (16, _) =>
          msg.embed_table = (
            reader |> @protobuf.async_read_message() : Identifier)
        (17, _) => msg.is_sqlc_slice = reader |> @protobuf.async_read_bool()
        (_, wire) => reader |> @protobuf.async_read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.AsyncWrite for Column with write(
  self : Column,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.name)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.original_name)
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_string(self.data_type)
  writer |> @protobuf.async_write_varint(32UL)
  writer |> @protobuf.async_write_bool(self.not_null)
  writer |> @protobuf.async_write_varint(40UL)
  writer |> @protobuf.async_write_bool(self.unsigned)
  writer |> @protobuf.async_write_varint(48UL)
  writer |> @protobuf.async_write_bool(self.is_array)
  writer |> @protobuf.async_write_varint(56UL)
  writer |> @protobuf.async_write_int32(self.array_dims)
  writer |> @protobuf.async_write_varint(66UL)
  writer |> @protobuf.async_write_string(self.comment)
  writer |> @protobuf.async_write_varint(72UL)
  writer |> @protobuf.async_write_int32(self.length)
  writer |> @protobuf.async_write_varint(80UL)
  writer |> @protobuf.async_write_bool(self.is_named_param)
  writer |> @protobuf.async_write_varint(88UL)
  writer |> @protobuf.async_write_bool(self.is_func_call)
  writer |> @protobuf.async_write_varint(98UL)
  writer |> @protobuf.async_write_string(self.scope)
  writer |> @protobuf.async_write_varint(106UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.table))
  @protobuf.AsyncWrite::write(self.table, writer)
  writer |> @protobuf.async_write_varint(114UL)
  writer |> @protobuf.async_write_string(self.table_alias)
  writer |> @protobuf.async_write_varint(122UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.type_))
  @protobuf.AsyncWrite::write(self.type_, writer)
  writer |> @protobuf.async_write_varint(130UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.embed_table))
  @protobuf.AsyncWrite::write(self.embed_table, writer)
  writer |> @protobuf.async_write_varint(136UL)
  writer |> @protobuf.async_write_bool(self.is_sqlc_slice)
}

///|
pub(all) struct Parameter {
  mut number : Int
  mut column : Column
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Parameter with size_of(self) {
  let mut size = 0U
  size += 1U + @protobuf.size_of(self.number)
  size += 1U +
    {
      let size = @protobuf.size_of(self.column)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for Parameter with default() -> Parameter {
  Parameter::{ number: Int::default(), column: Column::default() }
}

///|
pub fn Parameter::new(number : Int, column : Column) -> Parameter {
  Parameter::{ number, column }
}

///|
pub impl @protobuf.Read for Parameter with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Parameter raise {
  let msg = Parameter::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.number = reader |> @protobuf.read_int32()
        (2, _) => msg.column = (reader |> @protobuf.read_message() : Column)
        (_, wire) => reader |> @protobuf.read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.Write for Parameter with write(
  self : Parameter,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(8UL)
  writer |> @protobuf.write_int32(self.number)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.column))
  @protobuf.Write::write(self.column, writer)
}

///|
pub impl ToJson for Parameter with to_json(self) {
  let json : Map[String, Json] = {}
  if self.number != Default::default() {
    json["number"] = self.number.to_json()
  }
  if self.column != Default::default() {
    json["column"] = self.column.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Parameter with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Parameter raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Parameter"))
  }
  let message = Parameter::default()
  for key, value in obj {
    match (key, value) {
      ("number", value) => message.number = @json.from_json(value, path~)
      ("column", value) => message.column = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Parameter with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Parameter raise {
  let msg = Parameter::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.number = reader |> @protobuf.async_read_int32()
        (2, _) =>
          msg.column = (reader |> @protobuf.async_read_message() : Column)
        (_, wire) => reader |> @protobuf.async_read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.AsyncWrite for Parameter with write(
  self : Parameter,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(8UL)
  writer |> @protobuf.async_write_int32(self.number)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.column))
  @protobuf.AsyncWrite::write(self.column, writer)
}

///|
pub(all) struct Analysis {
  mut columns : Array[Column]
  mut params : Array[Parameter]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Analysis with size_of(self) {
  let mut size = 0U
  for s in self.columns {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  for s in self.params {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for Analysis with default() -> Analysis {
  Analysis::{ columns: [], params: [] }
}

///|
pub fn Analysis::new(
  columns : Array[Column],
  params : Array[Parameter],
) -> Analysis {
  Analysis::{ columns, params }
}

///|
pub impl @protobuf.Read for Analysis with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Analysis raise {
  let msg = Analysis::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.columns.push((reader |> @protobuf.read_message() : Column))
        (2, _) =>
          msg.params.push((reader |> @protobuf.read_message() : Parameter))
        (_, wire) => reader |> @protobuf.read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.Write for Analysis with write(
  self : Analysis,
  writer : &@protobuf.Writer,
) -> Unit raise {
  for item in self.columns {
    writer |> @protobuf.write_varint(10UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
  for item in self.params {
    writer |> @protobuf.write_varint(18UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
}

///|
pub impl ToJson for Analysis with to_json(self) {
  let json : Map[String, Json] = {}
  if self.columns != Default::default() {
    json["columns"] = self.columns.to_json()
  }
  if self.params != Default::default() {
    json["params"] = self.params.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Analysis with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Analysis raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Analysis"))
  }
  let message = Analysis::default()
  for key, value in obj {
    match (key, value) {
      ("columns", Array(value)) =>
        message.columns = value.map(v => @json.from_json(v, path~))
      ("params", Array(value)) =>
        message.params = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Analysis with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Analysis raise {
  let msg = Analysis::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.columns.push((reader |> @protobuf.async_read_message() : Column))
        (2, _) =>
          msg.params.push(
            (reader |> @protobuf.async_read_message() : Parameter),
          )
        (_, wire) => reader |> @protobuf.async_read_unknown(wire)
      }
    }
  } catch {
    @protobuf.EndOfStream => ()
    err => raise err
  }
  msg
}

///|
pub impl @protobuf.AsyncWrite for Analysis with write(
  self : Analysis,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  for item in self.columns {
    writer |> @protobuf.async_write_varint(10UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
  for item in self.params {
    writer |> @protobuf.async_write_varint(18UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
}
