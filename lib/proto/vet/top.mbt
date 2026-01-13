///|
pub(all) struct Parameter {
  mut number : Int
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Parameter with size_of(self) {
  let mut size = 0U
  size += 1U + @protobuf.size_of(self.number)
  size
}

///|
pub impl Default for Parameter with default() -> Parameter {
  Parameter::{ number: Int::default() }
}

///|
pub fn Parameter::new(number : Int) -> Parameter {
  Parameter::{ number, }
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
}

///|
pub impl ToJson for Parameter with to_json(self) {
  let json : Map[String, Json] = {}
  if self.number != Default::default() {
    json["number"] = self.number.to_json()
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
}

///|
pub(all) struct Config {
  mut version : String
  mut engine : String
  mut schema : Array[String]
  mut queries : Array[String]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Config with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.version)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.engine)
      @protobuf.size_of(size) + size
    }
  for s in self.schema {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  for s in self.queries {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for Config with default() -> Config {
  Config::{
    version: String::default(),
    engine: String::default(),
    schema: [],
    queries: [],
  }
}

///|
pub fn Config::new(
  version : String,
  engine : String,
  schema : Array[String],
  queries : Array[String],
) -> Config {
  Config::{ version, engine, schema, queries }
}

///|
pub impl @protobuf.Read for Config with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Config raise {
  let msg = Config::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.version = reader |> @protobuf.read_string()
        (2, _) => msg.engine = reader |> @protobuf.read_string()
        (3, _) => msg.schema.push(reader |> @protobuf.read_string())
        (4, _) => msg.queries.push(reader |> @protobuf.read_string())
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
pub impl @protobuf.Write for Config with write(
  self : Config,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.version)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.engine)
  for item in self.schema {
    writer |> @protobuf.write_varint(26UL)
    writer |> @protobuf.write_string(item)
  }
  for item in self.queries {
    writer |> @protobuf.write_varint(34UL)
    writer |> @protobuf.write_string(item)
  }
}

///|
pub impl ToJson for Config with to_json(self) {
  let json : Map[String, Json] = {}
  if self.version != Default::default() {
    json["version"] = self.version.to_json()
  }
  if self.engine != Default::default() {
    json["engine"] = self.engine.to_json()
  }
  if self.schema != Default::default() {
    json["schema"] = self.schema.to_json()
  }
  if self.queries != Default::default() {
    json["queries"] = self.queries.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Config with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Config raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Config"))
  }
  let message = Config::default()
  for key, value in obj {
    match (key, value) {
      ("version", value) => message.version = @json.from_json(value, path~)
      ("engine", value) => message.engine = @json.from_json(value, path~)
      ("schema", Array(value)) =>
        message.schema = value.map(v => @json.from_json(v, path~))
      ("queries", Array(value)) =>
        message.queries = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Config with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Config raise {
  let msg = Config::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.version = reader |> @protobuf.async_read_string()
        (2, _) => msg.engine = reader |> @protobuf.async_read_string()
        (3, _) => msg.schema.push(reader |> @protobuf.async_read_string())
        (4, _) => msg.queries.push(reader |> @protobuf.async_read_string())
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
pub impl @protobuf.AsyncWrite for Config with write(
  self : Config,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.version)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.engine)
  for item in self.schema {
    writer |> @protobuf.async_write_varint(26UL)
    writer |> @protobuf.async_write_string(item)
  }
  for item in self.queries {
    writer |> @protobuf.async_write_varint(34UL)
    writer |> @protobuf.async_write_string(item)
  }
}

///|
pub(all) struct Query {
  mut sql : String
  mut name : String
  mut cmd : String
  mut params : Array[Parameter]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for Query with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.sql)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.name)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.cmd)
      @protobuf.size_of(size) + size
    }
  for s in self.params {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for Query with default() -> Query {
  Query::{
    sql: String::default(),
    name: String::default(),
    cmd: String::default(),
    params: [],
  }
}

///|
pub fn Query::new(
  sql : String,
  name : String,
  cmd : String,
  params : Array[Parameter],
) -> Query {
  Query::{ sql, name, cmd, params }
}

///|
pub impl @protobuf.Read for Query with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> Query raise {
  let msg = Query::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.sql = reader |> @protobuf.read_string()
        (2, _) => msg.name = reader |> @protobuf.read_string()
        (3, _) => msg.cmd = reader |> @protobuf.read_string()
        (4, _) =>
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
pub impl @protobuf.Write for Query with write(
  self : Query,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.sql)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.name)
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_string(self.cmd)
  for item in self.params {
    writer |> @protobuf.write_varint(34UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
}

///|
pub impl ToJson for Query with to_json(self) {
  let json : Map[String, Json] = {}
  if self.sql != Default::default() {
    json["sql"] = self.sql.to_json()
  }
  if self.name != Default::default() {
    json["name"] = self.name.to_json()
  }
  if self.cmd != Default::default() {
    json["cmd"] = self.cmd.to_json()
  }
  if self.params != Default::default() {
    json["parameters"] = self.params.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for Query with from_json(
  json : Json,
  path : @json.JsonPath,
) -> Query raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for Query"))
  }
  let message = Query::default()
  for key, value in obj {
    match (key, value) {
      ("sql", value) => message.sql = @json.from_json(value, path~)
      ("name", value) => message.name = @json.from_json(value, path~)
      ("cmd", value) => message.cmd = @json.from_json(value, path~)
      ("parameters", Array(value)) =>
        message.params = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for Query with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> Query raise {
  let msg = Query::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.sql = reader |> @protobuf.async_read_string()
        (2, _) => msg.name = reader |> @protobuf.async_read_string()
        (3, _) => msg.cmd = reader |> @protobuf.async_read_string()
        (4, _) =>
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
pub impl @protobuf.AsyncWrite for Query with write(
  self : Query,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.sql)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.name)
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_string(self.cmd)
  for item in self.params {
    writer |> @protobuf.async_write_varint(34UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
}

///|
pub(all) struct PostgreSQL {
  mut explain : PostgreSQLExplain
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for PostgreSQL with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.explain)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for PostgreSQL with default() -> PostgreSQL {
  PostgreSQL::{ explain: PostgreSQLExplain::default() }
}

///|
pub fn PostgreSQL::new(explain : PostgreSQLExplain) -> PostgreSQL {
  PostgreSQL::{ explain, }
}

///|
pub impl @protobuf.Read for PostgreSQL with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> PostgreSQL raise {
  let msg = PostgreSQL::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.explain = (reader |> @protobuf.read_message() : PostgreSQLExplain)
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
pub impl @protobuf.Write for PostgreSQL with write(
  self : PostgreSQL,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.explain))
  @protobuf.Write::write(self.explain, writer)
}

///|
pub impl ToJson for PostgreSQL with to_json(self) {
  let json : Map[String, Json] = {}
  if self.explain != Default::default() {
    json["explain"] = self.explain.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for PostgreSQL with from_json(
  json : Json,
  path : @json.JsonPath,
) -> PostgreSQL raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for PostgreSQL"))
  }
  let message = PostgreSQL::default()
  for key, value in obj {
    match (key, value) {
      ("explain", value) => message.explain = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for PostgreSQL with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> PostgreSQL raise {
  let msg = PostgreSQL::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.explain = (
            reader |> @protobuf.async_read_message() : PostgreSQLExplain)
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
pub impl @protobuf.AsyncWrite for PostgreSQL with write(
  self : PostgreSQL,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.explain))
  @protobuf.AsyncWrite::write(self.explain, writer)
}

///|
pub(all) struct PostgreSQLExplain_SettingsEntry {
  mut key : String
  mut value : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for PostgreSQLExplain_SettingsEntry with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.key)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.value)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for PostgreSQLExplain_SettingsEntry with default() -> PostgreSQLExplain_SettingsEntry {
  PostgreSQLExplain_SettingsEntry::{
    key: String::default(),
    value: String::default(),
  }
}

///|
pub fn PostgreSQLExplain_SettingsEntry::new(
  key : String,
  value : String,
) -> PostgreSQLExplain_SettingsEntry {
  PostgreSQLExplain_SettingsEntry::{ key, value }
}

///|
pub impl @protobuf.Read for PostgreSQLExplain_SettingsEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> PostgreSQLExplain_SettingsEntry raise {
  let msg = PostgreSQLExplain_SettingsEntry::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.read_string()
        (2, _) => msg.value = reader |> @protobuf.read_string()
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
pub impl @protobuf.Write for PostgreSQLExplain_SettingsEntry with write(
  self : PostgreSQLExplain_SettingsEntry,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.key)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.value)
}

///|
pub impl ToJson for PostgreSQLExplain_SettingsEntry with to_json(self) {
  let json : Map[String, Json] = {}
  if self.key != Default::default() {
    json["key"] = self.key.to_json()
  }
  if self.value != Default::default() {
    json["value"] = self.value.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for PostgreSQLExplain_SettingsEntry with from_json(
  json : Json,
  path : @json.JsonPath,
) -> PostgreSQLExplain_SettingsEntry raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for PostgreSQLExplain_SettingsEntry"),
    )
  }
  let message = PostgreSQLExplain_SettingsEntry::default()
  for key, value in obj {
    match (key, value) {
      ("key", value) => message.key = @json.from_json(value, path~)
      ("value", value) => message.value = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for PostgreSQLExplain_SettingsEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> PostgreSQLExplain_SettingsEntry raise {
  let msg = PostgreSQLExplain_SettingsEntry::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.async_read_string()
        (2, _) => msg.value = reader |> @protobuf.async_read_string()
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
pub impl @protobuf.AsyncWrite for PostgreSQLExplain_SettingsEntry with write(
  self : PostgreSQLExplain_SettingsEntry,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.key)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.value)
}

///|
pub(all) struct PostgreSQLExplain_Plan {
  mut node_type : String
  mut parent_relationship : String
  mut relation_name : String
  mut schema : String
  mut alias_ : String
  mut parallel_aware : Bool
  mut async_capable : Bool
  mut startup_cost : Float
  mut total_cost : Float
  mut plan_rows : UInt64
  mut plan_width : UInt64
  mut output : Array[String]
  mut plans : Array[PostgreSQLExplain_Plan]
  mut shared_hit_blocks : UInt64
  mut shared_read_blocks : UInt64
  mut shared_dirtied_blocks : UInt64
  mut shared_written_blocks : UInt64
  mut local_hit_blocks : UInt64
  mut local_read_blocks : UInt64
  mut local_dirtied_blocks : UInt64
  mut local_written_blocks : UInt64
  mut temp_read_blocks : UInt64
  mut temp_written_blocks : UInt64
  mut sort_key : Array[String]
  mut join_type : String
  mut inner_unique : Bool
  mut hash_cond : String
  mut index_name : String
  mut scan_direction : String
  mut index_cond : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for PostgreSQLExplain_Plan with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.node_type)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.parent_relationship)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.relation_name)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.schema)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.alias_)
      @protobuf.size_of(size) + size
    }
  size += 1U + @protobuf.size_of(self.parallel_aware)
  size += 1U + @protobuf.size_of(self.async_capable)
  size += 1U + 4U
  size += 1U + 4U
  size += 1U + @protobuf.size_of(self.plan_rows)
  size += 1U + @protobuf.size_of(self.plan_width)
  for s in self.output {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  for s in self.plans {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size += 1U + @protobuf.size_of(self.shared_hit_blocks)
  size += 1U + @protobuf.size_of(self.shared_read_blocks)
  size += 2U + @protobuf.size_of(self.shared_dirtied_blocks)
  size += 2U + @protobuf.size_of(self.shared_written_blocks)
  size += 2U + @protobuf.size_of(self.local_hit_blocks)
  size += 2U + @protobuf.size_of(self.local_read_blocks)
  size += 2U + @protobuf.size_of(self.local_dirtied_blocks)
  size += 2U + @protobuf.size_of(self.local_written_blocks)
  size += 2U + @protobuf.size_of(self.temp_read_blocks)
  size += 2U + @protobuf.size_of(self.temp_written_blocks)
  for s in self.sort_key {
    let s = @protobuf.size_of(s)
    size += 2U + @protobuf.size_of(s) + s
  }
  size += 2U +
    {
      let size = @protobuf.size_of(self.join_type)
      @protobuf.size_of(size) + size
    }
  size += 2U + @protobuf.size_of(self.inner_unique)
  size += 2U +
    {
      let size = @protobuf.size_of(self.hash_cond)
      @protobuf.size_of(size) + size
    }
  size += 2U +
    {
      let size = @protobuf.size_of(self.index_name)
      @protobuf.size_of(size) + size
    }
  size += 2U +
    {
      let size = @protobuf.size_of(self.scan_direction)
      @protobuf.size_of(size) + size
    }
  size += 2U +
    {
      let size = @protobuf.size_of(self.index_cond)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for PostgreSQLExplain_Plan with default() -> PostgreSQLExplain_Plan {
  PostgreSQLExplain_Plan::{
    node_type: String::default(),
    parent_relationship: String::default(),
    relation_name: String::default(),
    schema: String::default(),
    alias_: String::default(),
    parallel_aware: Bool::default(),
    async_capable: Bool::default(),
    startup_cost: Float::default(),
    total_cost: Float::default(),
    plan_rows: UInt64::default(),
    plan_width: UInt64::default(),
    output: [],
    plans: [],
    shared_hit_blocks: UInt64::default(),
    shared_read_blocks: UInt64::default(),
    shared_dirtied_blocks: UInt64::default(),
    shared_written_blocks: UInt64::default(),
    local_hit_blocks: UInt64::default(),
    local_read_blocks: UInt64::default(),
    local_dirtied_blocks: UInt64::default(),
    local_written_blocks: UInt64::default(),
    temp_read_blocks: UInt64::default(),
    temp_written_blocks: UInt64::default(),
    sort_key: [],
    join_type: String::default(),
    inner_unique: Bool::default(),
    hash_cond: String::default(),
    index_name: String::default(),
    scan_direction: String::default(),
    index_cond: String::default(),
  }
}

///|
pub fn PostgreSQLExplain_Plan::new(
  node_type : String,
  parent_relationship : String,
  relation_name : String,
  schema : String,
  alias_ : String,
  parallel_aware : Bool,
  async_capable : Bool,
  startup_cost : Float,
  total_cost : Float,
  plan_rows : UInt64,
  plan_width : UInt64,
  output : Array[String],
  plans : Array[PostgreSQLExplain_Plan],
  shared_hit_blocks : UInt64,
  shared_read_blocks : UInt64,
  shared_dirtied_blocks : UInt64,
  shared_written_blocks : UInt64,
  local_hit_blocks : UInt64,
  local_read_blocks : UInt64,
  local_dirtied_blocks : UInt64,
  local_written_blocks : UInt64,
  temp_read_blocks : UInt64,
  temp_written_blocks : UInt64,
  sort_key : Array[String],
  join_type : String,
  inner_unique : Bool,
  hash_cond : String,
  index_name : String,
  scan_direction : String,
  index_cond : String,
) -> PostgreSQLExplain_Plan {
  PostgreSQLExplain_Plan::{
    node_type,
    parent_relationship,
    relation_name,
    schema,
    alias_,
    parallel_aware,
    async_capable,
    startup_cost,
    total_cost,
    plan_rows,
    plan_width,
    output,
    plans,
    shared_hit_blocks,
    shared_read_blocks,
    shared_dirtied_blocks,
    shared_written_blocks,
    local_hit_blocks,
    local_read_blocks,
    local_dirtied_blocks,
    local_written_blocks,
    temp_read_blocks,
    temp_written_blocks,
    sort_key,
    join_type,
    inner_unique,
    hash_cond,
    index_name,
    scan_direction,
    index_cond,
  }
}

///|
pub impl @protobuf.Read for PostgreSQLExplain_Plan with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> PostgreSQLExplain_Plan raise {
  let msg = PostgreSQLExplain_Plan::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.node_type = reader |> @protobuf.read_string()
        (2, _) => msg.parent_relationship = reader |> @protobuf.read_string()
        (3, _) => msg.relation_name = reader |> @protobuf.read_string()
        (4, _) => msg.schema = reader |> @protobuf.read_string()
        (5, _) => msg.alias_ = reader |> @protobuf.read_string()
        (6, _) => msg.parallel_aware = reader |> @protobuf.read_bool()
        (7, _) => msg.async_capable = reader |> @protobuf.read_bool()
        (8, _) => msg.startup_cost = reader |> @protobuf.read_float()
        (9, _) => msg.total_cost = reader |> @protobuf.read_float()
        (10, _) => msg.plan_rows = reader |> @protobuf.read_uint64()
        (11, _) => msg.plan_width = reader |> @protobuf.read_uint64()
        (12, _) => msg.output.push(reader |> @protobuf.read_string())
        (13, _) =>
          msg.plans.push(
            (reader |> @protobuf.read_message() : PostgreSQLExplain_Plan),
          )
        (14, _) => msg.shared_hit_blocks = reader |> @protobuf.read_uint64()
        (15, _) => msg.shared_read_blocks = reader |> @protobuf.read_uint64()
        (16, _) => msg.shared_dirtied_blocks = reader |> @protobuf.read_uint64()
        (17, _) => msg.shared_written_blocks = reader |> @protobuf.read_uint64()
        (18, _) => msg.local_hit_blocks = reader |> @protobuf.read_uint64()
        (19, _) => msg.local_read_blocks = reader |> @protobuf.read_uint64()
        (20, _) => msg.local_dirtied_blocks = reader |> @protobuf.read_uint64()
        (21, _) => msg.local_written_blocks = reader |> @protobuf.read_uint64()
        (22, _) => msg.temp_read_blocks = reader |> @protobuf.read_uint64()
        (23, _) => msg.temp_written_blocks = reader |> @protobuf.read_uint64()
        (24, _) => msg.sort_key.push(reader |> @protobuf.read_string())
        (25, _) => msg.join_type = reader |> @protobuf.read_string()
        (26, _) => msg.inner_unique = reader |> @protobuf.read_bool()
        (27, _) => msg.hash_cond = reader |> @protobuf.read_string()
        (28, _) => msg.index_name = reader |> @protobuf.read_string()
        (29, _) => msg.scan_direction = reader |> @protobuf.read_string()
        (30, _) => msg.index_cond = reader |> @protobuf.read_string()
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
pub impl @protobuf.Write for PostgreSQLExplain_Plan with write(
  self : PostgreSQLExplain_Plan,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.node_type)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.parent_relationship)
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_string(self.relation_name)
  writer |> @protobuf.write_varint(34UL)
  writer |> @protobuf.write_string(self.schema)
  writer |> @protobuf.write_varint(42UL)
  writer |> @protobuf.write_string(self.alias_)
  writer |> @protobuf.write_varint(48UL)
  writer |> @protobuf.write_bool(self.parallel_aware)
  writer |> @protobuf.write_varint(56UL)
  writer |> @protobuf.write_bool(self.async_capable)
  writer |> @protobuf.write_varint(69UL)
  writer |> @protobuf.write_float(self.startup_cost)
  writer |> @protobuf.write_varint(77UL)
  writer |> @protobuf.write_float(self.total_cost)
  writer |> @protobuf.write_varint(80UL)
  writer |> @protobuf.write_uint64(self.plan_rows)
  writer |> @protobuf.write_varint(88UL)
  writer |> @protobuf.write_uint64(self.plan_width)
  for item in self.output {
    writer |> @protobuf.write_varint(98UL)
    writer |> @protobuf.write_string(item)
  }
  for item in self.plans {
    writer |> @protobuf.write_varint(106UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
  writer |> @protobuf.write_varint(112UL)
  writer |> @protobuf.write_uint64(self.shared_hit_blocks)
  writer |> @protobuf.write_varint(120UL)
  writer |> @protobuf.write_uint64(self.shared_read_blocks)
  writer |> @protobuf.write_varint(128UL)
  writer |> @protobuf.write_uint64(self.shared_dirtied_blocks)
  writer |> @protobuf.write_varint(136UL)
  writer |> @protobuf.write_uint64(self.shared_written_blocks)
  writer |> @protobuf.write_varint(144UL)
  writer |> @protobuf.write_uint64(self.local_hit_blocks)
  writer |> @protobuf.write_varint(152UL)
  writer |> @protobuf.write_uint64(self.local_read_blocks)
  writer |> @protobuf.write_varint(160UL)
  writer |> @protobuf.write_uint64(self.local_dirtied_blocks)
  writer |> @protobuf.write_varint(168UL)
  writer |> @protobuf.write_uint64(self.local_written_blocks)
  writer |> @protobuf.write_varint(176UL)
  writer |> @protobuf.write_uint64(self.temp_read_blocks)
  writer |> @protobuf.write_varint(184UL)
  writer |> @protobuf.write_uint64(self.temp_written_blocks)
  for item in self.sort_key {
    writer |> @protobuf.write_varint(194UL)
    writer |> @protobuf.write_string(item)
  }
  writer |> @protobuf.write_varint(202UL)
  writer |> @protobuf.write_string(self.join_type)
  writer |> @protobuf.write_varint(208UL)
  writer |> @protobuf.write_bool(self.inner_unique)
  writer |> @protobuf.write_varint(218UL)
  writer |> @protobuf.write_string(self.hash_cond)
  writer |> @protobuf.write_varint(226UL)
  writer |> @protobuf.write_string(self.index_name)
  writer |> @protobuf.write_varint(234UL)
  writer |> @protobuf.write_string(self.scan_direction)
  writer |> @protobuf.write_varint(242UL)
  writer |> @protobuf.write_string(self.index_cond)
}

///|
pub impl ToJson for PostgreSQLExplain_Plan with to_json(self) {
  let json : Map[String, Json] = {}
  if self.node_type != Default::default() {
    json["Node Type"] = self.node_type.to_json()
  }
  if self.parent_relationship != Default::default() {
    json["Parent Relationship"] = self.parent_relationship.to_json()
  }
  if self.relation_name != Default::default() {
    json["Relation Name"] = self.relation_name.to_json()
  }
  if self.schema != Default::default() {
    json["Schema"] = self.schema.to_json()
  }
  if self.alias_ != Default::default() {
    json["Alias"] = self.alias_.to_json()
  }
  if self.parallel_aware != Default::default() {
    json["Parallel Aware"] = self.parallel_aware.to_json()
  }
  if self.async_capable != Default::default() {
    json["Async Capable"] = self.async_capable.to_json()
  }
  if self.startup_cost != Default::default() {
    json["Startup Cost"] = self.startup_cost.to_json()
  }
  if self.total_cost != Default::default() {
    json["Total Cost"] = self.total_cost.to_json()
  }
  if self.plan_rows != Default::default() {
    json["Plan Rows"] = self.plan_rows.to_json()
  }
  if self.plan_width != Default::default() {
    json["Plan Width"] = self.plan_width.to_json()
  }
  if self.output != Default::default() {
    json["Output"] = self.output.to_json()
  }
  if self.plans != Default::default() {
    json["Plans"] = self.plans.to_json()
  }
  if self.shared_hit_blocks != Default::default() {
    json["Shared Hit Blocks"] = self.shared_hit_blocks.to_json()
  }
  if self.shared_read_blocks != Default::default() {
    json["Shared Read Blocks"] = self.shared_read_blocks.to_json()
  }
  if self.shared_dirtied_blocks != Default::default() {
    json["Shared Dirtied Blocks"] = self.shared_dirtied_blocks.to_json()
  }
  if self.shared_written_blocks != Default::default() {
    json["Shared Written Blocks"] = self.shared_written_blocks.to_json()
  }
  if self.local_hit_blocks != Default::default() {
    json["Local Hit Blocks"] = self.local_hit_blocks.to_json()
  }
  if self.local_read_blocks != Default::default() {
    json["Local Read Blocks"] = self.local_read_blocks.to_json()
  }
  if self.local_dirtied_blocks != Default::default() {
    json["Local Dirtied Blocks"] = self.local_dirtied_blocks.to_json()
  }
  if self.local_written_blocks != Default::default() {
    json["Local Written Blocks"] = self.local_written_blocks.to_json()
  }
  if self.temp_read_blocks != Default::default() {
    json["Temp Read Blocks"] = self.temp_read_blocks.to_json()
  }
  if self.temp_written_blocks != Default::default() {
    json["Temp Written Blocks"] = self.temp_written_blocks.to_json()
  }
  if self.sort_key != Default::default() {
    json["Sort Key"] = self.sort_key.to_json()
  }
  if self.join_type != Default::default() {
    json["Join Type"] = self.join_type.to_json()
  }
  if self.inner_unique != Default::default() {
    json["Inner Unique"] = self.inner_unique.to_json()
  }
  if self.hash_cond != Default::default() {
    json["Hash Cond"] = self.hash_cond.to_json()
  }
  if self.index_name != Default::default() {
    json["Index Name"] = self.index_name.to_json()
  }
  if self.scan_direction != Default::default() {
    json["Scan Direction"] = self.scan_direction.to_json()
  }
  if self.index_cond != Default::default() {
    json["Index Cond"] = self.index_cond.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for PostgreSQLExplain_Plan with from_json(
  json : Json,
  path : @json.JsonPath,
) -> PostgreSQLExplain_Plan raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for PostgreSQLExplain_Plan"),
    )
  }
  let message = PostgreSQLExplain_Plan::default()
  for key, value in obj {
    match (key, value) {
      ("Node Type", value) => message.node_type = @json.from_json(value, path~)
      ("Parent Relationship", value) =>
        message.parent_relationship = @json.from_json(value, path~)
      ("Relation Name", value) =>
        message.relation_name = @json.from_json(value, path~)
      ("Schema", value) => message.schema = @json.from_json(value, path~)
      ("Alias", value) => message.alias_ = @json.from_json(value, path~)
      ("Parallel Aware", value) =>
        message.parallel_aware = @json.from_json(value, path~)
      ("Async Capable", value) =>
        message.async_capable = @json.from_json(value, path~)
      ("Startup Cost", Number(value, ..)) =>
        message.startup_cost = Float::from_double(value)
      ("Total Cost", Number(value, ..)) =>
        message.total_cost = Float::from_double(value)
      ("Plan Rows", value) => message.plan_rows = @json.from_json(value, path~)
      ("Plan Width", value) =>
        message.plan_width = @json.from_json(value, path~)
      ("Output", Array(value)) =>
        message.output = value.map(v => @json.from_json(v, path~))
      ("Plans", Array(value)) =>
        message.plans = value.map(v => @json.from_json(v, path~))
      ("Shared Hit Blocks", value) =>
        message.shared_hit_blocks = @json.from_json(value, path~)
      ("Shared Read Blocks", value) =>
        message.shared_read_blocks = @json.from_json(value, path~)
      ("Shared Dirtied Blocks", value) =>
        message.shared_dirtied_blocks = @json.from_json(value, path~)
      ("Shared Written Blocks", value) =>
        message.shared_written_blocks = @json.from_json(value, path~)
      ("Local Hit Blocks", value) =>
        message.local_hit_blocks = @json.from_json(value, path~)
      ("Local Read Blocks", value) =>
        message.local_read_blocks = @json.from_json(value, path~)
      ("Local Dirtied Blocks", value) =>
        message.local_dirtied_blocks = @json.from_json(value, path~)
      ("Local Written Blocks", value) =>
        message.local_written_blocks = @json.from_json(value, path~)
      ("Temp Read Blocks", value) =>
        message.temp_read_blocks = @json.from_json(value, path~)
      ("Temp Written Blocks", value) =>
        message.temp_written_blocks = @json.from_json(value, path~)
      ("Sort Key", Array(value)) =>
        message.sort_key = value.map(v => @json.from_json(v, path~))
      ("Join Type", value) => message.join_type = @json.from_json(value, path~)
      ("Inner Unique", value) =>
        message.inner_unique = @json.from_json(value, path~)
      ("Hash Cond", value) => message.hash_cond = @json.from_json(value, path~)
      ("Index Name", value) =>
        message.index_name = @json.from_json(value, path~)
      ("Scan Direction", value) =>
        message.scan_direction = @json.from_json(value, path~)
      ("Index Cond", value) =>
        message.index_cond = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for PostgreSQLExplain_Plan with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> PostgreSQLExplain_Plan raise {
  let msg = PostgreSQLExplain_Plan::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.node_type = reader |> @protobuf.async_read_string()
        (2, _) =>
          msg.parent_relationship = reader |> @protobuf.async_read_string()
        (3, _) => msg.relation_name = reader |> @protobuf.async_read_string()
        (4, _) => msg.schema = reader |> @protobuf.async_read_string()
        (5, _) => msg.alias_ = reader |> @protobuf.async_read_string()
        (6, _) => msg.parallel_aware = reader |> @protobuf.async_read_bool()
        (7, _) => msg.async_capable = reader |> @protobuf.async_read_bool()
        (8, _) => msg.startup_cost = reader |> @protobuf.async_read_float()
        (9, _) => msg.total_cost = reader |> @protobuf.async_read_float()
        (10, _) => msg.plan_rows = reader |> @protobuf.async_read_uint64()
        (11, _) => msg.plan_width = reader |> @protobuf.async_read_uint64()
        (12, _) => msg.output.push(reader |> @protobuf.async_read_string())
        (13, _) =>
          msg.plans.push(
            (reader |> @protobuf.async_read_message() : PostgreSQLExplain_Plan),
          )
        (14, _) =>
          msg.shared_hit_blocks = reader |> @protobuf.async_read_uint64()
        (15, _) =>
          msg.shared_read_blocks = reader |> @protobuf.async_read_uint64()
        (16, _) =>
          msg.shared_dirtied_blocks = reader |> @protobuf.async_read_uint64()
        (17, _) =>
          msg.shared_written_blocks = reader |> @protobuf.async_read_uint64()
        (18, _) =>
          msg.local_hit_blocks = reader |> @protobuf.async_read_uint64()
        (19, _) =>
          msg.local_read_blocks = reader |> @protobuf.async_read_uint64()
        (20, _) =>
          msg.local_dirtied_blocks = reader |> @protobuf.async_read_uint64()
        (21, _) =>
          msg.local_written_blocks = reader |> @protobuf.async_read_uint64()
        (22, _) =>
          msg.temp_read_blocks = reader |> @protobuf.async_read_uint64()
        (23, _) =>
          msg.temp_written_blocks = reader |> @protobuf.async_read_uint64()
        (24, _) => msg.sort_key.push(reader |> @protobuf.async_read_string())
        (25, _) => msg.join_type = reader |> @protobuf.async_read_string()
        (26, _) => msg.inner_unique = reader |> @protobuf.async_read_bool()
        (27, _) => msg.hash_cond = reader |> @protobuf.async_read_string()
        (28, _) => msg.index_name = reader |> @protobuf.async_read_string()
        (29, _) => msg.scan_direction = reader |> @protobuf.async_read_string()
        (30, _) => msg.index_cond = reader |> @protobuf.async_read_string()
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
pub impl @protobuf.AsyncWrite for PostgreSQLExplain_Plan with write(
  self : PostgreSQLExplain_Plan,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.node_type)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.parent_relationship)
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_string(self.relation_name)
  writer |> @protobuf.async_write_varint(34UL)
  writer |> @protobuf.async_write_string(self.schema)
  writer |> @protobuf.async_write_varint(42UL)
  writer |> @protobuf.async_write_string(self.alias_)
  writer |> @protobuf.async_write_varint(48UL)
  writer |> @protobuf.async_write_bool(self.parallel_aware)
  writer |> @protobuf.async_write_varint(56UL)
  writer |> @protobuf.async_write_bool(self.async_capable)
  writer |> @protobuf.async_write_varint(69UL)
  writer |> @protobuf.async_write_float(self.startup_cost)
  writer |> @protobuf.async_write_varint(77UL)
  writer |> @protobuf.async_write_float(self.total_cost)
  writer |> @protobuf.async_write_varint(80UL)
  writer |> @protobuf.async_write_uint64(self.plan_rows)
  writer |> @protobuf.async_write_varint(88UL)
  writer |> @protobuf.async_write_uint64(self.plan_width)
  for item in self.output {
    writer |> @protobuf.async_write_varint(98UL)
    writer |> @protobuf.async_write_string(item)
  }
  for item in self.plans {
    writer |> @protobuf.async_write_varint(106UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
  writer |> @protobuf.async_write_varint(112UL)
  writer |> @protobuf.async_write_uint64(self.shared_hit_blocks)
  writer |> @protobuf.async_write_varint(120UL)
  writer |> @protobuf.async_write_uint64(self.shared_read_blocks)
  writer |> @protobuf.async_write_varint(128UL)
  writer |> @protobuf.async_write_uint64(self.shared_dirtied_blocks)
  writer |> @protobuf.async_write_varint(136UL)
  writer |> @protobuf.async_write_uint64(self.shared_written_blocks)
  writer |> @protobuf.async_write_varint(144UL)
  writer |> @protobuf.async_write_uint64(self.local_hit_blocks)
  writer |> @protobuf.async_write_varint(152UL)
  writer |> @protobuf.async_write_uint64(self.local_read_blocks)
  writer |> @protobuf.async_write_varint(160UL)
  writer |> @protobuf.async_write_uint64(self.local_dirtied_blocks)
  writer |> @protobuf.async_write_varint(168UL)
  writer |> @protobuf.async_write_uint64(self.local_written_blocks)
  writer |> @protobuf.async_write_varint(176UL)
  writer |> @protobuf.async_write_uint64(self.temp_read_blocks)
  writer |> @protobuf.async_write_varint(184UL)
  writer |> @protobuf.async_write_uint64(self.temp_written_blocks)
  for item in self.sort_key {
    writer |> @protobuf.async_write_varint(194UL)
    writer |> @protobuf.async_write_string(item)
  }
  writer |> @protobuf.async_write_varint(202UL)
  writer |> @protobuf.async_write_string(self.join_type)
  writer |> @protobuf.async_write_varint(208UL)
  writer |> @protobuf.async_write_bool(self.inner_unique)
  writer |> @protobuf.async_write_varint(218UL)
  writer |> @protobuf.async_write_string(self.hash_cond)
  writer |> @protobuf.async_write_varint(226UL)
  writer |> @protobuf.async_write_string(self.index_name)
  writer |> @protobuf.async_write_varint(234UL)
  writer |> @protobuf.async_write_string(self.scan_direction)
  writer |> @protobuf.async_write_varint(242UL)
  writer |> @protobuf.async_write_string(self.index_cond)
}

///|
pub(all) struct PostgreSQLExplain_Planning {
  mut shared_hit_blocks : UInt64
  mut shared_read_blocks : UInt64
  mut shared_dirtied_blocks : UInt64
  mut shared_written_blocks : UInt64
  mut local_hit_blocks : UInt64
  mut local_read_blocks : UInt64
  mut local_dirtied_blocks : UInt64
  mut local_written_blocks : UInt64
  mut temp_read_blocks : UInt64
  mut temp_written_blocks : UInt64
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for PostgreSQLExplain_Planning with size_of(self) {
  let mut size = 0U
  size += 1U + @protobuf.size_of(self.shared_hit_blocks)
  size += 1U + @protobuf.size_of(self.shared_read_blocks)
  size += 1U + @protobuf.size_of(self.shared_dirtied_blocks)
  size += 1U + @protobuf.size_of(self.shared_written_blocks)
  size += 1U + @protobuf.size_of(self.local_hit_blocks)
  size += 1U + @protobuf.size_of(self.local_read_blocks)
  size += 1U + @protobuf.size_of(self.local_dirtied_blocks)
  size += 1U + @protobuf.size_of(self.local_written_blocks)
  size += 1U + @protobuf.size_of(self.temp_read_blocks)
  size += 1U + @protobuf.size_of(self.temp_written_blocks)
  size
}

///|
pub impl Default for PostgreSQLExplain_Planning with default() -> PostgreSQLExplain_Planning {
  PostgreSQLExplain_Planning::{
    shared_hit_blocks: UInt64::default(),
    shared_read_blocks: UInt64::default(),
    shared_dirtied_blocks: UInt64::default(),
    shared_written_blocks: UInt64::default(),
    local_hit_blocks: UInt64::default(),
    local_read_blocks: UInt64::default(),
    local_dirtied_blocks: UInt64::default(),
    local_written_blocks: UInt64::default(),
    temp_read_blocks: UInt64::default(),
    temp_written_blocks: UInt64::default(),
  }
}

///|
pub fn PostgreSQLExplain_Planning::new(
  shared_hit_blocks : UInt64,
  shared_read_blocks : UInt64,
  shared_dirtied_blocks : UInt64,
  shared_written_blocks : UInt64,
  local_hit_blocks : UInt64,
  local_read_blocks : UInt64,
  local_dirtied_blocks : UInt64,
  local_written_blocks : UInt64,
  temp_read_blocks : UInt64,
  temp_written_blocks : UInt64,
) -> PostgreSQLExplain_Planning {
  PostgreSQLExplain_Planning::{
    shared_hit_blocks,
    shared_read_blocks,
    shared_dirtied_blocks,
    shared_written_blocks,
    local_hit_blocks,
    local_read_blocks,
    local_dirtied_blocks,
    local_written_blocks,
    temp_read_blocks,
    temp_written_blocks,
  }
}

///|
pub impl @protobuf.Read for PostgreSQLExplain_Planning with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> PostgreSQLExplain_Planning raise {
  let msg = PostgreSQLExplain_Planning::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.shared_hit_blocks = reader |> @protobuf.read_uint64()
        (2, _) => msg.shared_read_blocks = reader |> @protobuf.read_uint64()
        (3, _) => msg.shared_dirtied_blocks = reader |> @protobuf.read_uint64()
        (4, _) => msg.shared_written_blocks = reader |> @protobuf.read_uint64()
        (5, _) => msg.local_hit_blocks = reader |> @protobuf.read_uint64()
        (6, _) => msg.local_read_blocks = reader |> @protobuf.read_uint64()
        (7, _) => msg.local_dirtied_blocks = reader |> @protobuf.read_uint64()
        (8, _) => msg.local_written_blocks = reader |> @protobuf.read_uint64()
        (9, _) => msg.temp_read_blocks = reader |> @protobuf.read_uint64()
        (10, _) => msg.temp_written_blocks = reader |> @protobuf.read_uint64()
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
pub impl @protobuf.Write for PostgreSQLExplain_Planning with write(
  self : PostgreSQLExplain_Planning,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(8UL)
  writer |> @protobuf.write_uint64(self.shared_hit_blocks)
  writer |> @protobuf.write_varint(16UL)
  writer |> @protobuf.write_uint64(self.shared_read_blocks)
  writer |> @protobuf.write_varint(24UL)
  writer |> @protobuf.write_uint64(self.shared_dirtied_blocks)
  writer |> @protobuf.write_varint(32UL)
  writer |> @protobuf.write_uint64(self.shared_written_blocks)
  writer |> @protobuf.write_varint(40UL)
  writer |> @protobuf.write_uint64(self.local_hit_blocks)
  writer |> @protobuf.write_varint(48UL)
  writer |> @protobuf.write_uint64(self.local_read_blocks)
  writer |> @protobuf.write_varint(56UL)
  writer |> @protobuf.write_uint64(self.local_dirtied_blocks)
  writer |> @protobuf.write_varint(64UL)
  writer |> @protobuf.write_uint64(self.local_written_blocks)
  writer |> @protobuf.write_varint(72UL)
  writer |> @protobuf.write_uint64(self.temp_read_blocks)
  writer |> @protobuf.write_varint(80UL)
  writer |> @protobuf.write_uint64(self.temp_written_blocks)
}

///|
pub impl ToJson for PostgreSQLExplain_Planning with to_json(self) {
  let json : Map[String, Json] = {}
  if self.shared_hit_blocks != Default::default() {
    json["Shared Hit Blocks"] = self.shared_hit_blocks.to_json()
  }
  if self.shared_read_blocks != Default::default() {
    json["Shared Read Blocks"] = self.shared_read_blocks.to_json()
  }
  if self.shared_dirtied_blocks != Default::default() {
    json["Shared Dirtied Blocks"] = self.shared_dirtied_blocks.to_json()
  }
  if self.shared_written_blocks != Default::default() {
    json["Shared Written Blocks"] = self.shared_written_blocks.to_json()
  }
  if self.local_hit_blocks != Default::default() {
    json["Local Hit Blocks"] = self.local_hit_blocks.to_json()
  }
  if self.local_read_blocks != Default::default() {
    json["Local Read Blocks"] = self.local_read_blocks.to_json()
  }
  if self.local_dirtied_blocks != Default::default() {
    json["Local Dirtied Blocks"] = self.local_dirtied_blocks.to_json()
  }
  if self.local_written_blocks != Default::default() {
    json["Local Written Blocks"] = self.local_written_blocks.to_json()
  }
  if self.temp_read_blocks != Default::default() {
    json["Temp Read Blocks"] = self.temp_read_blocks.to_json()
  }
  if self.temp_written_blocks != Default::default() {
    json["Temp Written Blocks"] = self.temp_written_blocks.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for PostgreSQLExplain_Planning with from_json(
  json : Json,
  path : @json.JsonPath,
) -> PostgreSQLExplain_Planning raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for PostgreSQLExplain_Planning"),
    )
  }
  let message = PostgreSQLExplain_Planning::default()
  for key, value in obj {
    match (key, value) {
      ("Shared Hit Blocks", value) =>
        message.shared_hit_blocks = @json.from_json(value, path~)
      ("Shared Read Blocks", value) =>
        message.shared_read_blocks = @json.from_json(value, path~)
      ("Shared Dirtied Blocks", value) =>
        message.shared_dirtied_blocks = @json.from_json(value, path~)
      ("Shared Written Blocks", value) =>
        message.shared_written_blocks = @json.from_json(value, path~)
      ("Local Hit Blocks", value) =>
        message.local_hit_blocks = @json.from_json(value, path~)
      ("Local Read Blocks", value) =>
        message.local_read_blocks = @json.from_json(value, path~)
      ("Local Dirtied Blocks", value) =>
        message.local_dirtied_blocks = @json.from_json(value, path~)
      ("Local Written Blocks", value) =>
        message.local_written_blocks = @json.from_json(value, path~)
      ("Temp Read Blocks", value) =>
        message.temp_read_blocks = @json.from_json(value, path~)
      ("Temp Written Blocks", value) =>
        message.temp_written_blocks = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for PostgreSQLExplain_Planning with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> PostgreSQLExplain_Planning raise {
  let msg = PostgreSQLExplain_Planning::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.shared_hit_blocks = reader |> @protobuf.async_read_uint64()
        (2, _) =>
          msg.shared_read_blocks = reader |> @protobuf.async_read_uint64()
        (3, _) =>
          msg.shared_dirtied_blocks = reader |> @protobuf.async_read_uint64()
        (4, _) =>
          msg.shared_written_blocks = reader |> @protobuf.async_read_uint64()
        (5, _) => msg.local_hit_blocks = reader |> @protobuf.async_read_uint64()
        (6, _) =>
          msg.local_read_blocks = reader |> @protobuf.async_read_uint64()
        (7, _) =>
          msg.local_dirtied_blocks = reader |> @protobuf.async_read_uint64()
        (8, _) =>
          msg.local_written_blocks = reader |> @protobuf.async_read_uint64()
        (9, _) => msg.temp_read_blocks = reader |> @protobuf.async_read_uint64()
        (10, _) =>
          msg.temp_written_blocks = reader |> @protobuf.async_read_uint64()
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
pub impl @protobuf.AsyncWrite for PostgreSQLExplain_Planning with write(
  self : PostgreSQLExplain_Planning,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(8UL)
  writer |> @protobuf.async_write_uint64(self.shared_hit_blocks)
  writer |> @protobuf.async_write_varint(16UL)
  writer |> @protobuf.async_write_uint64(self.shared_read_blocks)
  writer |> @protobuf.async_write_varint(24UL)
  writer |> @protobuf.async_write_uint64(self.shared_dirtied_blocks)
  writer |> @protobuf.async_write_varint(32UL)
  writer |> @protobuf.async_write_uint64(self.shared_written_blocks)
  writer |> @protobuf.async_write_varint(40UL)
  writer |> @protobuf.async_write_uint64(self.local_hit_blocks)
  writer |> @protobuf.async_write_varint(48UL)
  writer |> @protobuf.async_write_uint64(self.local_read_blocks)
  writer |> @protobuf.async_write_varint(56UL)
  writer |> @protobuf.async_write_uint64(self.local_dirtied_blocks)
  writer |> @protobuf.async_write_varint(64UL)
  writer |> @protobuf.async_write_uint64(self.local_written_blocks)
  writer |> @protobuf.async_write_varint(72UL)
  writer |> @protobuf.async_write_uint64(self.temp_read_blocks)
  writer |> @protobuf.async_write_varint(80UL)
  writer |> @protobuf.async_write_uint64(self.temp_written_blocks)
}

///|
pub(all) struct PostgreSQLExplain {
  mut plan : PostgreSQLExplain_Plan
  mut settings : Map[String, String]
  mut planning : PostgreSQLExplain_Planning
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for PostgreSQLExplain with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.plan)
      @protobuf.size_of(size) + size
    }
  for k, v in self.settings {
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    size += 1U +
      @protobuf.size_of(key_size + value_size) +
      key_size +
      value_size
  }
  size += 1U +
    {
      let size = @protobuf.size_of(self.planning)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for PostgreSQLExplain with default() -> PostgreSQLExplain {
  PostgreSQLExplain::{
    plan: PostgreSQLExplain_Plan::default(),
    settings: {},
    planning: PostgreSQLExplain_Planning::default(),
  }
}

///|
pub fn PostgreSQLExplain::new(
  plan : PostgreSQLExplain_Plan,
  settings : Map[String, String],
  planning : PostgreSQLExplain_Planning,
) -> PostgreSQLExplain {
  PostgreSQLExplain::{ plan, settings, planning }
}

///|
pub impl @protobuf.Read for PostgreSQLExplain with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> PostgreSQLExplain raise {
  let msg = PostgreSQLExplain::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.plan = (
            reader |> @protobuf.read_message() : PostgreSQLExplain_Plan)
        (2, _) => {
          let { key, value } = (
            reader |> @protobuf.read_message() : PostgreSQLExplain_SettingsEntry)
          msg.settings[key] = value
        }
        (3, _) =>
          msg.planning = (
            reader |> @protobuf.read_message() : PostgreSQLExplain_Planning)
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
pub impl @protobuf.Write for PostgreSQLExplain with write(
  self : PostgreSQLExplain,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.plan))
  @protobuf.Write::write(self.plan, writer)
  let keys = self.settings.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.settings.get(k).unwrap()
    writer |> @protobuf.write_varint(18UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.write_uint32(key_size + value_size)
    writer |> @protobuf.write_varint(10UL)
    writer |> @protobuf.write_string(k)
    writer |> @protobuf.write_varint(18UL)
    writer |> @protobuf.write_string(v)
  }
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.planning))
  @protobuf.Write::write(self.planning, writer)
}

///|
pub impl ToJson for PostgreSQLExplain with to_json(self) {
  let json : Map[String, Json] = {}
  if self.plan != Default::default() {
    json["Plan"] = self.plan.to_json()
  }
  if self.settings != Default::default() {
    json["Settings"] = self.settings.to_json()
  }
  if self.planning != Default::default() {
    json["Planning"] = self.planning.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for PostgreSQLExplain with from_json(
  json : Json,
  path : @json.JsonPath,
) -> PostgreSQLExplain raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for PostgreSQLExplain"),
    )
  }
  let message = PostgreSQLExplain::default()
  for key, value in obj {
    match (key, value) {
      ("Plan", value) => message.plan = @json.from_json(value, path~)
      ("Settings", _) => message.settings = @json.from_json(value, path~)
      ("Planning", value) => message.planning = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for PostgreSQLExplain with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> PostgreSQLExplain raise {
  let msg = PostgreSQLExplain::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.plan = (
            reader |> @protobuf.async_read_message() : PostgreSQLExplain_Plan)
        (2, _) => {
          let { key, value } = (
            reader |> @protobuf.async_read_message() :
            PostgreSQLExplain_SettingsEntry)
          msg.settings[key] = value
        }
        (3, _) =>
          msg.planning = (
            reader |> @protobuf.async_read_message() :
            PostgreSQLExplain_Planning)
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
pub impl @protobuf.AsyncWrite for PostgreSQLExplain with write(
  self : PostgreSQLExplain,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.plan))
  @protobuf.AsyncWrite::write(self.plan, writer)
  let keys = self.settings.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.settings.get(k).unwrap()
    writer |> @protobuf.async_write_varint(18UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.async_write_uint32(key_size + value_size)
    writer |> @protobuf.async_write_varint(10UL)
    writer |> @protobuf.async_write_string(k)
    writer |> @protobuf.async_write_varint(18UL)
    writer |> @protobuf.async_write_string(v)
  }
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.planning))
  @protobuf.AsyncWrite::write(self.planning, writer)
}

///|
pub(all) struct MySQL {
  mut explain : MySQLExplain
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQL with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.explain)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQL with default() -> MySQL {
  MySQL::{ explain: MySQLExplain::default() }
}

///|
pub fn MySQL::new(explain : MySQLExplain) -> MySQL {
  MySQL::{ explain, }
}

///|
pub impl @protobuf.Read for MySQL with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQL raise {
  let msg = MySQL::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.explain = (reader |> @protobuf.read_message() : MySQLExplain)
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
pub impl @protobuf.Write for MySQL with write(
  self : MySQL,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.explain))
  @protobuf.Write::write(self.explain, writer)
}

///|
pub impl ToJson for MySQL with to_json(self) {
  let json : Map[String, Json] = {}
  if self.explain != Default::default() {
    json["explain"] = self.explain.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQL with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQL raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for MySQL"))
  }
  let message = MySQL::default()
  for key, value in obj {
    match (key, value) {
      ("explain", value) => message.explain = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQL with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQL raise {
  let msg = MySQL::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.explain = (reader |> @protobuf.async_read_message() : MySQLExplain)
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
pub impl @protobuf.AsyncWrite for MySQL with write(
  self : MySQL,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.explain))
  @protobuf.AsyncWrite::write(self.explain, writer)
}

///|
pub(all) struct MySQLExplain_QueryBlock_CostInfoEntry {
  mut key : String
  mut value : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_QueryBlock_CostInfoEntry with size_of(
  self,
) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.key)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.value)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQLExplain_QueryBlock_CostInfoEntry with default() -> MySQLExplain_QueryBlock_CostInfoEntry {
  MySQLExplain_QueryBlock_CostInfoEntry::{
    key: String::default(),
    value: String::default(),
  }
}

///|
pub fn MySQLExplain_QueryBlock_CostInfoEntry::new(
  key : String,
  value : String,
) -> MySQLExplain_QueryBlock_CostInfoEntry {
  MySQLExplain_QueryBlock_CostInfoEntry::{ key, value }
}

///|
pub impl @protobuf.Read for MySQLExplain_QueryBlock_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_QueryBlock_CostInfoEntry raise {
  let msg = MySQLExplain_QueryBlock_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.read_string()
        (2, _) => msg.value = reader |> @protobuf.read_string()
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
pub impl @protobuf.Write for MySQLExplain_QueryBlock_CostInfoEntry with write(
  self : MySQLExplain_QueryBlock_CostInfoEntry,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.key)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.value)
}

///|
pub impl ToJson for MySQLExplain_QueryBlock_CostInfoEntry with to_json(self) {
  let json : Map[String, Json] = {}
  if self.key != Default::default() {
    json["key"] = self.key.to_json()
  }
  if self.value != Default::default() {
    json["value"] = self.value.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_QueryBlock_CostInfoEntry with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_QueryBlock_CostInfoEntry raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_QueryBlock_CostInfoEntry"),
    )
  }
  let message = MySQLExplain_QueryBlock_CostInfoEntry::default()
  for key, value in obj {
    match (key, value) {
      ("key", value) => message.key = @json.from_json(value, path~)
      ("value", value) => message.value = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_QueryBlock_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_QueryBlock_CostInfoEntry raise {
  let msg = MySQLExplain_QueryBlock_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.async_read_string()
        (2, _) => msg.value = reader |> @protobuf.async_read_string()
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
pub impl @protobuf.AsyncWrite for MySQLExplain_QueryBlock_CostInfoEntry with write(
  self : MySQLExplain_QueryBlock_CostInfoEntry,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.key)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.value)
}

///|
pub(all) struct MySQLExplain_QueryBlock {
  mut select_id : UInt64
  mut message : String
  mut cost_info : Map[String, String]
  mut table : MySQLExplain_Table
  mut ordering_operation : MySQLExplain_OrderingOperation
  mut nested_loop : Array[MySQLExplain_NestedLoopObj]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_QueryBlock with size_of(self) {
  let mut size = 0U
  size += 1U + @protobuf.size_of(self.select_id)
  size += 1U +
    {
      let size = @protobuf.size_of(self.message)
      @protobuf.size_of(size) + size
    }
  for k, v in self.cost_info {
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    size += 1U +
      @protobuf.size_of(key_size + value_size) +
      key_size +
      value_size
  }
  size += 1U +
    {
      let size = @protobuf.size_of(self.table)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.ordering_operation)
      @protobuf.size_of(size) + size
    }
  for s in self.nested_loop {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for MySQLExplain_QueryBlock with default() -> MySQLExplain_QueryBlock {
  MySQLExplain_QueryBlock::{
    select_id: UInt64::default(),
    message: String::default(),
    cost_info: {},
    table: MySQLExplain_Table::default(),
    ordering_operation: MySQLExplain_OrderingOperation::default(),
    nested_loop: [],
  }
}

///|
pub fn MySQLExplain_QueryBlock::new(
  select_id : UInt64,
  message : String,
  cost_info : Map[String, String],
  table : MySQLExplain_Table,
  ordering_operation : MySQLExplain_OrderingOperation,
  nested_loop : Array[MySQLExplain_NestedLoopObj],
) -> MySQLExplain_QueryBlock {
  MySQLExplain_QueryBlock::{
    select_id,
    message,
    cost_info,
    table,
    ordering_operation,
    nested_loop,
  }
}

///|
pub impl @protobuf.Read for MySQLExplain_QueryBlock with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_QueryBlock raise {
  let msg = MySQLExplain_QueryBlock::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.select_id = reader |> @protobuf.read_uint64()
        (2, _) => msg.message = reader |> @protobuf.read_string()
        (3, _) => {
          let { key, value } = (
            reader |> @protobuf.read_message() :
            MySQLExplain_QueryBlock_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (4, _) =>
          msg.table = (reader |> @protobuf.read_message() : MySQLExplain_Table)
        (5, _) =>
          msg.ordering_operation = (
            reader |> @protobuf.read_message() : MySQLExplain_OrderingOperation)
        (6, _) =>
          msg.nested_loop.push(
            (reader |> @protobuf.read_message() : MySQLExplain_NestedLoopObj),
          )
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
pub impl @protobuf.Write for MySQLExplain_QueryBlock with write(
  self : MySQLExplain_QueryBlock,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(8UL)
  writer |> @protobuf.write_uint64(self.select_id)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.message)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.write_varint(26UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.write_uint32(key_size + value_size)
    writer |> @protobuf.write_varint(10UL)
    writer |> @protobuf.write_string(k)
    writer |> @protobuf.write_varint(18UL)
    writer |> @protobuf.write_string(v)
  }
  writer |> @protobuf.write_varint(34UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.table))
  @protobuf.Write::write(self.table, writer)
  writer |> @protobuf.write_varint(42UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.ordering_operation))
  @protobuf.Write::write(self.ordering_operation, writer)
  for item in self.nested_loop {
    writer |> @protobuf.write_varint(50UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
}

///|
pub impl ToJson for MySQLExplain_QueryBlock with to_json(self) {
  let json : Map[String, Json] = {}
  if self.select_id != Default::default() {
    json["selectId"] = self.select_id.to_json()
  }
  if self.message != Default::default() {
    json["message"] = self.message.to_json()
  }
  if self.cost_info != Default::default() {
    json["costInfo"] = self.cost_info.to_json()
  }
  if self.table != Default::default() {
    json["table"] = self.table.to_json()
  }
  if self.ordering_operation != Default::default() {
    json["orderingOperation"] = self.ordering_operation.to_json()
  }
  if self.nested_loop != Default::default() {
    json["nestedLoop"] = self.nested_loop.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_QueryBlock with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_QueryBlock raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_QueryBlock"),
    )
  }
  let message = MySQLExplain_QueryBlock::default()
  for key, value in obj {
    match (key, value) {
      ("selectId", value) => message.select_id = @json.from_json(value, path~)
      ("message", value) => message.message = @json.from_json(value, path~)
      ("costInfo", _) => message.cost_info = @json.from_json(value, path~)
      ("table", value) => message.table = @json.from_json(value, path~)
      ("orderingOperation", value) =>
        message.ordering_operation = @json.from_json(value, path~)
      ("nestedLoop", Array(value)) =>
        message.nested_loop = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_QueryBlock with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_QueryBlock raise {
  let msg = MySQLExplain_QueryBlock::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.select_id = reader |> @protobuf.async_read_uint64()
        (2, _) => msg.message = reader |> @protobuf.async_read_string()
        (3, _) => {
          let { key, value } = (
            reader |> @protobuf.async_read_message() :
            MySQLExplain_QueryBlock_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (4, _) =>
          msg.table = (
            reader |> @protobuf.async_read_message() : MySQLExplain_Table)
        (5, _) =>
          msg.ordering_operation = (
            reader |> @protobuf.async_read_message() :
            MySQLExplain_OrderingOperation)
        (6, _) =>
          msg.nested_loop.push(
            (
              reader |> @protobuf.async_read_message() :
              MySQLExplain_NestedLoopObj),
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
pub impl @protobuf.AsyncWrite for MySQLExplain_QueryBlock with write(
  self : MySQLExplain_QueryBlock,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(8UL)
  writer |> @protobuf.async_write_uint64(self.select_id)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.message)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.async_write_varint(26UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.async_write_uint32(key_size + value_size)
    writer |> @protobuf.async_write_varint(10UL)
    writer |> @protobuf.async_write_string(k)
    writer |> @protobuf.async_write_varint(18UL)
    writer |> @protobuf.async_write_string(v)
  }
  writer |> @protobuf.async_write_varint(34UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.table))
  @protobuf.AsyncWrite::write(self.table, writer)
  writer |> @protobuf.async_write_varint(42UL)
  writer
  |> @protobuf.async_write_uint32(@protobuf.size_of(self.ordering_operation))
  @protobuf.AsyncWrite::write(self.ordering_operation, writer)
  for item in self.nested_loop {
    writer |> @protobuf.async_write_varint(50UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
}

///|
pub(all) struct MySQLExplain_Table_CostInfoEntry {
  mut key : String
  mut value : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_Table_CostInfoEntry with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.key)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.value)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQLExplain_Table_CostInfoEntry with default() -> MySQLExplain_Table_CostInfoEntry {
  MySQLExplain_Table_CostInfoEntry::{
    key: String::default(),
    value: String::default(),
  }
}

///|
pub fn MySQLExplain_Table_CostInfoEntry::new(
  key : String,
  value : String,
) -> MySQLExplain_Table_CostInfoEntry {
  MySQLExplain_Table_CostInfoEntry::{ key, value }
}

///|
pub impl @protobuf.Read for MySQLExplain_Table_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_Table_CostInfoEntry raise {
  let msg = MySQLExplain_Table_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.read_string()
        (2, _) => msg.value = reader |> @protobuf.read_string()
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
pub impl @protobuf.Write for MySQLExplain_Table_CostInfoEntry with write(
  self : MySQLExplain_Table_CostInfoEntry,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.key)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.value)
}

///|
pub impl ToJson for MySQLExplain_Table_CostInfoEntry with to_json(self) {
  let json : Map[String, Json] = {}
  if self.key != Default::default() {
    json["key"] = self.key.to_json()
  }
  if self.value != Default::default() {
    json["value"] = self.value.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_Table_CostInfoEntry with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_Table_CostInfoEntry raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_Table_CostInfoEntry"),
    )
  }
  let message = MySQLExplain_Table_CostInfoEntry::default()
  for key, value in obj {
    match (key, value) {
      ("key", value) => message.key = @json.from_json(value, path~)
      ("value", value) => message.value = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_Table_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_Table_CostInfoEntry raise {
  let msg = MySQLExplain_Table_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.async_read_string()
        (2, _) => msg.value = reader |> @protobuf.async_read_string()
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
pub impl @protobuf.AsyncWrite for MySQLExplain_Table_CostInfoEntry with write(
  self : MySQLExplain_Table_CostInfoEntry,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.key)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.value)
}

///|
pub(all) struct MySQLExplain_Table {
  mut table_name : String
  mut access_type : String
  mut rows_examined_per_scan : UInt64
  mut rows_produced_per_join : UInt64
  mut filtered : String
  mut cost_info : Map[String, String]
  mut used_columns : Array[String]
  mut insert : Bool
  mut possible_keys : Array[String]
  mut key : String
  mut used_key_parts : Array[String]
  mut key_length : String
  mut ref_ : Array[String]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_Table with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.table_name)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.access_type)
      @protobuf.size_of(size) + size
    }
  size += 1U + @protobuf.size_of(self.rows_examined_per_scan)
  size += 1U + @protobuf.size_of(self.rows_produced_per_join)
  size += 1U +
    {
      let size = @protobuf.size_of(self.filtered)
      @protobuf.size_of(size) + size
    }
  for k, v in self.cost_info {
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    size += 1U +
      @protobuf.size_of(key_size + value_size) +
      key_size +
      value_size
  }
  for s in self.used_columns {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size += 1U + @protobuf.size_of(self.insert)
  for s in self.possible_keys {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size += 1U +
    {
      let size = @protobuf.size_of(self.key)
      @protobuf.size_of(size) + size
    }
  for s in self.used_key_parts {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size += 1U +
    {
      let size = @protobuf.size_of(self.key_length)
      @protobuf.size_of(size) + size
    }
  for s in self.ref_ {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for MySQLExplain_Table with default() -> MySQLExplain_Table {
  MySQLExplain_Table::{
    table_name: String::default(),
    access_type: String::default(),
    rows_examined_per_scan: UInt64::default(),
    rows_produced_per_join: UInt64::default(),
    filtered: String::default(),
    cost_info: {},
    used_columns: [],
    insert: Bool::default(),
    possible_keys: [],
    key: String::default(),
    used_key_parts: [],
    key_length: String::default(),
    ref_: [],
  }
}

///|
pub fn MySQLExplain_Table::new(
  table_name : String,
  access_type : String,
  rows_examined_per_scan : UInt64,
  rows_produced_per_join : UInt64,
  filtered : String,
  cost_info : Map[String, String],
  used_columns : Array[String],
  insert : Bool,
  possible_keys : Array[String],
  key : String,
  used_key_parts : Array[String],
  key_length : String,
  ref_ : Array[String],
) -> MySQLExplain_Table {
  MySQLExplain_Table::{
    table_name,
    access_type,
    rows_examined_per_scan,
    rows_produced_per_join,
    filtered,
    cost_info,
    used_columns,
    insert,
    possible_keys,
    key,
    used_key_parts,
    key_length,
    ref_,
  }
}

///|
pub impl @protobuf.Read for MySQLExplain_Table with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_Table raise {
  let msg = MySQLExplain_Table::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.table_name = reader |> @protobuf.read_string()
        (2, _) => msg.access_type = reader |> @protobuf.read_string()
        (3, _) => msg.rows_examined_per_scan = reader |> @protobuf.read_uint64()
        (4, _) => msg.rows_produced_per_join = reader |> @protobuf.read_uint64()
        (5, _) => msg.filtered = reader |> @protobuf.read_string()
        (6, _) => {
          let { key, value } = (
            reader |> @protobuf.read_message() :
            MySQLExplain_Table_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (7, _) => msg.used_columns.push(reader |> @protobuf.read_string())
        (8, _) => msg.insert = reader |> @protobuf.read_bool()
        (9, _) => msg.possible_keys.push(reader |> @protobuf.read_string())
        (10, _) => msg.key = reader |> @protobuf.read_string()
        (11, _) => msg.used_key_parts.push(reader |> @protobuf.read_string())
        (12, _) => msg.key_length = reader |> @protobuf.read_string()
        (13, _) => msg.ref_.push(reader |> @protobuf.read_string())
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
pub impl @protobuf.Write for MySQLExplain_Table with write(
  self : MySQLExplain_Table,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.table_name)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.access_type)
  writer |> @protobuf.write_varint(24UL)
  writer |> @protobuf.write_uint64(self.rows_examined_per_scan)
  writer |> @protobuf.write_varint(32UL)
  writer |> @protobuf.write_uint64(self.rows_produced_per_join)
  writer |> @protobuf.write_varint(42UL)
  writer |> @protobuf.write_string(self.filtered)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.write_varint(50UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.write_uint32(key_size + value_size)
    writer |> @protobuf.write_varint(10UL)
    writer |> @protobuf.write_string(k)
    writer |> @protobuf.write_varint(18UL)
    writer |> @protobuf.write_string(v)
  }
  for item in self.used_columns {
    writer |> @protobuf.write_varint(58UL)
    writer |> @protobuf.write_string(item)
  }
  writer |> @protobuf.write_varint(64UL)
  writer |> @protobuf.write_bool(self.insert)
  for item in self.possible_keys {
    writer |> @protobuf.write_varint(74UL)
    writer |> @protobuf.write_string(item)
  }
  writer |> @protobuf.write_varint(82UL)
  writer |> @protobuf.write_string(self.key)
  for item in self.used_key_parts {
    writer |> @protobuf.write_varint(90UL)
    writer |> @protobuf.write_string(item)
  }
  writer |> @protobuf.write_varint(98UL)
  writer |> @protobuf.write_string(self.key_length)
  for item in self.ref_ {
    writer |> @protobuf.write_varint(106UL)
    writer |> @protobuf.write_string(item)
  }
}

///|
pub impl ToJson for MySQLExplain_Table with to_json(self) {
  let json : Map[String, Json] = {}
  if self.table_name != Default::default() {
    json["tableName"] = self.table_name.to_json()
  }
  if self.access_type != Default::default() {
    json["accessType"] = self.access_type.to_json()
  }
  if self.rows_examined_per_scan != Default::default() {
    json["rowsExaminedPerScan"] = self.rows_examined_per_scan.to_json()
  }
  if self.rows_produced_per_join != Default::default() {
    json["rowsProducedPerJoin"] = self.rows_produced_per_join.to_json()
  }
  if self.filtered != Default::default() {
    json["filtered"] = self.filtered.to_json()
  }
  if self.cost_info != Default::default() {
    json["costInfo"] = self.cost_info.to_json()
  }
  if self.used_columns != Default::default() {
    json["usedColumns"] = self.used_columns.to_json()
  }
  if self.insert != Default::default() {
    json["insert"] = self.insert.to_json()
  }
  if self.possible_keys != Default::default() {
    json["possibleKeys"] = self.possible_keys.to_json()
  }
  if self.key != Default::default() {
    json["key"] = self.key.to_json()
  }
  if self.used_key_parts != Default::default() {
    json["usedKeyParts"] = self.used_key_parts.to_json()
  }
  if self.key_length != Default::default() {
    json["keyLength"] = self.key_length.to_json()
  }
  if self.ref_ != Default::default() {
    json["ref"] = self.ref_.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_Table with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_Table raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_Table"),
    )
  }
  let message = MySQLExplain_Table::default()
  for key, value in obj {
    match (key, value) {
      ("tableName", value) => message.table_name = @json.from_json(value, path~)
      ("accessType", value) =>
        message.access_type = @json.from_json(value, path~)
      ("rowsExaminedPerScan", value) =>
        message.rows_examined_per_scan = @json.from_json(value, path~)
      ("rowsProducedPerJoin", value) =>
        message.rows_produced_per_join = @json.from_json(value, path~)
      ("filtered", value) => message.filtered = @json.from_json(value, path~)
      ("costInfo", _) => message.cost_info = @json.from_json(value, path~)
      ("usedColumns", Array(value)) =>
        message.used_columns = value.map(v => @json.from_json(v, path~))
      ("insert", value) => message.insert = @json.from_json(value, path~)
      ("possibleKeys", Array(value)) =>
        message.possible_keys = value.map(v => @json.from_json(v, path~))
      ("key", value) => message.key = @json.from_json(value, path~)
      ("usedKeyParts", Array(value)) =>
        message.used_key_parts = value.map(v => @json.from_json(v, path~))
      ("keyLength", value) => message.key_length = @json.from_json(value, path~)
      ("ref", Array(value)) =>
        message.ref_ = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_Table with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_Table raise {
  let msg = MySQLExplain_Table::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.table_name = reader |> @protobuf.async_read_string()
        (2, _) => msg.access_type = reader |> @protobuf.async_read_string()
        (3, _) =>
          msg.rows_examined_per_scan = reader |> @protobuf.async_read_uint64()
        (4, _) =>
          msg.rows_produced_per_join = reader |> @protobuf.async_read_uint64()
        (5, _) => msg.filtered = reader |> @protobuf.async_read_string()
        (6, _) => {
          let { key, value } = (
            reader |> @protobuf.async_read_message() :
            MySQLExplain_Table_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (7, _) => msg.used_columns.push(reader |> @protobuf.async_read_string())
        (8, _) => msg.insert = reader |> @protobuf.async_read_bool()
        (9, _) =>
          msg.possible_keys.push(reader |> @protobuf.async_read_string())
        (10, _) => msg.key = reader |> @protobuf.async_read_string()
        (11, _) =>
          msg.used_key_parts.push(reader |> @protobuf.async_read_string())
        (12, _) => msg.key_length = reader |> @protobuf.async_read_string()
        (13, _) => msg.ref_.push(reader |> @protobuf.async_read_string())
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
pub impl @protobuf.AsyncWrite for MySQLExplain_Table with write(
  self : MySQLExplain_Table,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.table_name)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.access_type)
  writer |> @protobuf.async_write_varint(24UL)
  writer |> @protobuf.async_write_uint64(self.rows_examined_per_scan)
  writer |> @protobuf.async_write_varint(32UL)
  writer |> @protobuf.async_write_uint64(self.rows_produced_per_join)
  writer |> @protobuf.async_write_varint(42UL)
  writer |> @protobuf.async_write_string(self.filtered)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.async_write_varint(50UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.async_write_uint32(key_size + value_size)
    writer |> @protobuf.async_write_varint(10UL)
    writer |> @protobuf.async_write_string(k)
    writer |> @protobuf.async_write_varint(18UL)
    writer |> @protobuf.async_write_string(v)
  }
  for item in self.used_columns {
    writer |> @protobuf.async_write_varint(58UL)
    writer |> @protobuf.async_write_string(item)
  }
  writer |> @protobuf.async_write_varint(64UL)
  writer |> @protobuf.async_write_bool(self.insert)
  for item in self.possible_keys {
    writer |> @protobuf.async_write_varint(74UL)
    writer |> @protobuf.async_write_string(item)
  }
  writer |> @protobuf.async_write_varint(82UL)
  writer |> @protobuf.async_write_string(self.key)
  for item in self.used_key_parts {
    writer |> @protobuf.async_write_varint(90UL)
    writer |> @protobuf.async_write_string(item)
  }
  writer |> @protobuf.async_write_varint(98UL)
  writer |> @protobuf.async_write_string(self.key_length)
  for item in self.ref_ {
    writer |> @protobuf.async_write_varint(106UL)
    writer |> @protobuf.async_write_string(item)
  }
}

///|
pub(all) struct MySQLExplain_NestedLoopObj {
  mut table : MySQLExplain_Table
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_NestedLoopObj with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.table)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQLExplain_NestedLoopObj with default() -> MySQLExplain_NestedLoopObj {
  MySQLExplain_NestedLoopObj::{ table: MySQLExplain_Table::default() }
}

///|
pub fn MySQLExplain_NestedLoopObj::new(
  table : MySQLExplain_Table,
) -> MySQLExplain_NestedLoopObj {
  MySQLExplain_NestedLoopObj::{ table, }
}

///|
pub impl @protobuf.Read for MySQLExplain_NestedLoopObj with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_NestedLoopObj raise {
  let msg = MySQLExplain_NestedLoopObj::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.table = (reader |> @protobuf.read_message() : MySQLExplain_Table)
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
pub impl @protobuf.Write for MySQLExplain_NestedLoopObj with write(
  self : MySQLExplain_NestedLoopObj,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.table))
  @protobuf.Write::write(self.table, writer)
}

///|
pub impl ToJson for MySQLExplain_NestedLoopObj with to_json(self) {
  let json : Map[String, Json] = {}
  if self.table != Default::default() {
    json["table"] = self.table.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_NestedLoopObj with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_NestedLoopObj raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_NestedLoopObj"),
    )
  }
  let message = MySQLExplain_NestedLoopObj::default()
  for key, value in obj {
    match (key, value) {
      ("table", value) => message.table = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_NestedLoopObj with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_NestedLoopObj raise {
  let msg = MySQLExplain_NestedLoopObj::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.table = (
            reader |> @protobuf.async_read_message() : MySQLExplain_Table)
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
pub impl @protobuf.AsyncWrite for MySQLExplain_NestedLoopObj with write(
  self : MySQLExplain_NestedLoopObj,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.table))
  @protobuf.AsyncWrite::write(self.table, writer)
}

///|
pub(all) struct MySQLExplain_OrderingOperation_CostInfoEntry {
  mut key : String
  mut value : String
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_OrderingOperation_CostInfoEntry with size_of(
  self,
) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.key)
      @protobuf.size_of(size) + size
    }
  size += 1U +
    {
      let size = @protobuf.size_of(self.value)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQLExplain_OrderingOperation_CostInfoEntry with default() -> MySQLExplain_OrderingOperation_CostInfoEntry {
  MySQLExplain_OrderingOperation_CostInfoEntry::{
    key: String::default(),
    value: String::default(),
  }
}

///|
pub fn MySQLExplain_OrderingOperation_CostInfoEntry::new(
  key : String,
  value : String,
) -> MySQLExplain_OrderingOperation_CostInfoEntry {
  MySQLExplain_OrderingOperation_CostInfoEntry::{ key, value }
}

///|
pub impl @protobuf.Read for MySQLExplain_OrderingOperation_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_OrderingOperation_CostInfoEntry raise {
  let msg = MySQLExplain_OrderingOperation_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.read_string()
        (2, _) => msg.value = reader |> @protobuf.read_string()
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
pub impl @protobuf.Write for MySQLExplain_OrderingOperation_CostInfoEntry with write(
  self : MySQLExplain_OrderingOperation_CostInfoEntry,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_string(self.key)
  writer |> @protobuf.write_varint(18UL)
  writer |> @protobuf.write_string(self.value)
}

///|
pub impl ToJson for MySQLExplain_OrderingOperation_CostInfoEntry with to_json(
  self,
) {
  let json : Map[String, Json] = {}
  if self.key != Default::default() {
    json["key"] = self.key.to_json()
  }
  if self.value != Default::default() {
    json["value"] = self.value.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_OrderingOperation_CostInfoEntry with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_OrderingOperation_CostInfoEntry raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (
        path, "Expected an object for MySQLExplain_OrderingOperation_CostInfoEntry",
      ),
    )
  }
  let message = MySQLExplain_OrderingOperation_CostInfoEntry::default()
  for key, value in obj {
    match (key, value) {
      ("key", value) => message.key = @json.from_json(value, path~)
      ("value", value) => message.value = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_OrderingOperation_CostInfoEntry with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_OrderingOperation_CostInfoEntry raise {
  let msg = MySQLExplain_OrderingOperation_CostInfoEntry::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.key = reader |> @protobuf.async_read_string()
        (2, _) => msg.value = reader |> @protobuf.async_read_string()
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
pub impl @protobuf.AsyncWrite for MySQLExplain_OrderingOperation_CostInfoEntry with write(
  self : MySQLExplain_OrderingOperation_CostInfoEntry,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_string(self.key)
  writer |> @protobuf.async_write_varint(18UL)
  writer |> @protobuf.async_write_string(self.value)
}

///|
pub(all) struct MySQLExplain_OrderingOperation {
  mut using_filesort : Bool
  mut cost_info : Map[String, String]
  mut table : MySQLExplain_Table
  mut nested_loop : Array[MySQLExplain_NestedLoopObj]
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain_OrderingOperation with size_of(self) {
  let mut size = 0U
  size += 1U + @protobuf.size_of(self.using_filesort)
  for k, v in self.cost_info {
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    size += 1U +
      @protobuf.size_of(key_size + value_size) +
      key_size +
      value_size
  }
  size += 1U +
    {
      let size = @protobuf.size_of(self.table)
      @protobuf.size_of(size) + size
    }
  for s in self.nested_loop {
    let s = @protobuf.size_of(s)
    size += 1U + @protobuf.size_of(s) + s
  }
  size
}

///|
pub impl Default for MySQLExplain_OrderingOperation with default() -> MySQLExplain_OrderingOperation {
  MySQLExplain_OrderingOperation::{
    using_filesort: Bool::default(),
    cost_info: {},
    table: MySQLExplain_Table::default(),
    nested_loop: [],
  }
}

///|
pub fn MySQLExplain_OrderingOperation::new(
  using_filesort : Bool,
  cost_info : Map[String, String],
  table : MySQLExplain_Table,
  nested_loop : Array[MySQLExplain_NestedLoopObj],
) -> MySQLExplain_OrderingOperation {
  MySQLExplain_OrderingOperation::{
    using_filesort,
    cost_info,
    table,
    nested_loop,
  }
}

///|
pub impl @protobuf.Read for MySQLExplain_OrderingOperation with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain_OrderingOperation raise {
  let msg = MySQLExplain_OrderingOperation::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) => msg.using_filesort = reader |> @protobuf.read_bool()
        (2, _) => {
          let { key, value } = (
            reader |> @protobuf.read_message() :
            MySQLExplain_OrderingOperation_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (3, _) =>
          msg.table = (reader |> @protobuf.read_message() : MySQLExplain_Table)
        (4, _) =>
          msg.nested_loop.push(
            (reader |> @protobuf.read_message() : MySQLExplain_NestedLoopObj),
          )
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
pub impl @protobuf.Write for MySQLExplain_OrderingOperation with write(
  self : MySQLExplain_OrderingOperation,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(8UL)
  writer |> @protobuf.write_bool(self.using_filesort)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.write_varint(18UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.write_uint32(key_size + value_size)
    writer |> @protobuf.write_varint(10UL)
    writer |> @protobuf.write_string(k)
    writer |> @protobuf.write_varint(18UL)
    writer |> @protobuf.write_string(v)
  }
  writer |> @protobuf.write_varint(26UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.table))
  @protobuf.Write::write(self.table, writer)
  for item in self.nested_loop {
    writer |> @protobuf.write_varint(34UL)
    writer |> @protobuf.write_uint32(@protobuf.size_of(item))
    @protobuf.Write::write(item, writer)
  }
}

///|
pub impl ToJson for MySQLExplain_OrderingOperation with to_json(self) {
  let json : Map[String, Json] = {}
  if self.using_filesort != Default::default() {
    json["usingFilesort"] = self.using_filesort.to_json()
  }
  if self.cost_info != Default::default() {
    json["costInfo"] = self.cost_info.to_json()
  }
  if self.table != Default::default() {
    json["table"] = self.table.to_json()
  }
  if self.nested_loop != Default::default() {
    json["nestedLoop"] = self.nested_loop.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain_OrderingOperation with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain_OrderingOperation raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError(
      (path, "Expected an object for MySQLExplain_OrderingOperation"),
    )
  }
  let message = MySQLExplain_OrderingOperation::default()
  for key, value in obj {
    match (key, value) {
      ("usingFilesort", value) =>
        message.using_filesort = @json.from_json(value, path~)
      ("costInfo", _) => message.cost_info = @json.from_json(value, path~)
      ("table", value) => message.table = @json.from_json(value, path~)
      ("nestedLoop", Array(value)) =>
        message.nested_loop = value.map(v => @json.from_json(v, path~))
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain_OrderingOperation with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain_OrderingOperation raise {
  let msg = MySQLExplain_OrderingOperation::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) => msg.using_filesort = reader |> @protobuf.async_read_bool()
        (2, _) => {
          let { key, value } = (
            reader |> @protobuf.async_read_message() :
            MySQLExplain_OrderingOperation_CostInfoEntry)
          msg.cost_info[key] = value
        }
        (3, _) =>
          msg.table = (
            reader |> @protobuf.async_read_message() : MySQLExplain_Table)
        (4, _) =>
          msg.nested_loop.push(
            (
              reader |> @protobuf.async_read_message() :
              MySQLExplain_NestedLoopObj),
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
pub impl @protobuf.AsyncWrite for MySQLExplain_OrderingOperation with write(
  self : MySQLExplain_OrderingOperation,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(8UL)
  writer |> @protobuf.async_write_bool(self.using_filesort)
  let keys = self.cost_info.keys().collect()
  for i in 0..<keys.length() {
    let k = keys[i]
    let v = self.cost_info.get(k).unwrap()
    writer |> @protobuf.async_write_varint(18UL)
    let key_size = 1U +
      {
        let size = @protobuf.size_of(k)
        @protobuf.size_of(size) + size
      }
    let value_size = 1U +
      {
        let size = @protobuf.size_of(v)
        @protobuf.size_of(size) + size
      }
    writer |> @protobuf.async_write_uint32(key_size + value_size)
    writer |> @protobuf.async_write_varint(10UL)
    writer |> @protobuf.async_write_string(k)
    writer |> @protobuf.async_write_varint(18UL)
    writer |> @protobuf.async_write_string(v)
  }
  writer |> @protobuf.async_write_varint(26UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.table))
  @protobuf.AsyncWrite::write(self.table, writer)
  for item in self.nested_loop {
    writer |> @protobuf.async_write_varint(34UL)
    writer |> @protobuf.async_write_uint32(@protobuf.size_of(item))
    @protobuf.AsyncWrite::write(item, writer)
  }
}

///|
pub(all) struct MySQLExplain {
  mut query_block : MySQLExplain_QueryBlock
} derive(Eq, Show)

///|
pub impl @protobuf.Sized for MySQLExplain with size_of(self) {
  let mut size = 0U
  size += 1U +
    {
      let size = @protobuf.size_of(self.query_block)
      @protobuf.size_of(size) + size
    }
  size
}

///|
pub impl Default for MySQLExplain with default() -> MySQLExplain {
  MySQLExplain::{ query_block: MySQLExplain_QueryBlock::default() }
}

///|
pub fn MySQLExplain::new(query_block : MySQLExplain_QueryBlock) -> MySQLExplain {
  MySQLExplain::{ query_block, }
}

///|
pub impl @protobuf.Read for MySQLExplain with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.Reader],
) -> MySQLExplain raise {
  let msg = MySQLExplain::default()
  try {
    for {
      match (reader |> @protobuf.read_tag()) {
        (1, _) =>
          msg.query_block = (
            reader |> @protobuf.read_message() : MySQLExplain_QueryBlock)
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
pub impl @protobuf.Write for MySQLExplain with write(
  self : MySQLExplain,
  writer : &@protobuf.Writer,
) -> Unit raise {
  writer |> @protobuf.write_varint(10UL)
  writer |> @protobuf.write_uint32(@protobuf.size_of(self.query_block))
  @protobuf.Write::write(self.query_block, writer)
}

///|
pub impl ToJson for MySQLExplain with to_json(self) {
  let json : Map[String, Json] = {}
  if self.query_block != Default::default() {
    json["queryBlock"] = self.query_block.to_json()
  }
  Json::object(json)
}

///|
pub impl @json.FromJson for MySQLExplain with from_json(
  json : Json,
  path : @json.JsonPath,
) -> MySQLExplain raise {
  guard json is Object(obj) else {
    raise @json.JsonDecodeError((path, "Expected an object for MySQLExplain"))
  }
  let message = MySQLExplain::default()
  for key, value in obj {
    match (key, value) {
      ("queryBlock", value) =>
        message.query_block = @json.from_json(value, path~)
      key => raise @json.JsonDecodeError((path, "Unknown field \{key}"))
    }
  }
  message
}

///|
pub impl @protobuf.AsyncRead for MySQLExplain with read_with_limit(
  reader : @protobuf.LimitedReader[&@protobuf.AsyncReader],
) -> MySQLExplain raise {
  let msg = MySQLExplain::default()
  try {
    for {
      match (reader |> @protobuf.async_read_tag()) {
        (1, _) =>
          msg.query_block = (
            reader |> @protobuf.async_read_message() : MySQLExplain_QueryBlock)
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
pub impl @protobuf.AsyncWrite for MySQLExplain with write(
  self : MySQLExplain,
  writer : &@protobuf.AsyncWriter,
) -> Unit raise {
  writer |> @protobuf.async_write_varint(10UL)
  writer |> @protobuf.async_write_uint32(@protobuf.size_of(self.query_block))
  @protobuf.AsyncWrite::write(self.query_block, writer)
}
