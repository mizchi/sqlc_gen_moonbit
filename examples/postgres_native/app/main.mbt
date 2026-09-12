///|
async fn create_user(client : @client.Client) -> Int {
  let name = "Elon Musk"
  let uuid_var = @uuid.from_hex("019e092f-c489-7475-bbec-b615291e14b7") |> Some
  let timestampz_var = @time.ZonedDateTime::of(
      2017,
      11,
      8,
      hour=16,
      minute=20,
      second=14,
      nanosecond=0,
      zone=@time.utc_zone,
    )
    |> Some
  let timestamp_var = @time.PlainDateTime::of(
      2017,
      11,
      8,
      hour=16,
      minute=20,
      second=14,
    )
    |> Some
  let date_var = @time.PlainDate::of(2020, 5, 6) |> Some
  let time_var = @time.PlainTime::of(20, 15, 6, 0) |> Some
  let interval_var = @pghelpers.Interval::of(
      @time.Period::of(months=3, days=20),
      @time.Duration::of(hours=5, minutes=20, seconds=0),
    )
    |> Some
  let timetz_var = @pghelpers.TimeTz::of(
      @time.PlainTime::of(20, 15, 6, 0),
      @time.ZoneOffset::of(hours=5, minutes=30),
    )
    |> Some
  let bool_var = Some(true)
  let int_var = Some(16)
  let int2_var = Some(16)
  let smallint_var = Some(16)
  let pg_int8_var = Some(Int64::from_int(64))
  let int64_var = Some(Int64::from_int(64))
  let bigserial_var = Some(Int64::from_int(6400))
  let json_var = "{\"name\": \"Elon Musk\"}".to_json() |> Some
  let jsonb_var = "{\"name\": \"Jeff Bezos\"}".to_json() |> Some
  let bytes_var = b"\x00\x00\x00\x02" |> Some
  let float_var = Float::from_int(3) |> Some
  let double_var = Double::from_int(3) |> Some
  let numeric_var = @decimal.Decimal::from_string("123.45")
  @sql.create_user(
    client,
    name~,
    date_var~,
    time_var~,
    timestamp_var~,
    timestampz_var~,
    uuid_var~,
    bool_var~,
    bigserial_var~,
    int64_var~,
    pg_int8_var~,
    int_var~,
    interval_var~,
    timetz_var~,
    smallint_var~,
    int2_var~,
    json_var~,
    jsonb_var~,
    bytes_var~,
    float_var~,
    double_var~,
    numeric_var~,
  )
}

///|
async fn create_user_nn(client : @client.Client) -> Int {
  let name = "Elon Musk"
  let uuid_var = @uuid.from_hex("019e092f-c489-7475-bbec-b615291e14b7")
  let timestampz_var = @time.ZonedDateTime::of(
    2017,
    11,
    8,
    hour=16,
    minute=20,
    second=14,
    nanosecond=0,
    zone=@time.utc_zone,
  )
  let timestamp_var = @time.PlainDateTime::of(
    2017,
    11,
    8,
    hour=16,
    minute=20,
    second=14,
  )
  let date_var = @time.PlainDate::of(2020, 5, 6)
  let time_var = @time.PlainTime::of(20, 15, 6, 0)
  let interval_var = @pghelpers.Interval::of(
    @time.Period::of(months=3, days=20),
    @time.Duration::of(hours=5, minutes=20, seconds=0),
  )
  let timetz_var = @pghelpers.TimeTz::of(
    @time.PlainTime::of(20, 15, 6, 0),
    @time.ZoneOffset::of(hours=5, minutes=30),
  )
  let bool_var = true
  let int_var = 16
  let int2_var = 16
  let smallint_var = 16
  let pg_int8_var = Int64::from_int(64)
  let int64_var = Int64::from_int(64)
  let bigserial_var = Int64::from_int(6400)
  let json_var = "{\"name\": \"Elon Musk\"}".to_json()
  let jsonb_var = "{\"name\": \"Jeff Bezos\"}".to_json()
  let bytes_var = b"\x00\x00\x00\x02"
  let float_var = Float::from_int(3)
  let double_var = Double::from_int(3)
  let numeric_var = match @decimal.Decimal::from_string("123.45") {
    Some(decimal) => decimal
    None => fail("Invalid decimal")
  }

  @sql.create_user_nn(
    client,
    name~,
    date_var~,
    time_var~,
    timestamp_var~,
    timestampz_var~,
    uuid_var~,
    bool_var~,
    bigserial_var~,
    int64_var~,
    pg_int8_var~,
    int_var~,
    interval_var~,
    timetz_var~,
    smallint_var~,
    int2_var~,
    json_var~,
    jsonb_var~,
    bytes_var~,
    float_var~,
    double_var~,
    numeric_var~,
  )
}

///|
async fn main {
  println("PostgreSQL example with sqlc-gen-moonbit")
  println("Generated types and queries are in db/gen/")

  let config = @client.Config::new(
    "localhost",
    port=5432,
    user="postgres",
    password="postgres",
    database="testdb",
    ssl_mode=@client.SslMode::Disable,
    application_name="my-service",
  )

  @async.with_task_group(group => {
    let (client, connection) = @client.connect(config)

    group.spawn_bg(() => connection.run())

    let current_user : String = client
      .query_one("select current_user::text as current_user")
      .get_name("current_user")
    println("Current_user : " + current_user)

    let serial = create_user(client)
    if serial > 0 {
      println("Inserted \{serial} serial")
    } else {
      panic()
    }

    let row = @sql.get_user(client, id=serial)
    if row.id == serial {
      println("Select by ID - \{Repr(row)}")
    } else {
      panic()
    }

    let rows = @sql.get_by_date(
      client,
      date_var=@time.PlainDate::of(2020, 5, 6) |> Some,
    ) catch {
      err => {
        println("Get by Date failed - " + err.to_string())
        []
      }
    }
    if rows.length() > 0 {
      println("Select by date - Found \{rows.length()} rows")
    } else {
      panic()
    }

    let rows = @sql.get_by_uuid(
      client,
      uuid_var=Some(@uuid.from_hex("019e092f-c489-7475-bbec-b615291e14b7")),
    ) catch {
      err => {
        println("Get by UUID failed - " + err.to_string())
        []
      }
    }
    if rows.length() > 0 {
      println("Select by UUID: Found \{rows.length()} rows")
    } else {
      panic()
    }
    let rows = @sql.list_users(client)
    if rows.length() > 0 {
      println("Found \{rows.length()} rows")
      for row in rows {
        println("Select row \{Repr(row)}")
      }
    } else {
      panic()
    }
    let rows = @sql.delete_user(client, id=serial)
    if rows > 0 {
      println("Deleted \{rows} rows")
    } else {
      panic()
    }

    println(
      "\n" + String::from_array(['-'].repeat(60)) + " Not Null queries \n",
    )

    let serial = create_user_nn(client)
    if serial > 0 {
      println("Inserted \{serial} serial")
    } else {
      panic()
    }
    let row = @sql.get_user_nn(client, id=serial)
    if row.id == serial {
      println("Select by ID - \{Repr(row)}")
    } else {
      panic()
    }
    let rows = @sql.get_by_date_nn(
      client,
      date_var=@time.PlainDate::of(2020, 5, 6),
    ) catch {
      err => {
        println("Get by Date failed - " + err.to_string())
        []
      }
    }
    if rows.length() > 0 {
      for row in rows {
        println("Select by date: \{Repr(row)}")
      }
    } else {
      panic()
    }

    let rows = @sql.get_by_uuid_nn(
      client,
      uuid_var=@uuid.from_hex("019e092f-c489-7475-bbec-b615291e14b7"),
    ) catch {
      err => {
        println("Get by UUID failed - " + err.to_string())
        []
      }
    }
    if rows.length() > 0 {
      for row in rows {
        println("Select by UUID: \{Repr(row)}")
      }
    } else {
      panic()
    }
    let rows = @sql.list_users_nn(client)
    if rows.length() > 0 {
      println("Found \{rows.length()} rows")
      for row in rows {
        println("Select row \{Repr(row)}")
      }
    } else {
      panic()
    }

    let rows = @sql.delete_user_nn(client, id=serial)
    if rows > 0 {
      println("Deleted \{rows} rows")
    } else {
      panic()
    }
    client.close()
  })
}
