// Atlas configuration for D1 migrations

env "local" {
  // Use SQLite for D1 compatibility
  src = "file://db/schema.sql"
  dev = "sqlite://dev?mode=memory"

  migration {
    dir = "file://db/migrations"
    format = atlas
  }
}

env "d1" {
  // Use SQLite for D1 compatibility
  src = "file://db/schema.sql"
  dev = "sqlite://dev?mode=memory"

  migration {
    dir    = "file://db/migrations"
    format = atlas
  }
}
