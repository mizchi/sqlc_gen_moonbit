#!/bin/sh
set -eu

if command -v pg_config >/dev/null 2>&1; then
  inc_dir=$(pg_config --includedir)
  lib_dir=$(pg_config --libdir)
  export CPPFLAGS="-I${inc_dir}"
  export C_INCLUDE_PATH="${inc_dir}"
  export CPATH="${inc_dir}"
  export LDFLAGS="-L${lib_dir}"
  export LIBRARY_PATH="${lib_dir}"
  if [ -d "${lib_dir}/pkgconfig" ]; then
    export PKG_CONFIG_PATH="${lib_dir}/pkgconfig"
  fi
elif command -v pkg-config >/dev/null 2>&1 && pkg-config --exists libpq; then
  cppflags=$(pkg-config --cflags libpq)
  libflags=$(pkg-config --libs-only-L libpq)
  inc_dirs=$(pkg-config --cflags-only-I libpq | sed 's/-I//g' | tr ' ' ':')
  lib_dirs=$(pkg-config --libs-only-L libpq | sed 's/-L//g' | tr ' ' ':')
  export CPPFLAGS="${cppflags}"
  export C_INCLUDE_PATH="${inc_dirs}"
  export CPATH="${inc_dirs}"
  export LDFLAGS="${libflags}"
  export LIBRARY_PATH="${lib_dirs}"
else
  echo "libpq not found: install PostgreSQL dev headers or set PKG_CONFIG_PATH for libpq." >&2
  exit 1
fi

exec "$@"
