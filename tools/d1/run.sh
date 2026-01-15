#!/bin/sh
set -eu

action=${1:-build}
name=${2:-}

case "$action" in
  build)
    cd examples/d1-basic && moon build --target js
    ;;
  dev)
    cd examples/d1-basic && moon build --target js && npx wrangler dev
    ;;
  migrate-diff)
    if [ -z "$name" ]; then
      echo "migrate-diff requires name=..." >&2
      exit 1
    fi
    cd examples/d1-basic && atlas migrate diff "$name" --env local
    ;;
  migrate-apply)
    cd examples/d1-basic && npx wrangler d1 migrations apply blog-db --local
    ;;
  migrate-apply-remote)
    cd examples/d1-basic && npx wrangler d1 migrations apply blog-db --remote
    ;;
  migrate-status)
    cd examples/d1-basic && atlas migrate status --env local
    ;;
  migrate-lint)
    cd examples/d1-basic && atlas migrate lint --env local
    ;;
  migrate-hash)
    cd examples/d1-basic && atlas migrate hash --env local
    ;;
  *)
    echo "unknown action: $action" >&2
    exit 1
    ;;
 esac
