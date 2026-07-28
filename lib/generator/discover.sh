#!/bin/sh
# Emit a dune sexp list of libpq compile or link flags.
# Tries pkg-config, then pg_config, then a bare -lpq.
set -e
emit() {
  printf '('
  for a in "$@"; do printf '%s ' "$a"; done
  printf ')\n'
}

if pkg-config --exists libpq 2>/dev/null; then
  case "$1" in
    cflags) emit $(pkg-config --cflags libpq) ;;
    libs) emit $(pkg-config --libs libpq) ;;
  esac
elif command -v pg_config >/dev/null 2>&1; then
  case "$1" in
    cflags) emit -I"$(pg_config --includedir)" ;;
    libs) emit -L"$(pg_config --libdir)" -lpq ;;
  esac
else
  case "$1" in
    cflags) emit ;;
    libs) emit -lpq ;;
  esac
fi
