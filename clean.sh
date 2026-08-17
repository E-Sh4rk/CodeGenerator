#!/bin/sh
set -e
cd "$(dirname "$0")"

if command -v opam >/dev/null 2>&1; then
  eval "$(opam env --switch=5.3.0 2>/dev/null)" || true
  if command -v dune >/dev/null 2>&1; then
    make clean
  fi
  if opam switch list --short 2>/dev/null | grep -qx 5.3.0; then
    opam switch remove -y 5.3.0
  fi
fi

rm -rf _build _opam
rm -f html/ace_js.bc.js html/scripts/seed/seed_tools_js.bc.js
