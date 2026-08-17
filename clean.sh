#!/bin/sh
set -e
cd "$(dirname "$0")"

if command -v opam >/dev/null 2>&1 && [ -d _opam ]; then
  eval "$(opam env --switch=. --set-switch 2>/dev/null)" || true
  if command -v dune >/dev/null 2>&1; then
    make clean
  fi
  opam switch remove -y . || true
fi

rm -rf _build _opam
rm -f html/ace_js.bc.js html/scripts/seed/seed_tools_js.bc.js
