#!/bin/sh
set -e
cd "$(dirname "$0")"

if ! command -v opam >/dev/null 2>&1; then
  echo "Install opam first: https://opam.ocaml.org/doc/Install.html" >&2
  exit 1
fi

if [ ! -d "${OPAMROOT:-$HOME/.opam}" ]; then
  opam init -y --bare
fi

if [ ! -d _opam ]; then
  opam switch create . 5.3.0
fi
eval "$(opam env --switch=. --set-switch)"

opam install -y dune menhir zarith zarith_stubs_js js_of_ocaml-compiler js_of_ocaml-ppx

make build
make js
make seedjs
