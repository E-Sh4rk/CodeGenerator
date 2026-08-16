# CodeGenerator

The ACE code generator is available [here](https://e-sh4rk.github.io/CodeGenerator/).

If you want to contribute by adding an ACE code to the generator, please make a pull request on [this repository](https://github.com/E-Sh4rk/EmeraldACE_web).

## Requirements

- [opam](https://opam.ocaml.org/doc/Install.html)
- [Node.js](https://nodejs.org/en/download)

## Build

```bash
./setup.sh
```

This creates the OCaml 5.3.0 switch, installs dependencies, and builds the native and JavaScript targets.

Later rebuilds:

```bash
make
make js
make seedjs
```

If the build is broken or you want a fresh install, wipe the OCaml 5.3.0 switch, `_build`, and `node_modules`, then set up again:

```bash
./clean.sh
./setup.sh
```

## Run locally

After `./setup.sh`:

```bash
make serve
```

Open the URL printed in the terminal (usually http://localhost:3000).

Emerald is the default page. Ruby/Sapphire is `index_rs.html`, FireRed/LeafGreen is `index_frlg.html`, and seed tools are under `scripts/seed/`.

## HTML / JS checks

```bash
npm run format
npm run lint
```

`npm install` (run by `./setup.sh`) also copies a git pre-commit hook that runs Prettier and ESLint on staged `html/` files.
