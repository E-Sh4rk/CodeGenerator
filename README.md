# CodeGenerator

The ACE code generator is available [here](https://e-sh4rk.github.io/CodeGenerator/).

If you want to contribute by adding an ACE code to the generator, please make a pull request on [this repository](https://github.com/E-Sh4rk/EmeraldACE_web).

## Requirements

- [opam](https://opam.ocaml.org/doc/Install.html)

## Build

```bash
./setup.sh
```

This creates a **local** opam switch in `_opam/` (OCaml 5.3.0), installs dependencies, and builds the native and JavaScript targets. A local switch keeps the compiler and packages inside this repository and does not change your global opam setup.

From this directory, later rebuilds use that local switch:

```bash
make
make js
make seedjs
```

If the build is broken or you want a fresh install, wipe the local switch and `_build`, then set up again:

```bash
./clean.sh
./setup.sh
```
