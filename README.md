# CodeGenerator

The ACE code generator is available [here](https://e-sh4rk.github.io/CodeGenerator/).

If you want to contribute by adding an ACE code to the generator, please make a pull request on [this repository](https://github.com/E-Sh4rk/EmeraldACE_web).

## Build instructions

```
sudo apt install opam
opam init
eval `opam config env`
opam switch create 4.14.1
opam install dune ppx_deriving num.1.4
make
```

## Build to Javascript instructions

```
opam install js_of_ocaml-compiler js_of_ocaml-ppx
make js
```
