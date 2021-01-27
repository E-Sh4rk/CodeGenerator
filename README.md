# EmeraldACE

## Build instructions

```
sudo apt install opam
opam init
eval `opam config env`
opam switch create 4.11.1
opam install dune ppx_deriving num
make
```
