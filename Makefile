
all: build run

build:
	dune build ace.exe

run:
	dune exec ./ace.exe

clean:
	dune clean
