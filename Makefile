
all: build run

build:
	dune build ace.exe

run:
	dune exec ./ace.exe

allp: buildp runp

buildp:
	dune build ace_p.exe

runp:
	dune exec ./ace_p.exe

js:
	dune build ace_js.bc.js

clean:
	dune clean
