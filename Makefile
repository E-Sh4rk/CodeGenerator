
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

alld: buildd rund

buildd:
	dune build pkmn_data.exe

rund:
	dune exec ./pkmn_data.exe

djs:
	dune build pkmn_data_js.bc.js

clean:
	dune clean
