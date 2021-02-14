
all: build run

build:
	dune build ace.exe

run:
	dune exec ./ace.exe

save: builds runs

builds:
	dune build save_edit.exe

runs:
	dune exec ./save_edit.exe

portable: buildp runp

buildp:
	dune build ace_p.exe

runp:
	dune exec ./ace_p.exe

js:
	dune build ace_js.bc.js

data: buildd rund

buildd:
	dune build pkmn_data.exe

rund:
	dune exec ./pkmn_data.exe

djs:
	dune build pkmn_data_js.bc.js

clean:
	dune clean
