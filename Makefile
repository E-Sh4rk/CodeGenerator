
all: build run

build:
	dune build ace.exe

run:
	dune exec ./ace.exe

test: buildt runt

buildt:
	dune build test.exe

runt:
	dune exec ./test.exe

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

datajs:
	dune build pkmn_data_js.bc.js

encoder: builde rune

builde:
	dune build encoder.exe

rune:
	dune exec ./encoder.exe

encoderjs:
	dune build encoder_js.bc.js

seed: buildseed runseed

buildseed:
	dune build seed_tools.exe

runseed:
	dune exec ./seed_tools.exe

seedjs:
	dune build seed_tools_js.bc.js

clean:
	dune clean
