
all: build run

build:
	dune build Main/ace.exe

run:
	dune exec ./Main/ace.exe

test: buildt runt

buildt:
	dune build Main/test.exe

runt:
	dune exec ./Main/test.exe

save: builds runs

builds:
	dune build Main/save_edit.exe

runs:
	dune exec ./Main/save_edit.exe

portable: buildp runp

buildp:
	dune build Main/ace_p.exe

runp:
	dune exec ./Main/ace_p.exe

js:
	dune build --profile release Main/ace_js.bc.js
	cp -f _build/default/Main/ace_js.bc.js html/ace_js.bc.js

data: buildd rund

buildd:
	dune build Main/pkmn_data.exe

rund:
	dune exec ./Main/pkmn_data.exe

datajs:
	dune build Main/pkmn_data_js.bc.js

seed: buildseed runseed

buildseed:
	dune build Main/seed_tools.exe

runseed:
	dune exec ./Main/seed_tools.exe

seedjs:
	dune build --profile release Main/seed_tools_js.bc.js
	cp -f _build/default/Main/seed_tools_js.bc.js html/scripts/seed/seed_tools_js.bc.js

clean:
	dune clean
