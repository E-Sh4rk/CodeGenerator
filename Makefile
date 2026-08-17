
DUNE = opam exec -- dune

all: build run

build:
	$(DUNE) build Main/ace.exe

run:
	$(DUNE) exec ./Main/ace.exe

test: buildt runt

buildt:
	$(DUNE) build Main/test.exe

runt:
	$(DUNE) exec ./Main/test.exe

save: builds runs

builds:
	$(DUNE) build Main/save_edit.exe

runs:
	$(DUNE) exec ./Main/save_edit.exe

portable: buildp runp

buildp:
	$(DUNE) build Main/ace_p.exe

runp:
	$(DUNE) exec ./Main/ace_p.exe

js:
	$(DUNE) build --profile release Main/ace_js.bc.js
	cp -f _build/default/Main/ace_js.bc.js html/ace_js.bc.js

data: buildd rund

buildd:
	$(DUNE) build Main/pkmn_data.exe

rund:
	$(DUNE) exec ./Main/pkmn_data.exe

datajs:
	$(DUNE) build Main/pkmn_data_js.bc.js

seed: buildseed runseed

buildseed:
	$(DUNE) build Main/seed_tools.exe

runseed:
	$(DUNE) exec ./Main/seed_tools.exe

seedjs:
	$(DUNE) build --profile release Main/seed_tools_js.bc.js
	cp -f _build/default/Main/seed_tools_js.bc.js html/scripts/seed/seed_tools_js.bc.js

clean:
	$(DUNE) clean

serve:
	xdg-open html/index.html >/dev/null 2>&1 || open html/index.html
