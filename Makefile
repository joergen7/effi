SRC=bash effi effi_interact effi_script perl python r lib_refactor
INC=effi
PWD=$(shell pwd)

all: _build/default/bin/effi

install: _build/default/bin/effi
	ln -sf $(PWD)/_build/default/bin/effi /usr/local/bin/effi

_build/default/bin/effi: $(SRC:%=src/%.erl) $(INC:%=include/%.hrl)
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	rm -rf doc
	rm -f rebar.lock
