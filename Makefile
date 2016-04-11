PWD=$(shell pwd)

all: compile

install: compile
	ln -sf $(PWD)/_build/default/bin/effi /usr/local/bin/effi

compile:
	rebar3 escriptize

dev:
	rebar3 do escriptize, eunit dialyzer, cover, edoc

clean:
	rm -rf .rebar
	rm -rf _build
	rm -rf doc
	rm -f rebar.lock
