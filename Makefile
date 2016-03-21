all:
	rebar3 compile

dev:
	rebar3 do escriptize, eunit dialyzer, cover

clean:
	rebar3 clean
