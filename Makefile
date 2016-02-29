all: .rebar/effi_18.2.1_plt
	rebar co eu dialyze

clean:
	rebar clean

.rebar/effi_18.2.1_plt:
	rebar get-deps build-plt
