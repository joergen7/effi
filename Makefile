all: .rebar/effi_18.2.1_plt
	rebar co eu dialyze doc

clean:
	rebar clean

.rebar/effi_18.2.1_plt:
	rebar build-plt
