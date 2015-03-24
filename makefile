.PHONY: all compile test clean

all: compile

deps:
	cp -r priv/ deps
	rebar get-deps

compile:
	rebar compile

test: compile
	rebar skip_deps=true eunit

clean:
	rebar clean
