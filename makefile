.PHONY: all compile test clean deps

all: compile

deps:
	rebar get-deps

compile:
	rebar compile

test: compile
	rebar skip_deps=true eunit

clean:
	rebar clean
