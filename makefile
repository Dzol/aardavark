.PHONY: all compile test clean deps

all: compile

deps:
	cp -R priv/rabbit_common/ deps/rabbit_common/
	cp -R priv/amqp_client/ deps/amqp_client/
	rebar get-deps

compile:
	rebar compile

test: compile
	rebar skip_deps=true eunit

clean:
	rebar clean
