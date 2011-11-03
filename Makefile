.PHONY: all compile clean eunit get-deps

all: compile

get-deps:
	rebar get-deps

compile:
	rebar compile

eunit:
	rebar eunit

clean:
	rebar clean

console:
	erl -pa deps/proper/ebin ebin

