.PHONY: all compile clean eunit

all: compile

compile:
	rebar compile

eunit:
	rebar eunit

clean:
	rebar clean
