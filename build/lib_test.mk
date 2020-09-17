# Erlang
ERLC:=$(shell which erlc)
ERLC_FLAGS:=-Werror -I ../.. +debug_info -Ddebug
ERL_HEADERS:=$(wildcard *.hrl) $(wildcard ../../*/include/*.hrl)
ERL_SOURCES:=$(wildcard *.erl)
ERL_OBJECTS:=$(ERL_SOURCES:%.erl=%.beam)
ALL_OBJECTS:=$(ERL_OBJECTS)

all: $(ALL_OBJECTS)

$(ERL_OBJECTS): $(ERL_HEADERS)

clean:
	rm -f $(ALL_OBJECTS) *.core

%.beam: %.erl
	env SCOTTI_LIB=$(SCOTTI_LIB) $(ERLC) $(ERLC_FLAGS) $<
