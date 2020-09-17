# Erlang
ERLC:=$(shell which erlc)
ERLC_FLAGS:=-I ../.. +debug_info -Ddebug
ERL_HEADERS:=$(wildcard *.hrl) $(wildcard ../../*/include/*.hrl)
ERL_SOURCES:=$(wildcard *.erl)
ERL_OBJECTS:=$(ERL_SOURCES:%.erl=../ebin/%.beam)
ALL_OBJECTS:=$(ERL_OBJECTS)

all: $(ALL_OBJECTS)

$(ERL_OBJECTS): $(ERL_HEADERS)

clean:
	rm -f $(ALL_OBJECTS) *.core

../ebin/%.beam: %.erl
	env SCOTTI_LIB=$(SCOTTI_LIB) $(ERLC) $(ERLC_FLAGS) -o ../ebin $<
