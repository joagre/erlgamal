ERL:=$(shell readlink -f `which erl`)
ERL_TOP:=$(ERL:%/bin/erl=%)
OS:=$(shell uname -s)
CFLAGS:=-I$(ERL_TOP)/usr/include
