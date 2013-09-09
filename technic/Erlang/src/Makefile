.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = area_server0 area_server1 hello kvs

all: compile
	${ERL} -pa '~/source/life/docs/Erlang' -s hello start

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump


