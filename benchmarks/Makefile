ifdef HIPE
  HIPE_ERLC_OPTS = +'native' +'{hipe,[o3]}'
else
  HIPE_ERLC_OPTS =
endif
all:	benchmarks-code

benchmarks-code: tmp/msg.beam

tmp/msg.erl: msg.proto
	[ -d tmp ] || mkdir tmp
	erl -boot start_clean -pa ../ebin -noshell -noinput +B \
		-v never -c false -I`pwd` -o tmp -s gpb_compile c $<

tmp/msg.beam: tmp/msg.erl
	cd tmp; erlc -Wall $(HIPE_ERLC_OPTS) -I../../include +debug_info msg.erl

clean:
	$(RM) -r tmp

benchmarks: benchmarks-code
	@echo CPU info
	@egrep '^model name' /proc/cpuinfo | head -1
	@egrep '^cache' /proc/cpuinfo | head -1
	@printf "# cores/threads : %s\n" `egrep -c '^processor' /proc/cpuinfo`
	@egrep '^bogomips' /proc/cpuinfo | head -1
	@echo
	erl +V
	@echo
	./proto_bench \
          msg Message1 google_message1.dat \
          msg Message2 google_message2.dat
