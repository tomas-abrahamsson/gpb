ifdef ERL
  erl=$(ERL)
else
  erl=erl
endif

ifdef ERLC
  erlc=$(ERLC)
else
  erlc=erlc
endif

ifdef ESCRIPT
  escript=$(ESCRIPT)
else
  escript=escript
endif

ifdef HIPE
  HIPE_ERLC_OPTS = +'native' +'{hipe,[o3]}'
else
  HIPE_ERLC_OPTS =
endif

ifdef NIFS
  nif_opt = -nif
  tmp/msg.beam: tmp/msg.nif.so
  tmp/d.beam: tmp/d.nif.so
ifdef PROTOHOME
  protohome = $(PROTOHOME)
else
  protohome = /usr/local/src/protobuf
endif
ifdef PROTOC
  protoc = $(PROTOC)
else
  protoc = $(protohome)/src/protoc
endif
else
  nif_opt =
endif

ifdef MAPS
  maps_opt = -maps
else
  maps_opt =
endif

get_protolib = $(if $(shell grep 'option.*optimize_for.*LITE_RUNTIME' $(1)),protobuf-lite,protobuf)

all:	benchmarks-code

benchmarks-code: tmp/msg.beam tmp/d.beam

tmp/msg.erl: msg.proto
tmp/d.erl: d.proto

tmp/msg.beam: tmp/msg.erl
tmp/d.beam:   tmp/d.erl

tmp/%.erl: %.proto
	[ -d tmp ] || mkdir tmp
	$(erl) -boot start_clean -pa ../ebin -noshell -noinput +B \
		-v never -c false -I`pwd` -o tmp $(nif_opt) $(maps_opt) \
		-s gpb_compile c $<

tmp/%.beam: tmp/%.erl
	cd tmp; $(erlc) -Wall $(HIPE_ERLC_OPTS) -I../../include +debug_info \
		$(patsubst tmp/%,%,$<)

tmp/%.nif.so: %.proto
	$(protoc) --cpp_out=tmp/ $*.proto
	$(CXX) -g -fPIC -O3 -I$(protohome)/src -o tmp/$*.pb.o -c tmp/$*.pb.cc
	$(CXX) -g -fPIC -Wall -O3 -I$(protohome)/src \
	  -o tmp/$*.nif.o -c tmp/$*.nif.cc
	$(CXX) -g -fPIC -shared -O3 -Wall -I$(protohome)/src -o $@ \
	  tmp/$*.nif.o tmp/$*.pb.o \
	  -L$(protohome)/src/.libs -l$(call get_protolib,$*.proto) \
	  -Wl,-rpath=$(protohome)/src/.libs
clean:
	$(RM) -r tmp

benchmarks: benchmarks-code
	@echo CPU info
	@egrep '^model name' /proc/cpuinfo | head -1
	@egrep '^cache' /proc/cpuinfo | head -1
	@printf "# cores/threads : %s\n" `egrep -c '^processor' /proc/cpuinfo`
	@egrep '^bogomips' /proc/cpuinfo | head -1
	@echo
	$(erl) +V
	@echo
	./proto-bench \
          msg Message1 google_message1.dat \
          msg Message2 google_message2.dat \
	  d dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d dm1 d-msgs/d-all-concatenated.dat

