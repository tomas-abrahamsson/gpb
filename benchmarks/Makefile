erl     ?= erl
erlc    ?= erlc
escript ?= escript

ifdef HIPE
  HIPE_ERLC_OPTS := +'native' +'{hipe,[o3]}'
else
  HIPE_ERLC_OPTS :=
endif

PROTOBUF_CFLAGS  ?= $(shell pkg-config --cflags protobuf)
PROTOBUF_LDFLAGS ?= $(shell pkg-config --libs protobuf)
PROTOC           ?= $(shell which protoc)
ifdef PROTOHOME
  PROTOBUF_CFLAGS  := "-I$(PROTOHOME)/src"
  PROTOBUF_LIBS    := $(PROTOHOME)/src/.libs
  PROTOBUF_LDFLAGS := "-Wl,-rpath=$(PROTOBUF_LIBS)" "-L$(PROTOBUF_LIBS)" -lprotobuf -pthread -lpthread
  PROTOC := "$(PROTOHOME)/src/protoc"
else
  PROTOBUF_CFLAGS  ?= "-I/usr/local/src/protobuf/src"
  PROTOBUF_LIBS    ?= /usr/local/src/protobuf/src/.libs
  PROTOBUF_LDFLAGS ?= "-Wl,-rpath=$(PROTOBUF_LIBS)" "-L$(PROTOBUF_LIBS)" -lprotobuf -pthread -lpthread
endif


all:	benchmarks-erl-code

benchmarks-erl-code: tmp/msg_r.beam tmp/d_r.beam \
		tmp/msg_mo.beam tmp/d_mo.beam \
		tmp/msg_mp.beam tmp/d_mp.beam

benchmarks-gr-code: tmp/msg_r.beam

benchmarks-maps-mo-code: tmp/msg_mo.beam

benchmarks-nif-code: tmp/msg.beam tmp/msg.nif.so \
		tmp/d.beam tmp/d.nif.so

show-hw-sw-info = \
	echo CPU info; \
	egrep '^model name' /proc/cpuinfo | head -1; \
	egrep '^cache' /proc/cpuinfo | head -1; \
	printf 'cores/threads   : %s\n' `egrep -c '^processor' /proc/cpuinfo`; \
	egrep '^bogomips' /proc/cpuinfo | head -1; \
	echo; \
	$(erl) +V

# Args:
#  $1  - module suffix
#  $2  - any options
#  $3  - proto file name
gpb-compile = \
	$(erl) -boot start_clean -pa ../ebin -noshell -noinput +B \
		-v never -c false -I`pwd` -o tmp \
		-modsuffix $1 \
		$(2) \
		-s gpb_compile c $(3)

tmp/%_r.erl: %.proto
	[ -d tmp ] || mkdir tmp
	$(call gpb-compile,_r,,$<)

tmp/%_mo.erl: %.proto
	[ -d tmp ] || mkdir tmp
	$(call gpb-compile,_mo,-maps -maps_unset_optional omitted,$<)

tmp/%_mp.erl: %.proto
	[ -d tmp ] || mkdir tmp
	$(call gpb-compile,_mp,-maps -maps_unset_optional present_undefined,$<)

%.beam: %.erl
	cd tmp; $(erlc) -Wall $(HIPE_ERLC_OPTS) -I../../include +debug_info \
		$(patsubst tmp/%,%,$<)

tmp/%.nif.so: %.proto
	$(PROTOC) --cpp_out=tmp/ $*.proto
	$(CXX) -g -fPIC -O3 $(PROTOBUF_CFLAGS) $(CXXFLAGS) \
	  -o tmp/$*.pb.o -c tmp/$*.pb.cc
	$(CXX) -g -fPIC -Wall -O3 $(PROTOBUF_CFLAGS) $(CXXFLAGS) \
	  -o tmp/$*.nif.o -c tmp/$*.nif.cc
	$(CXX) -g -fPIC -shared -O3 -Wall $(PROTOBUF_CFLAGS) $(CXXFLAGS) \
	  -o $@ tmp/$*.nif.o tmp/$*.pb.o $(PROTOBUF_LDFLAGS)

tmp/%.erl: %.proto
	[ -d tmp ] || mkdir tmp
	$(call gpb-compile,"",-nif,$<)

clean:
	$(RM) -r tmp

# Run only the "standard" Google benchmark messages with records
benchmarks: benchmarks-gr-code
	@$(call show-hw-sw-info)
	@echo
	./proto-bench \
	  msg_r  Message1 google_message1.dat \
	  msg_r  Message2 google_message2.dat

# Run only (one of) the maps benchmarks
maps-benchmarks: benchmarks-maps-mo-code
	@$(call show-hw-sw-info)
	@echo
	./proto-bench \
	  msg_mo Message1 google_message1.dat \
	  msg_mo Message2 google_message2.dat

# "Standard" Google benchmark messages
erl-benchmarks: benchmarks-erl-code
	@$(call show-hw-sw-info)
	@echo
	./proto-bench \
	  --echo "-- Records --" \
	  msg_r  Message1 google_message1.dat \
	  msg_r  Message2 google_message2.dat \
	  --echo "--" \
	  --echo "-- Maps (unset optionals: omitted) --" \
	  msg_mo Message1 google_message1.dat \
	  msg_mo Message2 google_message2.dat \
	  --echo "--" \
	  --echo "-- Maps (unset optionals: present, undefined) --" \
	  msg_mp Message1 google_message1.dat \
	  msg_mp Message2 google_message2.dat

# Some "d" messages
d-benchmarks: benchmarks-code
	@$(call show-hw-sw-info)
	@echo
	./proto-bench \
	  --echo "-- Records --" \
	  d_r  dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d_r  dm1 d-msgs/d-all-concatenated.dat \
	  --echo "--" \
	  --echo "-- Maps (unset optionals: omitted) --" \
	  d_mo dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d_mo dm1 d-msgs/d-all-concatenated.dat \
	  --echo "--" \
	  --echo "-- Maps (unset optionals: present, undefined) --" \
	  d_mp dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d_mp dm1 d-msgs/d-all-concatenated.dat

# Some "d" messages
nif-benchmarks: benchmarks-nif-code
	@$(call show-hw-sw-info)
	@echo
	./proto-bench \
	  msg  Message1 google_message1.dat \
	  msg  Message2 google_message2.dat \
	  d dm1 --multi d-msgs/d-msg-n*.dat --end-multi \
	  d dm1 d-msgs/d-all-concatenated.dat \
