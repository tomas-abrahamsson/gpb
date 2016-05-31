#
# Makefile for gpb -- for projects that use (GNU) make to build
#
# Copyright (C) 2013  Tomas Abrahamsson
#
# Author: Tomas Abrahamsson <tab@lysator.liu.se>
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
# MA  02110-1301  USA


# Parameters:
# ----------
# GPB_PREFIX      -- if this makefile is to be included in some larger project
#                    NB: must end with a slash!
# VERBOSE         -- set (to any value) to make the following steps verbose:
#                    * eunit testing
#                    * generation of the include/gpb_version.hrl and 
#                      ebin/gpb.app files
#                    * edoc generation command
#                    * xref checking command
# ERL             -- path to erl
# ERLC            -- path to erlc
# ERLC_FLAGS      -- any additional flags to erlc
# EDOC_DEST_DIR   -- destination directory for documentation
# EDOC_OPTS       -- any options to edoc (NB: _not_ including list brackets!)
# ERL_BATCH_FLAGS -- Flags to start the Erlang VM in batch mode


.SUFFIXES += .erl .xrl .yrl .hrl .beam .proto
.PHONY: all clean test doc xref

SHELL = /bin/sh

ERL  ?= erl
ERLC ?= erlc

GPB_PREFIX =

src            = $(GPB_PREFIX)src
ebin           = $(GPB_PREFIX)ebin
incdir         = $(GPB_PREFIX)include
descr_src      = $(GPB_PREFIX)descr_src
test           = $(GPB_PREFIX)test
doc            = $(GPB_PREFIX)doc
build          = $(GPB_PREFIX)build

empty_str :=
space := $(empty_str) $(empty_str)
comma := ,

##
ifndef EDOC_DEST_DIR
EDOC_DEST_DIR=.
endif

## Check verbosity
ifdef VERBOSE
verbose_opt := verbose
silencer    :=
else
verbose  :=
silencer := @
endif

# When making 'clean', don't run in parallel.
# Otherwise "make -j clean all" in an already-built dir will fail:
# it will clean while rebuilding, or fail to detect what needs to
# be rebuilt since it runs in parallel.
#
# If anyone tries that, revert to non-parallel execution, which
# is safe, but slower. Issue a warning about that.
ifneq ($(filter clean,$(MAKECMDGOALS)),)
.NOTPARALLEL:
ifneq ($(filter-out clean,$(MAKECMDGOALS)),)
# User is not _only_ making clean...
$(warning "WARNING: cannot make in parallel when _also_ making clean")
endif
endif


ERLC_FLAGS += -Wall +debug_info -I$(incdir)

ERL_BATCH_FLAGS = +B -noshell -noinput

ifdef NO_HAVE_MAPS
ERLC_FLAGS += -DNO_HAVE_MAPS=true
else
## attempt to auto-detect
ERLVM_SUPPORTS_MAPS := $(shell $(ERL) $(ERL_BATCH_FLAGS) -eval ' \
                             try maps:size(maps:new()) of \
                                0 -> io:format("true~n") \
                             catch error:undef -> io:format("false~n") \
                             end, \
                             receive after 10 -> ok end.' \
                         -s erlang halt)
ifeq ($(ERLVM_SUPPORTS_MAPS),false)
ERLC_FLAGS += -DNO_HAVE_MAPS=true
endif
endif


# Sorting it also eliminates duplicates
# (eg: gpb_parse due to both .yrl and .erl on rebuild, ditto for gpb_scan)
MODULES := \
	$(sort \
	  $(patsubst $(src)/%.erl,%,$(wildcard $(src)/*.erl)) \
	  $(patsubst $(src)/%.yrl,%,$(wildcard $(src)/*.yrl)) \
	  $(patsubst $(src)/%.xrl,%,$(wildcard $(src)/*.xrl)))

DESCR_PROTO := $(descr_src)/gpb_descriptor.proto

DESCR_MODULES := \
	gpb_compile_descr \
	$(patsubst $(descr_src)/%.proto,%,$(DESCR_PROTO))

TEST_MODULES := \
	$(patsubst $(test)/%.erl,%,$(wildcard $(test)/*.erl))

# Run eunit on these modules:
# - If module M and M_tests exist, only include M (M_tests is then implicit)
# - If M_tests exists, but no M, include M_tests (eg gpb_compile_maps_tests)
# sorting it also removes duplicates (gpb_parse)
EUNIT_MODULES := \
	$(MODULES) \
	$(filter-out $(patsubst %,%_tests,$(MODULES)),$(TEST_MODULES))


BEAMS       := $(patsubst %,$(ebin)/%.beam,$(MODULES))
DESCR_BEAMS := $(patsubst %,$(ebin)/%.beam,$(DESCR_MODULES))
TEST_BEAMS  := $(patsubst %,$(test)/%.beam,$(TEST_MODULES)) \
               $(test)/gpb_compile_maps_tests.beam

TARGETS = \
	$(incdir)/gpb_version.hrl \
	$(BEAMS) \
	$(DESCR_BEAMS) \
	$(ebin)/gpb.app


all:	$(TARGETS)

clean:
	$(RM) $(TARGETS)
	$(RM) $(src)/gpb_parse.erl
	$(RM) $(src)/gpb_scan.erl
	$(RM) $(patsubst %.proto,%.erl,$(DESCR_PROTO))
	$(RM) $(patsubst %.proto,%.hrl,$(DESCR_PROTO))
	$(RM) $(TEST_BEAMS)
	$(RM) -r doc

test:	all $(TEST_BEAMS) FORCE
	@echo Testing...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(test) -pa $(ebin) -eval " \
	    case eunit:test([$(subst $(space),$(comma),$(EUNIT_MODULES))], \
			    [$(verbose_opt)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

doc:	| $(src)/gpb_parse.erl $(src)/gpb_scan.erl
	@echo Generating documentation...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(ebin) -eval " \
	    case edoc:application(gpb,\"$(EDOC_DEST_DIR)\",[$(EDOC_OPTS)]) of \
		ok -> halt(0); \
		_  -> halt(1) \
	    end."

xref: all
	@echo Checking for calls to undefined functions...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -eval " \
	    Res = xref:d(\"$(ebin)\"), \
	    case lists:keyfind(undefined,1,Res) of \
		{undefined,[]}     -> halt(0); \
		{undefined,Undefs} -> io:format(\"~p~n\",[Undefs]), halt(1) \
	    end."

FORCE:

##
## General default rules for how to compile some files
##
$(ebin)/%.beam: $(src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) -pa $(ebin) -o $(ebin) $<

$(ebin)/%.beam: $(descr_src)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) -pa $(ebin) -o $(ebin) $<

$(test)/%.beam: $(test)/%.erl | $(ebin)
	$(ERLC) $(ERLC_FLAGS) $(EUNIT_ERLC_FLAGS) -pa $(ebin) -o $(test) $<

$(src)/%.erl: $(src)/%.yrl
	$(ERLC) -o $(src) $<

$(src)/%.erl: $(src)/%.xrl
	$(ERLC) -o $(src) $<

$(ebin):
	mkdir -pv $(ebin)

##
## Some extra dependencies, not covered by default rules above
##

# To compile gpb_compile, we first need the parse transform in gpb_codegen
$(ebin)/gpb_compile.beam: $(ebin)/gpb_codegen.beam

# To compile gpb_codegen_tests, we first need the parse transform in gpb_codegen
$(test)/gpb_codegen_tests.beam: $(ebin)/gpb_codegen.beam

# To compile gpb.erl, we need gpb_include.hrl
$(ebin)/gpb.beam: $(src)/gpb.erl $(incdir)/gpb_version.hrl

# gpb_compile_tests.erl includes gpb_tests.erl (see the files for details
# on this unorthodox setup), this dependency needs to be recorded
$(test)/gpb_compile_tests.beam: $(test)/gpb_compile_tests.erl \
				$(test)/gpb_tests.erl

# To compile the description generator, we
# must first have compiled the proto file for the gpb_description.proto
descr_encoder = $(patsubst %.proto,%.erl,$(DESCR_PROTO))

$(ebin)/gpb_compile_descr.beam: $(descr_src)/gpb_compile_descr.erl \
				$(descr_encoder)

$(descr_encoder): $(DESCR_PROTO) $(BEAMS)
	@echo Proto-compiling the description definition...
	$(silencer)$(ERL) $(ERL_BATCH_FLAGS) -pa $(ebin) \
		-I $(abspath $(descr_src)) \
		-o $(descr_src) \
		-s gpb_compile c $(abspath $(descr_src))/gpb_descriptor.proto

# To generate the ebin/gpb.app file, process the src/gpb.app.src file
$(ebin)/gpb.app: $(src)/gpb.app.src | $(ebin)
	@echo Generating $@...
	$(silencer)$(ERL) +B -noshell -noinput -eval " \
	    try \
		{ok, [{application,App,KVs1}]} = file:consult(\"$<\"), \
		Vsn2 = case lists:keyfind(vsn,1,KVs1) of \
			   {vsn,{cmd,Cmd}} -> \
				string:strip(os:cmd(Cmd),right,$$\n); \
			   {vsn,Vsn1} -> \
				Vsn1 \
		       end, \
		KVs2 = lists:keystore(vsn,1,KVs1,{vsn,Vsn2}), \
		AppTerm  = {application,App,KVs2}, \
		ok = file:write_file( \
		       \"$@\", \
		       iolist_to_binary( \
		         io_lib:format(\"~p.~n\", [AppTerm]))), \
		halt(0) \
	    catch Class:Reason -> \
		ST = erlang:get_stacktrace(), \
		io:format(\"ERROR: {~p,~p~n\" \
			  \"        ~p}~n\", [Class,Reason,ST]), \
		halt(1) \
	    end."

$(incdir)/gpb_version.hrl: $(incdir)/gpb_version.hrl.in
	@echo Generating $@...
	$(silencer)$(build)/mk_version_hrl \
	    < include/gpb_version.hrl.in \
	    > include/gpb_version.hrl
