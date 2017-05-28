About the benchmarks
--------------------

The benchmark setup is very similar to the benchmarks found in the
protobuf's subversion repository.

Deviations from the benchmarks in protobuf's subversion repository:

* The google_size.proto and the google_speed.proto have been
  fused into msg.proto since gpb doesn't know how to optimize
  neither for speed nor for code size.
* The "repeated group" construction has been changed into a
  repeated (sub) message, since groups were not supported by gpb originally.
  The google_message2.dat file has been updated accordingly.
* Addition of the d.proto and messages in d-msgs/ This contains
  randomly generated messages according to d.proto.
  The d-msgs/d-all-concatenated.dat will cause a lot of message
  merging to happen upon deserialization.

Running a benchmark
-------------------

1. Build the gpb itself (in the top-directory of gpb)
   $ rebar compile

2. Build the benchmarking code (in this benchmarks directory)
   $ make

3. Run the benchmarks:
   $ make benchmarks

   This will call the proto-bench escript. Arguments are given in
   triples - the first is the module name; the second is the message
   name, the third is the filename. For example:
   $ ./proto-bench \ msg Message1 google_message1.dat \
                     msg Message2 google_message2.dat

   There are currently some alternative benchmarks one can run:

   - make maps-benchmarks

     This runs the same benchmarks (the google messages) with code
     compiled for maps with maps_unset_optional = omitted

   - make erl-benchamrks

     Benchmark the google messages using records (suffix = _r), and
     twice with maps; once with maps_unset_optional = omitted
     (suffix = _mo) once with maps_unset_optional = present_undefined
     (suffix = _mp) thus, the target "benchmarks" and "maps-benchmarks"
     run subsets of this target.

   - make nif-benchmarks

     Benchmark the google messages using the nif bindings to Google's
     libprotobuf

   To run the benchmarks with HiPE-compiled code, set the HIPE
   variable, for example like this:

     make HIPE=1 erl-benchmarks

   Please note that things here might change as development shifts,
   for instance target names and variables, these names are super stable.

4. Wait! Each test runs for around 30--35 seconds, and there are 2 tests
   per msg/data combination. The above command will take about
   4.5 -- 5 minutes to run.
