About the benchmarks
--------------------

The benchmark setup is very similar to the benchmarks found in the
protobuf's subversion repository.

Deviations from the benchmarks in protobuf's subversion repository:

* The google_size.proto and the google_speed.proto have been
  fused into msg.proto since gpb doesn't know how to optimize
  neither for speed nor for code size.
* The "repeated group" construction has been changed into a
  repeated (sub) message, since groups are not supported by gpb.
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
   Alternatively, build with HiPE:
   $ make HIPE=true

3. Run the benchmarks:
   $ make benchmark

   This will call the proto-bench escript. Arguments are given in
   triples - the first is the module name; the second is the message
   name, the third is the filename. For example:
   $ ./proto-bench \ msg Message1 google_message1.dat \
                     msg Message2 google_message2.dat

4. Wait! Each test runs for around 30--35 seconds, and there are 2 tests
   per msg/data combination. The above command will take about
   4.5 -- 5 minutes to run.
