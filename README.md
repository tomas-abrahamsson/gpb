The gpb is a compiler for Google protocol buffer definitions files
for Erlang.

See link: https://code.google.com/p/protobuf/ for further information on
the Google protocol buffers.

Features of gpb
---------------

*  Parses protocol buffer definition files and can generate:
   - record defintions, one record for each message
   - erlang code for encoding/decoding the messages to/from binaries

*  Features of the protocol buffer defintion files:
   gpb supports:
   - message definitions (also messages in messages)
   - scalar types
   - importing other proto files
   - nested types
   - message extensions
   - the 'packed' and 'default' options
   - generating metadata information

   gpb reads but ignores or throws away:
   - options other than 'packed' or 'default'
   - custom options
   - services
   - package information

   gpb does not support:
   - groups
   - aggregate custom options introduced in protobuf 2.4.0
   - descriptors
   - rpc

*  Characteristics of gpb:
   - Skipping over unknown message fields, when decoding, is supported
   - Merging of messages, also recursive merging, is supported
   - Gpb can optionally generate code for verification of values during
     encoding this makes it easy to catch e.g integers out of range,
     or values of the wrong type.
   - Gpb can optionally or conditionally copying the contents of 'bytes'
     fields, in order to let the runtime system free the larger message
     binary.
   - The generated encode/decoder has no run-time dependency to gpb,
     but there is a compile-time dependency for the generated code:
     to the #field{} record in gpb.hrl the for the get_msg_defs function.
   - Gpb can generate code both to files and to binaries.

*  Introspection

   gpb generates some functions for examining the messages and enums:
   - get_msg_defs(), get_msg_names(), get_enum_names()
   - find_msg_def(MsgName) and fetch_msg_def(MsgName)
   - find_eunm_def(MsgName) and fetch_enum_def(MsgName)

   There are also some version information:

   - gpb:version_as_string() and gpb:version_as_list()
   - GeneratedCode:version_as_string() and GeneratedCode:version_as_list()
   - ?gpb_version  (in gpb_version.hrl)
   - ?'GeneratedCode_gpb_version'  (in GeneratedCode.hrl)

*  Reporting of errors in .proto files

   Gpb is not very good at error reporting, especially referencing
   errors, such as references to messages that are not defined.
   You might want to first verify with `protoc' that the .proto files
   are valid before feeding them to gpb.

*  Caveats

   The gpb does accept reserved words as names for fields (just like
   protoc does), but not as names for messages. To correct this, one
   would have to either rewrite the grammar, or stop using yecc.
   (maybe rewrite it all as a protoc plugin?)

Performance
-----------

Here is a comparison between gpb (interpreted by the erlang vm) and
the C++, Python and Java serializers/deserializers of protobuf-2.4.1

    [MB/s]        | gpb   |pb/c++ |pb/c++ | pb/c++ | pb/py |pb/java| pb/java|
                  |       |(speed)|(size) | (lite) |       |(size) | (speed)|
    --------------+-------+-------+-------+--------+-------+-------+--------+
    small msgs    |       |       |       |        |       |       |        |
      serialize   | 25.17 | 164.9 | 31.25 |  206.2 | 3.22  | 29.01 |  570.7 |
      deserialize | 26.71 | 126.0 | 29.26 |  174.9 | 2.56  | 34.14 |  326.3 |
    --------------+-------+-------+-------+--------+-------+-------+--------+
    large msgs    |       |       |       |        |       |       |        |
      serialize   | 17.64 | 134.9 | 26.85 |  156.8 | 2.31  | 26.71 |  330.5 |
      deserialize | 21.76 | 113.3 | 25.34 |  123.0 | 1.81  | 19.25 |  226.0 |
    --------------+-------+-------+-------+--------+-------+-------+--------+

The performances are measured as number of processed MB/s,
serialized form.  Higher values means better performance.

The benchmarks are run with small and large messages (228 and 84584
bytes, respectively, in serialized form)

The Java benchmark is run with optimization both for code size and for
speed. The Python implementation cannot optimize for speed.

    SW: Python 2.6.6, Java SE 1.6.0_22, Erlang/OTP R14B02,
        Linux kernel 2.6.39, Debian, protobuf-2.4.1
    HW: Intel Core i5 760, 2.8GHz, 4x256 kB L2 cache, 8MB L3 cache

The benchmarks are all done with the exact same messages files and
proto files.  The source of the benchmarks was found in the Google
protobuf's svn repository.  The gpb does not support groups, but the
benchmarks in the protobuf used groups, so I converted the
google_message*.dat to use sub message structures instead.
For protobuf, that change was only barely noticable.

For performance, the generated Erlang code avoids creating sub
binaries as far as possible. It has to for sub messages, strings and
bytes, but for the rest of the types, it avoids creating sub binaries,
both during encoding and decoding (for info, compile with the
`bin_opt_info' option)


Mapping of protocol buffer datatypes to erlang
----------------------------------------------

    .proto type           Erlang type
    ----------------------------------------------------------------
    double, float         floating point number
                          when encoding, integers, too, are accepted
    ----------------------------------------------------------------
    int32, int64,
    uint32, uint64,
    sint32, sint64,
    fixed32, fixed64,
    sfixed32, sfixed64    integer
    ----------------------------------------------------------------
    bool                  true | false
    ----------------------------------------------------------------
    enum                  atom
    ----------------------------------------------------------------
    message               record (thus tuple)
    ----------------------------------------------------------------
    string                unicode string, thus list of integers
    ----------------------------------------------------------------
    bytes                 binary
