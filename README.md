The gpb is a compiler for Google protocol buffer definitions files
for Erlang.

See https://code.google.com/p/protobuf/ for further information on
the Google protocol buffers.

Features of gpb
---------------

*  Parses protocol buffer definition files and can generate:
   - record defintions, one record for each message
   - erlang code for encoding/decoding the messages to/from binaries

*  Features of the protocol buffer definition files:
   gpb supports:
   - message definitions (also messages in messages)
   - scalar types
   - importing other proto files
   - nested types
   - message extensions
   - the 'packed' and 'default' options
   - generating metadata information
   - package namespacing (optional)

   gpb reads but ignores or throws away:
   - options other than 'packed' or 'default'
   - custom options
   - services

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
   - Gpb can optionally make use of the package attribute by prepending
     the name of the package to every contained message type (if defined),
     which is useful to avoid name clashes of message types across packages.
   - The generated encode/decoder has no run-time dependency to gpb,
     but there is a compile-time dependency for the generated code:
     to the #field{} record in gpb.hrl the for the get_msg_defs function.
   - Gpb can generate code both to files and to binaries.

*  Introspection

   gpb generates some functions for examining the messages and enums:
   - `get_msg_defs()`, `get_msg_names()`, `get_enum_names()`
   - `find_msg_def(MsgName)` and `fetch_msg_def(MsgName)`
   - `find_enum_def(MsgName)` and `fetch_enum_def(MsgName)`
   - `enum_symbol_by_value(EnumName, Value)`,
   - `enum_symbol_by_value_<EnumName>(Value)`,
     `enum_value_by_symbol(EnumName, Enum)` and
     `enum_value_by_symbol_<EnumName>(Enum)`

   There are also some version information functions:

   - `gpb:version_as_string()` and `gpb:version_as_list()`
   - `GeneratedCode:version_as_string()` and `GeneratedCode:version_as_list()`
   - `?gpb_version`  (in gpb_version.hrl)
   - `?'GeneratedCode_gpb_version'  (in GeneratedCode.hrl)

*  Reporting of errors in .proto files

   Gpb is not very good at error reporting, especially referencing
   errors, such as references to messages that are not defined.
   You might want to first verify with `protoc` that the .proto files
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
      serialize   | 27.56 | 479.9 | 31.81 |  302.8 |  3.09 | 27.08 |  547.9 |
      deserialize | 27.30 | 269.8 | 28.16 |  381.7 |  2.49 | 32.00 |  325.1 |
    --------------+-------+-------+-------+--------+-------+-------+--------+
    large msgs    |       |       |       |        |       |       |        |
      serialize   | 18.83 | 447.7 | 26.87 |  284.8 |  2.20 | 24.82 |  314.2 |
      deserialize | 24.39 | 269.6 | 24.69 |  337.2 |  1.74 | 17.93 |  215.0 |
    --------------+-------+-------+-------+--------+-------+-------+--------+

The performances are measured as number of processed MB/s,
serialized form.  Higher values means better performance.

The benchmarks are run with small and large messages (228 and 84584
bytes, respectively, in serialized form)

The Java benchmark is run with optimization both for code size and for
speed. The Python implementation cannot optimize for speed.

    SW: Python 2.6.6, Java SE 1.6.0_22, Erlang/OTP R16B01, g++ 4.6.1
        Linux kernel 3.10, Debian, protobuf-2.4.1,
    HW: Intel Core i5 760, 2.8GHz, 4x256 kB L2 cache, 8MB L3 cache
        (Turbo boost turned off, CPU frequency pinned to 2.8 GHz)

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
`bin_opt_info` option)

The Erlang code ran in the smp emulator, though only one CPU core
was utilized.

The generated C++ core was compiled with -O3.


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


Interaction with rebar
----------------------

Place the .proto files for instance in a `proto/` subdirectory.
Any subdirectory, other than src/, is fine, since rebar will try to
use another protobuf compiler for any .proto it finds in the src/
subdirectory.  Here are some some lines for the `rebar.config` file:

    %% -*- erlang -*-
    {pre_hooks,
     [{compile, "mkdir -p include"}, %% ensure the include dir exists
      {compile,
       "erl +B -noinput -pa /path/to/gpb/ebin "
       "    -I`pwd`/proto -o-erl src -o-hrl include "
       "    -s gpb_compile c `pwd`/proto/*.proto"
      }]}.

    {post_hooks,
     [{clean,
       "bash -c 'for f in proto/*.proto; "
       "do "
       "  rm -f src/$(basename $f .proto).erl; "
       "  rm -f include/$(basename $f .proto).hrl; "
       "done'"}
     ]}.

    {erl_opts, [{i, "/path/to/gpb/include"}]}.


Version numbering
-----------------

The gpb version number is fetched from the git latest git tag
matching N.M where N and M are integers.  This version is
inserted into the gpb.app file as well as into the
include/gpb_version.hrl.  The version is the result of the command

  git describe --always --tags --match '[0-9]*.[0-9]*'

Thus, so create a new version of gpb, the single source from where
this version is fetched, is the git tag.   (If you are importing
gpb into another version control system than git, or using another
build tool than rebar, you might have to adapt rebar.config and
src/gpb.app.src accordingly.)
