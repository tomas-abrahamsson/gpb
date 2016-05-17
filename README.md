The gpb is a compiler for Google protocol buffer definitions files
for Erlang.

See https://developers.google.com/protocol-buffers/ for further information
on the Google protocol buffers.

Features of gpb
---------------

*  Parses protocol buffer definition files and can generate:
   - record definitions, one record for each message
   - erlang code for encoding/decoding the messages to/from binaries

*  Features of the protocol buffer definition files:
   gpb supports:
   - message definitions (also messages in messages)
   - scalar types
   - importing other proto files
   - nested types
   - message extensions
   - the 'packed' and 'default' options for fields
   - the 'allow_alias' enum option (treated as if it is always set true)
   - generating metadata information
   - package namespacing (optional)
   - oneof (introduced in protobuf 2.6.0)
   - map<_,_> (introduced in protobuf 3.0.0)

   gpb reads but ignores or throws away:
   - options other than 'packed' or 'default'
   - custom options

   gpb does not support:
   - groups
   - aggregate custom options introduced in protobuf 2.4.0
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
     but there is normally a compile-time dependency for the generated
     code: to the #field{} record in gpb.hrl the for the get_msg_defs
     function, but it is possible to avoid this dependency by using
     the also the `defs_as_proplists` or `-pldefs` option.
   - Gpb can generate code both to files and to binaries.

*  Introspection

   gpb generates some functions for examining messages, enums and services:
   - `get_msg_defs()`, `get_msg_names()`, `get_enum_names()`
   - `find_msg_def(MsgName)` and `fetch_msg_def(MsgName)`
   - `find_enum_def(MsgName)` and `fetch_enum_def(MsgName)`
   - `enum_symbol_by_value(EnumName, Value)`,
   - `enum_symbol_by_value_<EnumName>(Value)`,
     `enum_value_by_symbol(EnumName, Enum)` and
     `enum_value_by_symbol_<EnumName>(Enum)`
   - `get_service_names()`, `get_service_def(ServiceName)`, `get_rpc_names(ServiceName)`
   - `find_rpc_def(ServiceName, RpcName)`, `fetch_rpc_def(ServiceName, RpcName)`

   There are also some version information functions:

   - `gpb:version_as_string()` and `gpb:version_as_list()`
   - `GeneratedCode:version_as_string()` and `GeneratedCode:version_as_list()`
   - `?gpb_version`  (in gpb_version.hrl)
   - `?'GeneratedCode_gpb_version'`  (in GeneratedCode.hrl)

   The gpb can also generate a self-description of the proto file.
   The self-description is a description of the proto file, encoded to
   a binary using the descriptor.proto that comes with the Google
   protocol buffers library. Note that such an encoded self-descriptions
   won't be byte-by-byte identical to what the Google protocol buffers
   compiler will generate for the same proto, but should be roughly
   equivalent.

*  Erroneously encoded protobuf messages and fields will generally
   cause the decoder to crash.  Examples of such erroneous encodings are:
   - varints with too many bits
   - strings, bytes, sub messages or packed repeated fields,
     where the encoded length is longer than the remaining binary

*  Maps

   Gpb can generate encoders/decoders for maps.

   The option `maps_unset_optional` can be used to specify behavior
   for non-present optional fields: whether they are omitted from
   maps, or whether they are present, but have the value `undefined`
   like for records.

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

   The gpb will fail to decode floats that are NaN, +Inf and -Inf,
   and there is no possibility to encode such floats.

Performance
-----------

Here is a comparison between gpb (interpreted by the erlang vm) and
the C++, Python and Java serializers/deserializers of protobuf-2.6.1rc1

    [MB/s]        | gpb   |pb/c++ |pb/c++ | pb/c++ | pb/py |pb/java| pb/java|
                  |       |(speed)|(size) | (lite) |       |(size) | (speed)|
    --------------+-------+-------+-------+--------+-------+-------+--------+
    small msgs    |       |       |       |        |       |       |        |
      serialize   |   52  | 1240  |   85  |   750  |  6.5  |   68  |  1290  |
      deserialize |   69  |  880  |   85  |   950  |  5.5  |   90  |   450  |
    --------------+-------+-------+-------+--------+-------+-------+--------+
    large msgs    |       |       |       |        |       |       |        |
      serialize   |   36  |  950  |   72  |   670  |  4.5  |   55  |   670  |
      deserialize |   45  |  620  |   71  |   480  |  4.0  |   60  |   360  |
    --------------+-------+-------+-------+--------+-------+-------+--------+

The performances are measured as number of processed MB/s,
serialized form.  Higher values means better performance.

The benchmarks are run with small and large messages (228 and 84584
bytes, respectively, in serialized form)

The Java benchmark is run with optimization both for code size and for
speed. The Python implementation cannot optimize for speed.

    SW: Python 2.7.11, Java 1.8.0_77 (Oracle JDK), Erlang/OTP 18.3, g++ 5.3.1
        Linux kernel 4.4, Debian (in 64 bit mode), protobuf-2.6.1rc1
    HW: Intel Core i7 5820k, 3.3GHz, 6x256 kB L2 cache, 15MB L3 cache
        (CPU frequency pinned to 3.3 GHz)

The benchmarks are all done with the exact same messages files and
proto files.  The source of the benchmarks was found in the Google
protobuf's svn repository.  The gpb does not support groups, but the
benchmarks in the protobuf used groups, so I converted the
google_message*.dat to use sub message structures instead.
For protobuf, that change was only barely noticeable.

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
                          or maps, if the maps (-maps) option is specified
    ----------------------------------------------------------------
    string                unicode string, thus list of integers
                          or binaries, if the strings_as_binaries (-strbin)
                          option is specified
    ----------------------------------------------------------------
    bytes                 binary
    ----------------------------------------------------------------
    oneof                 {ChosenFieldName, Value}
    ----------------------------------------------------------------
    map<_,_>              An unordered list of 2-tuples: [{Key,Value}]
                          or a map, if the maps (-maps) option is specified


Interaction with rebar
----------------------

For info on how to use gpb with rebar3, see
https://www.rebar3.org/docs/using-available-plugins#section-using-gpb

In rebar there is support for gpb since version 2.6.0. See the
proto compiler section of rebar.sample.config file at
https://github.com/rebar/rebar/blob/master/rebar.config.sample

For older versions of rebar---prior to 2.6.0---the text below outlines
how to proceed:

Place the .proto files for instance in a `proto/` subdirectory.
Any subdirectory, other than src/, is fine, since rebar will try to
use another protobuf compiler for any .proto it finds in the src/
subdirectory.  Here are some some lines for the `rebar.config` file:

    %% -*- erlang -*-
    {pre_hooks,
     [{compile, "mkdir -p include"}, %% ensure the include dir exists
      {compile,
       "/path/to/gpb/bin/protoc-erl -I`pwd`/proto"
       "-o-erl src -o-hrl include `pwd`/proto/*.proto"
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

Thus, to create a new version of gpb, the single source from where
this version is fetched, is the git tag.   (If you are importing
gpb into another version control system than git, or using another
build tool than rebar, you might have to adapt rebar.config and
src/gpb.app.src accordingly.)

The version number on the master branch of the gpb on github is
intended to always be only integers with dots, in order to be
compatible with reltool.  In other words, each push to github is
considered a release, and the version number is bumped.  To ensure
this, there is a `pre-push` git hook and two scripts,
`install-git-hooks` and `tag-next-minor-vsn`, in the helpers
subdirectory. The ChangeLog file will not necessarily reflect all
minor version bumps, only important updates.

Places to update when making a new version:
* Write about the changes in the ChangeLog file,
  if it is a non-minor version bump.
* tag it in git


Contributing
------------

Contributions are welcome, preferably as pull requests or git patches
or git fetch requests.  Here are some guide lines:

* Use only spaces for indentation, no tabs. Indentation is 4 spaces.
* The code must fit 80 columns
* Verify that the code and documentation compiles and that tests are ok:
  rebar clean compile eunit doc xref
* If you add a feature, test cases are most welcome,
  so that the feature won't get lost in any future refactorization
* Use a git branch for your feature. This way, the git history will
  look better in case there is need to refetch.
