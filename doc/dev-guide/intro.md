
Encoder/decoder in gpb
----------------------

The gpb module implements a protobuf encoder and decoder without any
bells or whistles.  It only supports records, not maps, and no
options.  It does not generate any code.  It encodes or decodes according
to the proto definitions passed as input.  The main purose of it
is to serve another implementation that can be used to cross-check
the encoder and decoder generated from gpb_compile.
Originally, this was the only encoder and decoder in gpb.

Generated encoder/decoder
-------------------------

The gpb_compile can generate code from either a file, a string or
proto definitions.  Output is normally an erl file, but it is also
possible to let it return just the proto definitions after parsing
and name resolutions have taken place.

The generated code can either contain an encoder and decoder in
Erlang, but it is also possible to use the the C++ encoder/decoder
as a NIF.  The NIF binding can be useful for interop tests of
encoding and decoding with the reference implementation.


Quickcheck suite in a repo
--------------------------

The gpb-eqc contains a quickcheck suite for testing encoding and decoding.
Using maps:to_list and maps:from_list, it can test maps as well as records.
