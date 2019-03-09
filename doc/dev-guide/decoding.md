Short guide to the design of decoding
-------------------------------------

The generated code centers much around decoding of
[varints](https://developers.google.com/protocol-buffers/docs/encoding#varints)
ie variable length integers. The basic loop construct looks like:

```
  decode_varint(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->
      decode_varint(Rest, N+7, X bsl N + Acc, Msg);
  decode_varint(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->
      Varint = X bsl N + Acc,
      ...body...

```

The `...body...` generally adds an updated field to the `Msg` tuple or
map, and then continues to unpack more fields.

Some varints are known in advance, at code-generation time, and the
generated code has fast-path function clauses for these. But a varint
can also be encoded in a non-minimal way, so there is also an general
varint decoder (this fast-path approach gave some performance
improvement):

```

  %% fast path
  dfp(<<10, Rest/binary>>, Msg) -> decode_field_f1(Rest, Msg);
  dfp(<<18, Rest/binary>>, Msg) -> decode_field_f2(Rest, Msg);
  ...
  dfp(<<>>, Msg) -> Msg;
  dfp(Other, Msg) -> dg(Other, Msg).

  %% general case
  dg(<<1:1, X:7, Rest/binary>>, N, Acc, Msg) ->
      dg(Rest, N+7, X bsl N + Acc, Msg);
  dg(<<0:1, X:7, Rest/binary>>, N, Acc, Msg) ->
      Varint = X bsl N + Acc,
      case Varint of
          10 -> decode_field_f1(Rest, Msg);
          18 -> decode_field_f2(Rest, Msg);
          ...
      end;
  dg(<<>>, _N, _Acc, Msg) ->
      Msg.
```

Another trick I've employed is that if decoding a message means making
no or few calls to other functions, then I can have all fields as
parameters to the decoder function instead (ie largely untouched in y
registers), and pack the `Msg` (to a record or map) only last, when all
fields have been processed. So the generated functions often have many
arguments, one for each message field. An estimate is made initially, to
determine, per message, whether it is more advantageous to pass fields
as a record/map or as parameters. There is also a limitation in number
of parameters for a function. This approach also meant some performance
gain. When passing fields as parameters, the generated code looks
roughly like:

```
  dfp(<<10, Rest/binary>>, F1, F2, ...) -> decode_field_f1(Rest, F1, F2, ...);
  dfp(<<18, Rest/binary>>, F1, F2, ...) -> decode_field_f2(Rest, F1, F2, ...);
  ...
  dfp(<<>>, F1, F2, ...) -> #msg{f1 = F1, f2 = F2, ...};
  dfp(Other, F1, F2, ...) -> dg(Other, F1, F2, ...).

  dg(...) ->
     ...similarly..

  %% let's say the type of field f1 is fixed32
  decode_field_f1(<<X:32/little, Rest/binary>>, _, F2, ...) ->
     dfp(Rest, X, F2, ...).
```

The decision whether to pass around a message as a tuple or map, or to
pass the fields as parameters, is taken (per message) in
gpb_analyzer:compute_decode_field_pass_methods/2, and is influenced by
number of fields as well as an estimate on how many function calls needs
to be made. Function calls implies saving parameters to the stack and
restoring from the stack after the function call.

Code morphing
-------------

There are many dimensions:
- field pass method (as record or map fields, or as parameters)
- generate for maps or records
- for maps: generate with unset optionals omitted (default) or
  with the value `undefined` (mostly due to historical reasons/mistakes)
- for maps: oneof are tuples or separate keys (flat)
- all the different protobuf field types
- whether each field is repeated, optional or required

The code generator needs to lay out different code for each of these
cases, but it leads to a quite large combinatorial explosion.
To reduce that explosion somewhat, the generation of the decoding code is
separated into two steps:

First, generate code for decoding to records, take care of
- always field pass method = pass as record
- all the different protobuf field types
- whether each field is repeated, optional or required

Second, possibly transform (morph) the generated code, taking these
aspects into consideration:
- field pass method (as record or map fields, or as parameters)
- generate for maps or records
- for maps: generate with unset optionals omitted (default) or
  with the value `undefined` (mostly due to historical reasons/mistakes)
- for maps: oneof are tuples or separate keys (flat)

This second step is done by the `gpb_codemorpher.erl`, which is called
from the gpb_gen_decoders.
