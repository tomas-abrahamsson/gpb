Short guide to the translations
-------------------------------

For each translation there are a function calls on the following format:

- `tr_encode_Msg.field(Value, UserData)`
- `tr_decode_Msg.field(Value, UserData)`
- `tr_merge_Msg.field(Value1, Value2, UserData)`

and so on.

If no translation is needed, the body if such a `tr_...` function
is just `id(Value)`.

To get some speed (avoiding function calls, see the reasoning decoding.md),
each such `tr_...` call is marked `-compile({inline,Fn/Arity})`,
so it is inlined at the call site, often causing minimal extra cost,
especially for `id` translations.

This means it is possible to make the generation of translations into a
separate step, it is not necessary to mix this into all the other
intricacies of encoding, decoding, merging and so on. Those need only
generate the call, and the actual translation, possible stacking of
multiple translations, and argument templating can be done separately.

The generation of (inline-marked) translation functions needs to handle
- possible underscoring of unused parameters,
  to avoid warnings about unused variables
- argument templating
- stacking of translations

The templating is illustrated like this: if an option
to `gpb_compile:file/2` is
```
   {translate_field,
    {['MsgName',field_name],
     [{encode, {mod, e_fn, ['$1']}},
      {decode, {mod, d_fn, ['$user_data', '$1', a, b, c]}}]}}
```
then the generated translation functions would look roughly like:
```
   tr_encode_MsgName.field_name(Value, _UserData) -> % nb: undercore!
      mod:e_fn(Value).

   tr_decode_MsgName.field_name(Value, UserData) -> '$1' refers to 1st param
      mod:d_fn(UserData, Value, a, b, c). % template use
```

The idea to be able to have translations stackable works like this: if
there are 2 encode-translations, A and B in that order for some field,
then the `tr_encode_` function would look like:
```
tr_encode_MsgName.field_name(Value, _UserData) ->
    mod_b:e_fn_b(
      mod_a:e_fn_a(Value)).
```

Map type fields
---------------

Internally within gpb, the translation mechanism is also (re-)used for
`map<_,_>` type fields. A field in a message `M`
```
message M {
  ...
  map<KeyType,ValueType> f = 17;
  ...
}
```
is translated internally like this:
```
message M {
  ...
  repeated SomeDynamicallyGeneratedName f = 17;
  ...
}

message SomeDynamicallyGeneratedName {
  optional KeyType key = 1;
  optional ValueType value = 2;
}
```

In fact, I had had the translation idea in my head for a long time, but
when protobuf 3.0.0 arrived with the `map<_,_>` type fields and
`google.protobuf.Any` wellknown fields, I decided to implement those as
translations. Only later on, I opened up for translations to be applied
to all kinds of fields, and also to be stackable.

Specifying translators for fields and by type
---------------------------------------------

It is possible to specify translators to be applied to individual
fields, or for all fields of a certain type.

The "all fields of a certain type" gets expanded/converted by
`gpb_analyzer` into translation options by for the individual fields, by
running a separate pass to see which message fields are of that
specified type.
