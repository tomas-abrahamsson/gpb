Version marker
--------------

The format is currently a list of tuples, most are 2-tuples.

The version is indicated with a `{proto_defs_version, integer()}` item,
preferably first in list.

If there is no `proto_defs_version`, then it is implicitly version 1.

Version numbering
-----------------

Version numbers are integers, and a new version indicate a
non-backwards-compatible change.

Even when the version number has not changed, there may still
be differences, but only backwards compatible changes.

A non-backwards-compatible change is generally a change to an
existing tuple definition item, such as:

* A change of the interpretation of some value
* A change to the size of a tuple
* Reordering of lists where the order matters, such as
  the list of fields in message, or the list of enumerators
  in an enum (tooling and users expects same order as in
  the proto)

A change of the entire format, such as from a list of tuples to a
map, would of course be a non-backwards-compatible as well.

A backwards-compatible change is for example

* Addition of a new kind of tuple, ie elements with types
  different from any other list items, such as tuples with
  new first-element keys.
* Reordering of a tuples
* Reordering of inner lists, where the ordering is not
  significant, such as elements in a `msg_containment`
  tuple.

Format versions obey the Erlang term order and be compared like
normal Erlang terms with for instance `=<`.

Conversion between versions
---------------------------

Internally, gpb operates on the latest version.

On input, for example the `gpb_compile:proto_defs/2,3`, definitions are
converted to the latest, if needed.

On output, for example the `to_proto_defs` option to `gpb_compile:file` or
`gpb_compile:string`, definitions are converted if needed, as indicated
by any `proto_defs_version` option.  Another form of output are the
generated introspection functions, the format of which can be
controlled with the `introspect_proto_defs_version` option.

In gpb-4.x.y, the default `proto_defs_version` is 1, and the default
and the default `introspect_proto_defs_version` is 1 if possible, else 2.

To opt in to a later version, use `proto_defs_version` option like this:
```
  gpb_compile:file(..., [to_proto_defs,
                         {proto_defs_version, <SomeLaterVersion>},
                         ...)
```

There are also functions in `gpb_defs.erl` for converting to and from
the latest version, see the `gpb_defs:convert_defs_to_latest_version/1`
and the `gpb_defs:convert_defs_from_latest_version/2` functions.

In general, converting from a previous version to the latest is
expected to always succeed, but converting from the latest to a
previous version may fail.

In a future gpb-5.0.0, the intention is that the default version
will change to 2.

Specifying versions from other programs
---------------------------------------

If you write programs that operate on the proto definitions, then the
recommendation is to specify the targeted version explicitly.
In particular, _do not_ specify it using something like
`lists:max(gpb_defs:supported_defs_versions())`, because then if gpb would
introduce a new version, your program would receive unexpected proto
definitions, which could cause it to break.

Version 1
---------

This is the initial version.

Version 2
---------

In this version, the following has changed:

For proto files with `syntax="proto3"`, the `occurrence` of fields has
changed from `optional` to `defaulty`.

In Google's protobuf 3.12.0, as of this writing, experimental support for
`optional` was added also to proto3 messages, see [the field_presence.md document](https://github.com/protocolbuffers/protobuf/blob/v3.12.0/docs/field_presence.md)
for more info.  When this is used, `occurrence=optional` is used
to indicate it.

Example: Given the following proto file:
```
  syntax="proto3";
  message Msg1 {
    uint32 f1 = 1;
  }
  message Msg2 {
    optional uint32 g1 = 1;
  }
  ```
the definitions in version 2 is now:
```
  [{proto_defs_version, 2},
   ...
   {proto3_msgs, ['Msg1', 'Msg2']},
   ...
   {{msg,'Msg1'}, [#?gpb_field{name=f1, ..., occurrence=defaulty, ...}]},
   %%                                                   ^^^^^^^^
   %% in version 1, the occurrence would have been:     optional
   ...
   {{msg,'Msg2'}, [#?gpb_field{name=g1, ..., occurrence=optional, ...}]},
   %%                                                   ^^^^^^^^
   %% in version 2, this indicates proto2-style field presence handling.
   ...]
```

It is possible to convert from version 2 to version 1 only if there
are no proto3 message with a field with `occurrence=optional`.

### Impact

First some background: With `syntax="proto3"`, when encoding a
field, if it has the type's default value, it must not be serialized,
and on decoding, it must be set to the type's default value if it is
not present in the binary to decode. With `syntax="proto2"` if the
field is set, it is included in the serialized binary regardless of
value.  On decoding, if it is not present in the binary to decode, it
is always considered to not be set.

In version 1, when processing a `#?gpb_field{}` with
`occurrence=optional`, to invoke the correct handling as described
above, it was also necessary to also check whether the message
was a proto3 message or not (using the `proto3_msgs` item.)
In version 2, `occurrence=defaulty` indicates proto3 handling
and `occurrence=optional` indicates proto2 handling, there is no longer
any need to also check whether a message is proto3 or not.


Version 3
---------

In version 3, options and enums have changed.

Enumerators in version 2 were `{Sym,Value}` but in version 3, they are:

```
   {Symbol::atom(), Value::integer(), Options::list()}
```

Also, enumeration options are moved out to a separate
`{{enum_options, EnumName}, Options}` entry, like how it is for `msg_options`.

Example: Given the following proto file:
```
  enum E1 {
    option (my_option) = true;
    A = 0 [(my_enumerator_option) = true];
  }
```

the definitions in version 3 vs 2 look like:
```
  [{proto_defs_version, 3},
   ...
   {{enum,'E1'}, [{'A', 0, [{[my_enumerator_option], true}]}]}]
   {{enum_options, 'E1'}, [{[my_option], true}]}
   ...]

vs

  [{proto_defs_version, 2},
   ...
   %% In version 2, the my_enumerator_option is not included
   {{enum,'E1'}, [{option, [my_option], true},
                  {a, 0}]}
   ...]
```

Version 4
---------

In version 4, the representation of custom options has changed.

This concerns options and custom options on file-level, in messages and
fields, in, enumerations and enum values and in services.  Options for
rpc methods are a bit of a different story though: they now look like 
options in any other positions, which was not the case previously.

Still in version 4, custom option names are not resolved, but instead
preserved as they occor in the .proto file, though on erlang format.

The table below summarizes the option format versions.

| In the `.proto` | format ≥ 4       | format ≤ 3        | rpc opts ≤ 3    |
| --------------- | ---------------- | ----------------- | --------------- |
| `opt_name`      | `opt_name`       | `opt_name`        | `opt_name`      |
| `(custom_opt)`  | `[{custom_opt}]` | `[custom_opt]`    | `custom_opt`    |
| `(pkg.opt)`     | `[{pkg,opt}]`    | `[pkg,'.',opt]`   | `'pkg...opt'`   |
| `(pkg.opt).f`   | `[{pkg,opt},f]`  | `[pkg,'.',opt,f]` | `'pkg...opt.f'` |
| `(opt).f.g`     | `[{opt},f,g]`    | `[opt,f,g]`       | `'opt.f.g'`     |
| `(p).(f).g`     | `[{p},{f},g]`    | `[p,f,g]`         | `'p.f.g'`       |
| `(.p).g`        | `[{'.',p},g]`    | `['.',p,'.',f,g]` | `'..p...f.g'`   |

Below is an example of proto definitions to indicate where
these option names have changed format. This example does not
show all places where (custom) options can occur, but highlights
where the options in the table above occur.

```
.proto:

   enum E1 {
     allow_alias = true;
     (my_custom_option) = 1;
     (other_custom_opt).x = "abc";
     A = 0 [(a_opt) = 1];
   }
   message M1 {
     option (b_opt) = true;
     required uint32 f = 1 [packed, deprecated=false, (c_opt).d=2];
   }
   service S1 {
     rpc Cc (M1) returns (M1) {
       option (m_opt) = 1;
     }
   }

Defs:

   [{proto_defs_version, 4},
    ...
    {{enum, 'E1'}, [{'A', 0, [{[{a_opt}], 1}]}
                   ]},
    {{enum_options,'E1'}, [{allow_alias,true},
                           {[{my_custom_option}], 1},
                           {[{other_custom_option},x], "abc"}]},
    ...
    {{msg, 'M1'}, [#?gpb_field{name=f, opts=[{packed,true},
                                             {deprecated,false},
                                             {[{c_opt},d], 2}]}]},
    {{msg_options,'M1'}, [{[{b_opt}], true}}]},
    ...
    {{service,'S1'}, [#?gpb_rpc{opts=[{[{m_opt}], 1}]}]}
    %%                                 ^^^^^^^^^
    %%                                 rpc opts in the table above
    ...]
```
