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
and `introspect_proto_defs_version` is the same as `proto_defs_version`
if it is set, else 1.

To opt in to a later version, use `proto_defs_version` option like this:
```
  gpb_compile:file(..., [to_proto_defs,
                         {proto_defs_version, <SomeLaterVersion>},
                         ...)
```

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
