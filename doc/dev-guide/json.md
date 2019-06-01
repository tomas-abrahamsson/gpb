JSON formats:
---------------------
- jsx
    objects: proplist() | [{}] % non-empty, empty object respectively
             | map()           % optionally
    keys:    binary()
             | atom()     % on encoding or optionally on decoding
             | integer()
    arrays:  list()
    strings: binary()
             | atom()     % on encoding (only?)
    null:    null
- mochijson2
    https://github.com/bjnortier/mochijson2/blob/master/src/mochijson2.erl
    objects: {struct, proplist()}
    keys:    binary()
             | atom()             % on encoding
    arrays:  list()
    strings: binary()
             | atom()             % on encoding
             | {json, iolist()}
    null:    null
- jiffy
    https://github.com/davisp/jiffy
    objects: {proplist()}
             | map()    % optionally
    keys:    binary()
             | atom()   % on encoding
    arrays:  list()
    strings: binary()
             | atom()   % on encoding
    null:    null
             | nil      % optionally on encoding
- eep0018
    https://github.com/erlang/eep/blob/master/eeps/eep-0018.md
    objects: proplist() | [{}] % non-empty, empty object respectively
    keys:    binary() | atom() % binary() is default, atom() optionally
    arrays:  list()
    strings: binary()
    null:    null
- mochijson
    https://github.com/basho/mochiweb/blob/master/src/mochijson.erl
    objects: {struct, proplist()}
    keys:    binary() | atom() | string()
    arrays:  {array, list()}
    strings: binary() | atom() | string()
    null:    null
- jsonx
    https://github.com/iskra/jsonx
    objects: {proplist()}
             | proplist()          % runtime option
             | {struct,proplist()} % runtime option
             | #rec{...}           % runtime option
    keys:    binary()
    arrays:  list()
    strings: binary()
             | {json, iolist()}    % on encoding
    null:    null
- erlang-json-eep-parser
    https://github.com/jchris/erlang-json-eep-parser/tree/master
    objects: {proplist()}
    keys:    binary()
    arrays:  list()
    strings: binary()
    null:    ??
- yaws json2
    https://github.com/klacke/yaws/blob/0244e95/src/json2.erl
    objects: {struct, proplist()}
    keys:    string()
    arrays:  {array, [ElementList]}
    string:  [0..65000]
    null:    null
             | undefined  % on encoding
- jsone
    https://github.com/sile/jsone
    objects: {proplist()}
             | proplist() | [{}]   % optionally
             | map()               % optionally
    keys:    binary()
             | atom()              % on encoding
    arrays:  list()
    strings: binary()
             | atom()              % on encoding
             | {json, iolist()}    % on encoding
             | {json_utf8, Chars}  % on encoding
    null:    null
             | undefined           % on encoding
Things to additionally conisder:
- poison (elixir)
    https://github.com/devinus/poison
- jazz (elixir)
    https://github.com/meh/jazz
- jason (elixir)
    https://github.com/michalmuskala/jason
- json (elixir)
    https://github.com/cblage/elixir-json
- exjson (elixir)
    https://github.com/guedes/exjson
- jsex (elixir)
    objects: map()
    arrays:  list() | Enumerable
    null:    nil
- tiny (elixir)
    https://github.com/whitfin/tiny
    objects: map()
    keys:    binary()
             | atom()  % on encoding
- erlson (elixir)
    https://github.com/alavrik/erlson
    objects: erlson dictionary
    keys:    atom()
    strings: binary()
    arrays:  list()
    null:    undefined

GPB options to support (most of) the above (hopefully)
--------------------------------------------------------
  {json_object_format, eep18 | {proplist} | {atom(), proplist} | map}
  {json_key_format,    atom | binary | string}
  {json_array_format,  list | {atom(), list}}
  {json_string_format, binary | list}
  {json_null,          atom()}
