-ifndef(gpb_hrl).
-define(gpb_hrl, true).

-type gpb_map_key_type() ::
        int32 | int64
        | uint32 | uint64
        | sint32 | sint64
        | fixed32 | fixed64
        | sfixed32 | sfixed64
        | bool
        | string.

-type gpb_field_type() ::        %% Erlang type  Comment
        'int32' | 'int64'         % integer()    variable-length encoded
        | 'uint32' | 'uint64'     % integer()    variable-length encoded
        | 'sint32' | 'sint64'     % integer()    variable-length zig-zag encoded
        | 'fixed32' | 'fixed64'   % integer()    always 4 | 8 bytes on wire
        | 'sfixed32' | 'sfixed64' % integer()    always 4 | 8 bytes on wire
        | 'bool'                  % true | false
        | 'float' | 'double'      % float()
        | 'string'                % string()     UTF-8 encoded
        | 'bytes'                 % binary()
        | {'enum',atom()}         % atom()       the enum literal is the atom
        | {'msg',atom()}          % record()     the msg name is record name
        | {'map',gpb_map_key_type(), gpb_field_type()}. % [{K,V}] or map()

%% The following two definitions (`gpb_field' and `gpb_rpc') are to
%% avoid clashes with other code, since the `field' and `rpc' are
%% really too general names, they should have been prefixed.
%%
%% Unfortunately, they are already part of the API, so they can't
%% be changed without breaking backwards compatibility.
%% (They appear as parameters or return values for functions in `gpb'
%% in generated code.)
%%
%% In case a clash, it is possible to redefine the name locally.
%% The recommendation is to redefine them with prefix, ie to `gpb_field'
%% and `gpb_rpc', since this is what they will change to in some future.
%%
-ifdef(gpb_field_record_name).
-define(gpb_field, ?gpb_field_record_name).
-else.
-define(gpb_field, field). %% odd definition is due to backwards compatibility
-endif.

-ifdef(gpb_rpc_record_name).
-define(gpb_rpc, ?gpb_rpc_record_name).
-else.
-define(gpb_rpc, rpc). %% odd definition is due to backwards compatibility
-endif.

-record(?gpb_field, % NB: record name is (currently) `field' (not `gpb_field')!
        {name               :: atom(),
         fnum               :: integer(),
         rnum               :: pos_integer(), %% field number in the record
         type               :: gpb_field_type() |
                               {ref,  term()} | %% intermediary, during parsing
                               {msg,  list()} | %% intermediary, during parsing
                               {enum, list()},  %% intermediary, during parsing
         occurrence         :: 'required' | 'optional' | 'repeated',
         opts      = []     :: [term()]
        }).

-record(?gpb_rpc, % NB: record name is (currently) `rpc' (not `gpb_rpc')!
        {name               :: atom(),
         input,
         output
        }).

-record(gpb_oneof,
        {name   :: atom(),
         rnum   :: pos_integer(),  %% field number in the record
         fields :: [#?gpb_field{}] %% all fields have the same rnum
        }).

-endif.
