-ifndef(gpb_hrl).
-define(gpb_hrl, true).

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
        | {'msg',atom()}.         % record()     the msg name is record name

-record(field,
        {name       :: atom(),
         fnum       :: integer(),
         rnum       :: pos_integer(), %% field number in the record
         type       :: gpb_field_type(),
         occurrence :: 'required' | 'optional' | 'repeated',
         opts       :: [term()]
        }).

-endif.
