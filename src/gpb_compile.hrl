-ifndef(gpb_compile_hrl).
-define(gpb_compile_hrl, true).

-record(ft, {type, occurrence, is_packed}).
-record(anres, %% result of analysis
        {
          used_types,         % :: sets:set(gpb_field_type()),
          known_msg_size,     % :: dict:dict(), %% MsgName -> Size | undefined
          fixlen_types,       % :: sets:set(#ft{}),
          num_packed_fields,  % :: integer(),
          num_fields,         % :: dict:dict(), %% MsgName -> integer()
          d_field_pass_method,% :: dict:dict()  %% MsgName -> pass_as_record |
                              %                 %%            pass_as_params
          maps_as_msgs,       % :: list() % same format as `Defs'
          translations,       % :: dict:dict(), %% FieldPath -> TranslationOps
          default_transls,    % :: sets:set({FnName::atom(),Arity::integer()})
          map_types,          % :: sets:set({map,_,_})
          map_value_types,    % :: {boolean(), boolean()} % submsgs?/nonsubmsgs?
          group_occurrences,  % :: dict:dict() %% GroupName -> repeated | ...
          has_p3_opt_strings  % :: boolean()
        }).

-define(f(Fmt),        io_lib:format(Fmt, [])).
-define(f(Fmt, Args),  io_lib:format(Fmt, Args)).
-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

-endif. % gpb_compile_hrl
