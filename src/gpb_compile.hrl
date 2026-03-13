-ifndef(gpb_compile_hrl).
-define(gpb_compile_hrl, true).

-record(ft, {type, occurrence, is_packed}).
-record(anres, %% result of analysis
        {
          source_filenames    :: [string()],
          used_types          :: sets:set(gpb_field_type()),
          known_msg_size      :: #{MsgName::atom() => Size::non_neg_integer() |
                                                      undefined},
          fixlen_types        :: sets:set(#ft{}),
          num_packed_fields   :: integer(),
          num_fields          :: #{MsgName::atom() => integer()},
          d_field_pass_method :: #{MsgName::atom() => pass_as_record |
                                                      pass_as_params},
          maps_as_msgs        :: gpb_defs:defs(),
          dec_maps_as_msgs    :: gpb_defs:defs(),
          translations        :: #{FieldPath::[atom() | []] =>
                                       [{Op::atom(),
                                         [{module(), atom(), [term()]}] |
                                         '$default'}]},
          types_only_via_translations :: sets:set(gpb_field_type()),
          map_types           :: sets:set({map,_,_}),
          map_value_types     :: {boolean(), boolean()}, % submsgs?/nonsubmsgs?
          group_occurrences   :: #{GroupName::atom() => repeated |
                                                        optional |
                                                        required},
          has_p3_opt_strings  :: boolean(),
          unknowns_info       :: both_with_and_without_field_for_unknowns |
                                 all_are_without_field_for_unknowns |
                                 all_are_with_field_for_unknowns |
                                 no_msgs,
          renamings           :: no_renamings | gpb_names:renamings()
        }).
-record(maps, {
          unset_optional  :: omitted | present_undefined,
          oneof           :: tuples | flat
         }).

-define(f(Fmt),        io_lib:format(Fmt, [])).
-define(f(Fmt, Args),  io_lib:format(Fmt, Args)).
-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

-endif. % gpb_compile_hrl
