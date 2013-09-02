%%% Copyright (C) 2013  Tomas Abrahamsson
%%%
%%% Author: Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Library General Public
%%% License as published by the Free Software Foundation; either
%%% version 2 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Library General Public License for more details.
%%%
%%% You should have received a copy of the GNU Library General Public
%%% License along with this library; if not, write to the Free
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-module(gpb_compile_descr).

-export([defs_to_descriptor/1, defs_to_descriptor/2]).
-export([encode_defs_to_descriptor/1, encode_defs_to_descriptor/2]).

-include("gpb_descriptor.hrl").
-include("../include/gpb.hrl").

-define(ff(Fmt, Args), lists:flatten(io_lib:format(Fmt, Args))).

encode_defs_to_descriptor(Defs) ->
    encode_defs_to_descriptor(undefined, Defs).

encode_defs_to_descriptor(Name, Defs) ->
    gpb_descriptor:encode_msg(defs_to_descriptor(Name, Defs), [verify]).

defs_to_descriptor(Defs) ->
    defs_to_descriptor(undefined, Defs).

defs_to_descriptor(Name, Defs) ->
    #'FileDescriptorSet'{file = [defs_to_descr_2(Name, Defs)]}.

defs_to_descr_2(Name, Defs) ->
    #'FileDescriptorProto'{
       name             = Name,      %% string() | undefined
       package          = defs_to_package(Defs),
       dependency       = [],        %% [string()]
       message_type     = defs_to_msgtype(Defs),
       enum_type        = defs_to_enumtype(Defs),
       service          = [],        %% [#'ServiceDescriptorProto'{}]
       extension        = [],        %% [#'FieldDescriptorProto'{}]
       options          = undefined, %% #'FileOptions'{} | undefined
       source_code_info = undefined  %% #'SourceCodeInfo'{} | undefined
      }.

defs_to_package(Defs) ->
    %% There can be at most 1 package definition
    %% The parser will reject any multiple package definitions
    case [P || {package, P} <- Defs] of
        [Pkg] -> atom_to_ustring(Pkg);
        []    -> undefined
    end.

defs_to_msgtype(Defs) ->
    %% There is a slight bit of mismatch here: the DescriptorProto
    %% contains fields for `nested_type' and `enum_type', and defines
    %% a name resolution scheme, but the gpb parser already un-nests,
    %% resolves and extends such things.
    %%
    %% To produce a faithful (ie: similar to what protoc would
    %% produce) defintion, the parser would need to additionally
    %% save also the unprocessed parse results.
    [#'DescriptorProto'{
        name            = atom_to_ustring(MsgName),
        field           = field_defs_to_mgstype_fields(Fields),
        extension       = [],
        nested_type     = [],
        enum_type       = [],
        extension_range = [],
        options         = undefined
       }
     || {{msg,MsgName}, Fields} <- Defs].

field_defs_to_mgstype_fields(Fields) ->
    [#'FieldDescriptorProto'{
        name          = atom_to_ustring(FName),
        number        = FNum,
        label         = occurrence_def_to_descr_label(Occurrence),
        type          = type_to_descr_type(Type),
        type_name     = type_to_descr_type_name(Type),
        default_value = field_default_value(Field),
        options       = field_options(Opts)}
     || #field{name=FName,
               fnum=FNum,
               type=Type,
               occurrence=Occurrence,
               opts=Opts}=Field <- Fields].

occurrence_def_to_descr_label(optional) -> 'LABEL_OPTIONAL';
occurrence_def_to_descr_label(required) -> 'LABEL_REQUIRED';
occurrence_def_to_descr_label(repeated) -> 'LABEL_REPEATED'.

type_to_descr_type(sint32)           -> 'TYPE_SINT32';
type_to_descr_type(sint64)           -> 'TYPE_SINT64';
type_to_descr_type(int32)            -> 'TYPE_INT32';
type_to_descr_type(int64)            -> 'TYPE_INT64';
type_to_descr_type(uint32)           -> 'TYPE_UINT32';
type_to_descr_type(uint64)           -> 'TYPE_UINT64';
type_to_descr_type(bool)             -> 'TYPE_BOOL';
type_to_descr_type({enum,_EnumName}) -> 'TYPE_ENUM';
type_to_descr_type(fixed64)          -> 'TYPE_FIXED64';
type_to_descr_type(sfixed64)         -> 'TYPE_SFIXED64';
type_to_descr_type(double)           -> 'TYPE_DOUBLE';
type_to_descr_type(string)           -> 'TYPE_STRING';
type_to_descr_type(bytes)            -> 'TYPE_BYTES';
type_to_descr_type({msg,_MsgName})   -> 'TYPE_MESSAGE';
type_to_descr_type(fixed32)          -> 'TYPE_FIXED32';
type_to_descr_type(sfixed32)         -> 'TYPE_SFIXED32';
type_to_descr_type(float)            -> 'TYPE_FLOAT'.

type_to_descr_type_name({msg,MsgName})   -> atom_to_ustring(MsgName);
type_to_descr_type_name({enum,EnumName}) -> atom_to_ustring(EnumName);
type_to_descr_type_name(_)               -> undefined.

field_default_value(#field{type=Type, opts=Opts}) ->
    case {Type, proplists:get_value(default, Opts)} of
        {_, undefined}     -> undefined;
        {sint32, I}        -> integer_to_list(I);
        {sint64, I}        -> integer_to_list(I);
        {int32, I}         -> integer_to_list(I);
        {int64, I}         -> integer_to_list(I);
        {uint32, I}        -> integer_to_list(I);
        {uint64, I}        -> integer_to_list(I);
        {bool, B}          -> atom_to_list(B);
        {{enum,_EnumName}, E} -> atom_to_ustring(E);
        {fixed64, I}       -> integer_to_list(I);
        {sfixed64, I}      -> integer_to_list(I);
        {double, F}        -> float_to_list(F);
        {string, S}        -> S;
        {bytes, B}         -> escape_bytes(B);
        {fixed32, I}       -> integer_to_list(I);
        {sfixed32, I}      -> integer_to_list(I);
        {float, F}         -> float_to_list(F)
    end.

field_options(Opts) ->
    Packed = case lists:member(packed, Opts) of
                 true  -> true;
                 false -> undefined
             end,
    Deprecated = case lists:member(deprecated, Opts) of
                     true  -> true;
                     false -> undefined
                 end,
    if Packed == undefined, Deprecated == undefined ->
            undefined;
       true ->
            #'FieldOptions'{packed     = Packed,
                            deprecated = Deprecated}
    end.

defs_to_enumtype(Defs) ->
    [#'EnumDescriptorProto'{
        name  = atom_to_ustring(EnumName),
        value = [#'EnumValueDescriptorProto'{name   = atom_to_ustring(EName),
                                             number = EValue}
                 || {EName, EValue} <- Enumerators]}
     || {{enum,EnumName}, Enumerators} <- Defs].

atom_to_ustring(A) ->
    Utf8Str = atom_to_list(A),
    unicode:characters_to_list(list_to_binary(Utf8Str), utf8).

escape_bytes(<<B, Rest/binary>>) ->
    if B >= 127 -> escape_char(B) ++ escape_bytes(Rest);
       B < $\s  -> escape_char(B) ++ escape_bytes(Rest);
       true     -> [B | escape_bytes(Rest)]
    end;
escape_bytes(<<>>) ->
    "".

escape_char(C) -> ?ff("\\~.8b", [C]).
