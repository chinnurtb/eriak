%% -------------------------------------------------------------------
%%
%% eriak_value: Methods for managing values and associated metadata
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc eriak_value is used to wrap data and metadata that forms the value
%%                  of a Riak object

-module(eriak_value).

%% @headerfile "eriak.hrl"
-include("eriak.hrl").

-export([new/1,new/2,
	     data/1,
         set_data/2,
         content_type/1,
         set_content_type/2,
         clear_content_type/1,
         charset/1,
         set_charset/2,
         clear_charset/1,
         encoding/1,
         set_encoding/2,
         clear_encoding/1,
         indexes/1,
         set_indexes/2,
         add_indexes/2,
         clear_indexes/1,
         usermeta/1,
         set_usermeta/2,
         add_usermeta/2,
         clear_usermeta/1,
         links/1,
         set_links/2,
         add_links/2,
         clear_links/1
        ]).

-record(eriak_value, {
          data :: data(),
          indexes :: [index()],
          usermeta :: [usermeta()],
          links :: [link()],
          content_type :: content_type(),
          charset :: charset(),
          encoding :: encoding()
         }).
-opaque eriak_value() :: #eriak_value{}.

%% @doc Constructor for new eriak value.
-spec new(data()) -> eriak_value().
new(Data) ->
    new(Data, undefined).

%% @doc Constructor for new eriak value.
-spec new(data(), content_type()) -> eriak_value().
new(Data, ContentType) when is_binary(Data) andalso is_list(ContentType) ->
    #eriak_value{data = Data, content_type = ContentType, indexes = [], usermeta = [], 
                 links = [], charset = undefined, encoding = undefined}.

%% @doc Return the data of the value
-spec data(eriak_value()) -> data().
data(Value) ->
    Value#eriak_value.data.

%% @doc Set the data for the value
-spec set_data(eriak_value(), data()) -> eriak_value().
set_data(Value, Data) ->
    Value#eriak_value{data = Data}.

%% @doc Return the data of the value
-spec content_type(eriak_value()) -> content_type() | undefined.
content_type(Value) ->
    Value#eriak_value.content_type.

%% @doc Set the content type for the value
-spec set_content_type(eriak_value(), content_type()) -> eriak_value().
set_content_type(Value, ContentType) when is_list(ContentType) ->
    Value#eriak_value{content_type = ContentType}.

%% @doc Clear the content type for the value
-spec clear_content_type(eriak_value()) -> eriak_value().
clear_content_type(Value) ->
    Value#eriak_value{content_type = undefined}.

%% @doc Return the data of the value
-spec charset(eriak_value()) -> charset() | undefined.
charset(Value) ->
    Value#eriak_value.charset.

%% @doc Set the charset for the value
-spec set_charset(eriak_value(), charset()) -> eriak_value().
set_charset(Value, Charset) when is_list(Charset) ->
    Value#eriak_value{charset = Charset}.

%% @doc Clear the charset for the value
-spec clear_charset(eriak_value()) -> eriak_value().
clear_charset(Value) ->
    Value#eriak_value{charset = undefined}.

%% @doc Return the encoding of the value
-spec encoding(eriak_value()) -> encoding() | undefined.
encoding(Value) ->
    Value#eriak_value.encoding.

%% @doc Set the encoding for the value
-spec set_encoding(eriak_value(), encoding()) -> eriak_value().
set_encoding(Value, Encoding) when is_list(Encoding) ->
    Value#eriak_value{encoding = Encoding}.

%% @doc Clear the encoding for the value
-spec clear_encoding(eriak_value()) -> eriak_value().
clear_encoding(Value) ->
    Value#eriak_value{encoding = undefined}.

%% @doc Return list of indexes of the value
-spec indexes(eriak_value()) -> [] | [index()].
indexes(Value) ->
    Value#eriak_value.indexes.

%% @doc Set indexes for the value. This will remove existing indexes.
-spec set_indexes(eriak_value(), [index()]) -> eriak_value() | {error, incorrect_index_specification}.
set_indexes(Value, IndexList) ->
    case process_index_list(IndexList) of
        {error, Reason} ->
            {error, Reason};
        ProcessedIndexList ->
            Value#eriak_value{indexes = lists:usort(ProcessedIndexList)}
    end.

%% @doc Add indexes to set of existing indexes for the value.
-spec add_indexes(eriak_value(), [index()]) -> eriak_value() | {error, incorrect_index_specification}.
add_indexes(#eriak_value{indexes = Indexes} = Value, IndexList) ->
    case process_index_list(IndexList) of
        {error, Reason} ->
            {error, Reason};
        ProcessedIndexList ->
            Value#eriak_value{indexes = lists:usort([Indexes | ProcessedIndexList])}
    end.

%% @doc Clear indexes for the value
-spec clear_indexes(eriak_value()) -> eriak_value().
clear_indexes(Value) ->
    Value#eriak_value{indexes = []}.

%% @doc Return the data of the value
-spec usermeta(eriak_value()) -> [] | [usermeta()].
usermeta(Value) ->
    Value#eriak_value.usermeta.

%% @doc Set user metadata for the value. This will remove existing user metadata.
-spec set_usermeta(eriak_value(), [index()]) -> eriak_value() | {error, duplicate_usermeta_entries | illegal_usermeta} .
set_usermeta(Value, UserMetaList) when is_list(UserMetaList) ->
    case validate_usermeta(UserMetaList) of
        {error, Reason} ->
            {error, Reason};
        ValidatedList ->
            Value#eriak_value{usermeta = lists:usort(ValidatedList)}
    end.

%% @doc Add user metadata to set of existing user metadata for the value.
-spec add_usermeta(eriak_value(), [index()]) -> eriak_value() | {error, duplicate_usermeta_entries | illegal_usermeta}.
add_usermeta(#eriak_value{usermeta = UserMeta} = Value, UserMetaList) when is_list(UserMetaList)->
    case validate_usermeta([UserMeta | UserMetaList]) of
        {error, Reason} ->
            {error, Reason};
        ValidatedList ->
            Value#eriak_value{usermeta = lists:usort(ValidatedList)}
    end.

%% @doc Clear usermeta for the value
-spec clear_usermeta(eriak_value()) -> eriak_value().
clear_usermeta(Value) ->
    Value#eriak_value{usermeta = []}.

%% @doc Return the data of the value
-spec links(eriak_value()) -> [] | [link()].
links(Value) ->
    Value#eriak_value.links.

%% @doc Set links for the value. This will remove existing links.
-spec set_links(eriak_value(), [link()]) -> eriak_value() | {error, illegal_link}.
set_links(Value, LinkList) ->
    case validate_links(LinkList) of
        {error, Reason} ->
            {error, Reason};
        ValidatedList ->
            Value#eriak_value{links = lists:usort(ValidatedList)}
    end.

%% @doc Add links to set of existing links for the value.
-spec add_links(eriak_value(), [index()]) -> eriak_value() | {error, illegal_link}.
add_links(#eriak_value{links = Links} = Value, LinkList) ->
    case validate_usermeta([Links | LinkList]) of
        {error, Reason} ->
            {error, Reason};
        ValidatedList ->
            Value#eriak_value{links = lists:usort(ValidatedList)}
    end.

%% @doc Clear links for the value
-spec clear_links(eriak_value()) -> eriak_value().
clear_links(Value) ->
    Value#eriak_value{links = []}.

%% Internal functions
process_index_list(IndexList) when is_list(IndexList) ->
    process_index_list(IndexList, []).

process_index_list([], Indexes) ->
    Indexes;
process_index_list([{IndexName, IndexValue} | Rest], Indexes) when is_list(IndexName) andalso is_integer(IndexValue) ->
    case lists:suffix("_int", IndexName) of
        true ->
            process_index_list(Rest, [{IndexName, IndexValue} | Indexes]);
        false ->
            NewIndexName = IndexName ++ "_int",
            process_index_list(Rest, [{NewIndexName, IndexValue} | Indexes])
    end;
process_index_list([{IndexName, IndexValue} | Rest], Indexes) when is_list(IndexName) andalso is_binary(IndexValue) ->
    case lists:suffix("_bin", IndexName) of
        true ->
            process_index_list(Rest, [{IndexName, IndexValue} | Indexes]);
        false ->
            NewIndexName = IndexName ++ "_bin",
            process_index_list(Rest, [{NewIndexName, IndexValue} | Indexes])
    end;
process_index_list(_, _) ->
    {error, incorrect_index_specification}.

validate_usermeta(UserMetaList) ->
    validate_usermeta(UserMetaList, []).

validate_usermeta([], UserMeta) ->
    Keys = lists:usort([K || {K, _} <- UserMeta]),
    case length(Keys) == length(UserMeta) of
        true ->
            UserMeta;
        false ->
            {error, duplicate_usermeta_entries}
    end;
validate_usermeta([{UserMetaName, UserMetaValue} | Rest], MetaData) when is_list(UserMetaName) andalso is_list(UserMetaValue) ->
    validate_usermeta(Rest, [{UserMetaName, UserMetaValue} | MetaData]); 
validate_usermeta(_, _) ->
    {error, illegal_usermeta}.

validate_links(Links) ->
    validate_links(Links, []).

validate_links([], Links) ->
    lists:usort(Links);
validate_links([{{Bucket, Key}, Tag} | Rest], Links) when is_binary(Bucket) andalso is_binary(Key) andalso is_list(Tag) ->
    validate_links(Rest, [{{Bucket, Key}, Tag} | Links]); 
validate_links(_, _) ->
    {error, illegal_link}.
