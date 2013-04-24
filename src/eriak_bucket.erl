%% -------------------------------------------------------------------
%%
%% eriak_bucket: Methods for managing bucket properties and creating objects
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

%% @doc eriak_bucket is used to wrap bucket operations and management 
%%                   of properties

-module(eriak_bucket).

-export([init/2,
         bucket/1,
         pool/1,
         properties/1,
         set_properties/2,
         list_keys/1,list_keys/2,
         stream_list_keys/1,stream_list_keys/2,
         list_buckets/1, list_buckets/2,
         new_object/3,
         get_object/2,get_object/3,get_object/4,
         multiget_objects/2,multiget_objects/3,multiget_objects/4,
         put_object/2,put_object/3,put_object/4,
         delete_object/2
        ]).

-record(eriak_bucket, {
          bucket :: bucket(),
          pool :: pool(),
          properties :: bucket_props()
         }).

%% @doc Initiate bucket for a specific pool.
-spec init(bucket(), pool()) -> eriak_bucket() | {error, pool_does_not_exist} | {error, term()}.
init(BucketName, Pool) when is_atom(Pool) andalso is_binary(BucketName) ->
    % Initiate record and get properties from pool
    {error, pool_does_not_exist}.

%% @doc Get bucket name.
-spec bucket(eriak_bucket()) -> bucket().
bucket(Bucket) ->
    Bucket#eriak_bucket.bucket.

%% @doc Get bucket pool.
-spec pool(eriak_bucket()) -> pool().
pool(Bucket) ->
    Bucket#eriak_bucket.pool.

%% @doc Get bucket properties.
-spec properties(eriak_bucket()) -> bucket_props().
properties(Bucket) ->
    % Get list of properties from riak, store updated list on bucket and return results
    [].

%% @doc Update bucket properties.
-spec set_properties(eriak_bucket(), bucket_props()) -> ok | {error, term()}.
set_properties(Bucket, Properties) ->
    ok.

%% @doc List all keys for bucket.
-spec list_keys(eriak_bucket()) -> {ok, [key()]} | {error, term()}.
list_keys(Bucket) ->
    {ok, []}.

%% @doc List all keys for bucket.
-spec list_keys(eriak_bucket(), timeout()) -> {ok, [key()]} | {error, term()}.    
list_keys(Bucket, Timeout) ->
    {ok, []}.

%% @doc Stream list of keys in the bucket to the calling process.  The
%%      process receives these messages.
%% ```    {ReqId::req_id(), {keys, [key()]}}
%%        {ReqId::req_id(), done}'''
%% <em>This is a potentially expensive operation and should not be used in production.</em>
%% @equiv stream_list_keys(eriak_bucket(), default_timeout(stream_list_keys_timeout))
-spec stream_list_keys(eriak_bucket() -> {ok, req_id()} | {error, term()}.
stream_list_keys(Bucket) ->
    stream_list_keys(Bucket, default_timeout(stream_list_keys_timeout)).

%% @doc Stream list of keys in the bucket to the calling process specifying server side
%%      timeout.
%%      The process receives these messages.
%% ```    {ReqId::req_id(), {keys, [key()]}}
%%        {ReqId::req_id(), done}'''
%% <em>This is a potentially expensive operation and should not be used in production.</em>
%% @equiv stream_list_keys(Pid, Bucket, Timeout, default_timeout(stream_list_keys_call_timeout))
-spec stream_list_keys(eriak_bucket(), timeout()) -> {ok, req_id()} | {error, term()}.
stream_list_keys(Bucket, Timeout) ->
    ok.

%% @doc List all buckets in database
-spec list_buckets(pool()) -> {ok, [bucket()]} | {error, pool_does_not_exist} | {error, term()}.
list_buckets(Pool) when is_atom(Pool) ->
    {ok, []}.

%% @doc List all buckets in database
-spec list_buckets(pool(), timeout()) -> {ok, [bucket()]} | {error, pool_does_not_exist} | {error, term()}.
list_buckets(Pool, Timeout) when is_atom(Pool) ->
    {ok, []}.

%% @doc Create new object
-spec new_object(eriak_bucket(), key(), eriak_value()) -> eriak_object() | {error, term()}.
new_object(Bucket, Key, Value) ->
    eriak_object:new(Bucket#eriak_bucket.bucket, Key, Value, Bucket#eriak_bucket.pool).

%% @doc Get object from the server based on key.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv get_object(Bucket, Key, [], default_timeout(get_timeout))
-spec get_object(eriak_bucket(), key()) -> {ok, eriak_object()} | {error, notfound} | {error, term()}.
get_object(Bucket, Key) ->
    get_object(Bucket, Key, [], eriak:default_timeout(get_timeout)).

%% @doc Get object from the server based on key.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv get_object(Bucket, Key, Props, default_timeout(get_timeout))
-spec get_object(eriak_bucket(), key(), get_options()) -> {ok, eriak_object()} | {error, notfound} | {error, term()}.
get_object(Bucket, Key, Props) when is_list(Props) ->
    get_object(Bucket, Key, Props, eriak:default_timeout(get_timeout));

%% @doc Get object from the server based on key.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv get_object(Bucket, Key, [], Timeout)
-spec get_object(eriak_bucket(), key(), timeout()) -> {ok, eriak_object()} | {error, notfound} | {error, term()}.
get_object(Bucket, Key, Timeout) ->
    get_object(Bucket, Key, [], Timeout).

%% @doc Get object from the server based on key.
%%      Will return {error, notfound} if the key is not on the server.
-spec get_object(eriak_bucket(), key(), get_options(), timeout()) -> {ok, eriak_object()} | {error, notfound} | {error, term()}.
get_object(Bucket, Key, Options, Timeout) ->
    ok.

%% @doc Get multiple objects in parallel from the server based on keys.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv multiget_objects(Bucket, [Key], [], default_timeout(get_timeout))
-spec multiget_objects(eriak_bucket(), [key()]) -> {ok, [eriak_object() | {key(), notfound} | {key(), error, term()}]} | {error, term()}.
multiget_objects(Bucket, Key) ->
    multiget_objects(Bucket, Key, [], eriak:default_timeout(multiget_timeout)).

%% @doc Get multiple objects in parallel from the server based on keys.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv multiget_objects(Bucket, Key, Props, default_timeout(get_timeout))
-spec multiget_objects(eriak_bucket(), key(), multiget_options()) -> {ok, [eriak_object() | {key(), notfound} | {key(), error, term()}]} | {error, notfound} | {error, term()}.
multiget_objects(Bucket, Key, Options) when is_list(Options) ->
    multiget_objects(Bucket, Key, Options, eriak:default_timeout(multiget_timeout));

%% @doc Get multiple objects in parallel from the server based on keys.
%%      Will return {error, notfound} if the key is not on the server.
%% @equiv multiget_objects(Bucket, Key, [], Timeout)
-spec multiget_objects(eriak_bucket(), key(), timeout()) -> {ok, [eriak_object() | {key(), notfound} | {key(), error, term()}]} | {error, notfound} | {error, term()}.
multiget_objects(Bucket, Key, Timeout) ->
    multiget_objects(Bucket, Key, [], Timeout).

%% @doc Get multiple objects in parallel from the server based on keys.
%%      Will return {error, notfound} if the key is not on the server.
-spec multiget_objects(eriak_bucket(), key(), multiget_options(), timeout()) -> {ok, [eriak_object() | {key(), notfound} | {key(), error, term()}]} | {error, notfound} | {error, term()}.
multiget_objects(Bucket, Key, Options, Timeout) ->
    ok.

%% @doc Put the object under bucket/key
%% @equiv put_object(Pid, Obj, [])
%% @see put_object/4
-spec put_object(eriak_bucket(), eriak_object()) ->
                 ok | {ok, eriak_object()} | {ok, key()} | {error, unresolved_siblings} | {error, term()}.
put_object(Bucket, Obj) ->
    put_object(Bucket, Obj, []).

%% @doc Put the object under bucket/key with options or timeout.
%% @equiv put_object(Pid, Obj, Options, Timeout)
%% @see put_object/4
-spec put_object(pid(), riakc_obj(), timeout() | put_options()) ->
                 ok | {ok, eriak_object()} | {ok, key()} | {error, unresolved_siblings} | {error, term()}.
put_object(Bucket, Obj, Timeout) when is_integer(Timeout); Timeout =:= infinity ->
    put(Bucket, Obj, [], Timeout);
put_object(Bucket, Obj, Options) ->
    put_object(Bucket, Obj, Options, default_timeout(put_timeout)).

%% @doc Put the object under bucket/key with
%%      options and timeout. 
-spec put_object(pid(), eriak_object(), put_options(), timeout()) ->
                 ok | {ok, eriak_object()} | {ok, key()} | {error, unresolved_siblings} | {error, term()}.
put(Bucket, Obj, Options, Timeout) ->
    ok.

%% @doc Delete object from the server based on bucket/key.
-spec delete_object(eriak_bucket(), key()) -> ok | {error, term()}.
delete_object(Bucket, Key) ->
    ok.
