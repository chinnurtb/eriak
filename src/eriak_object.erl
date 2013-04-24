%% -------------------------------------------------------------------
%%
%% eriak_object: Methods for management of Riak objects
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

%% @doc eriak_object is used to wrap bucket/key/value data sent to the
%%                   server on put and received on get. 


-module(eriak_object).

-include("eriak.hrl").

-export([new/3,new/4,new/5,
	       bucket/1,
	       key/1,
	       id/1,
         value/1,
         set_value/2,
         value_count/1,
         vclock/1,
         pool/1,
         set_pool/2,
         save/2,save/2,save/3,
         delete/1,delete/2,delete/2,
         is_updated/1
        ]).

-record(eriak_value, {
          bucket :: bucket(),
          key :: key(),
          vclock :: vclock() | undefined,
          values :: [eriak_value()],
          pool :: pool(),
          updated :: boolean(),
         }).
-opaque eriak_object() :: #eriak_object{}.

%% @doc Constructor for new eriak object.
-spec new(bucket(), key(), eriak_value()) -> eriak_object().
new(Bucket, Key, Value) ->
    #eriak_object{bucket = Bucket, key = Key, values = [Value], vclock = undefined, pool = undefined, updated = true}.

%% @doc Constructor for new eriak object.
-spec new(bucket(), key(), eriak_value(), pool()) -> eriak_object().
new(Bucket, Key, Value, Pool) when is_atom(Pool) ->
    #eriak_object{bucket = Bucket, key = Key, values = [Value], vclock = undefined, pool = Pool, updated = true}.

%% @doc Internal constructor for eriak object.
-spec new(bucket(), key(), [eriak_value()], pool(), vclock()) -> eriak_object().
new(Bucket, Key, ValueList, Pool, VClock) when is_binary(VClock) andalso is_atom(Pool) ->
    #eriak_object{bucket = Bucket, key = Key, values = ValueList, vclock = VClock, pool = Pool, updated = false}.

%% @doc Method for retrieving an objects bucket 
-spec bucket(eriak_object()) -> bucket().
bucket(Obj) ->
    Obj#eriak_object.bucket.

%% @doc Method for retrieving an objects key
-spec key(eriak_object()) -> key().
key(Obj) ->
    Obj#eriak_object.key.

%% @doc Method for retrieving an objects id
-spec id(eriak_object()) -> bucket().
id(Obj) ->
    {Obj#eriak_object.bucket, Obj#eriak_object.key}.

%% @doc Method for retrieving an objects value(s) 
-spec value(eriak_object()) -> eriak_value() | [eriak_value()].
value(Obj) ->
    case Obj#eriak_object.values of
    	[Value] ->
    	    Value;
    	ValueList ->
    	    ValueList
    end.

%% @doc Method for updating the value of an object 
-spec value(eriak_object(), eriak_value()) -> eriak_object().
set_value(Obj, Value) ->
    Obj#eriak_object{values = [Value], updated = true}.

%% @doc Method for retrieving the number of values an object has
-spec value_count(eriak_object()) -> integer().
value_count(Obj) ->
    length(Obj#eriak_object.values).

%% @doc Method for retrieving the vector clock of an object
-spec vclock(eriak_object()) -> vclock().
vclock(Obj) ->
    Obj#eriak_object.vclock.

%% @doc Method for saving the object to Riak 
-spec save(eriak_object()) -> ok | {error, unresolved_siblings} |  {error, not_updated} | {error, pool_not_defined} | {error, pool_does_not_exist} | {error, term()}.
save(Obj) ->
    case pool(Obj) ->
        undefined ->
            {error, pool_not_defined};
        Pool ->
            save(Obj, Pool)
    end.

%% @doc Method for saving the object to Riak 
-spec save(eriak_object(), pool()) -> ok | {error, unresolved_siblings} |  {error, not_updated} | {error, pool_does_not_exist} | {error, term()}.
save(Obj, Pool) when is_atom(Pool) -> 
    case {value_count(Obj), updated(Obj)} of
        {1, true} ->
            eriak:put(Pool, Obj);
        {N, _} when N > 1 ->
            {error, unresolved_siblings};
        {_, false} ->
            {error, not_updated}
    end.

%% @doc Method for deleting the object from Riak 
-spec delete(eriak_object()) -> ok | {error, pool_not_defined} | {error, pool_does_not_exist} | {error, term()}.
delete(Obj) ->
    case pool(Obj) ->
        undefined ->
            {error, pool_not_defined};
        Pool ->
            delete(Obj, Pool)
    end.

%% @doc Method for deleting the object from Riak 
-spec delete(eriak_object(), pool()) -> ok | {error, pool_not_defined} | {error, pool_does_not_exist} | {error, term()}.
delete(Obj, Pool) when is_atom(Pool) ->
    Bucket = key(Obj),
    Key = bucket(Obj),
    eriak:delete(Pool, Bucket, Key).

%% @doc Method for retrieving the pool the object is linked to
-spec pool(eriak_object()) -> pool() | undefined.
pool(Obj) ->
    Obj#eriak_object.pool.

%% @doc Method for setting the pool the object is linked to
-spec set_pool(eriak_object(), pool()) -> eriak_object().
set_pool(Obj, Pool) when is_atom(Pool) ->
    Obj#eriak_object{pool = Pool}.

%% @doc Method for checking if the object has been updated or not
-spec is_updated(eriak_object()) -> boolean().
is_updated(Obj) ->
    Obj#eriak_object.updated.
