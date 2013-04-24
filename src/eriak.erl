%% -------------------------------------------------------------------
%%
%% eriak: Methods managing connection pools and issuing requests
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

-module(eriak).

-include("eriak.hrl").

%% Pool Management Functions
-export([create_pool/2,
	      list_pools/0,
	      pool_properties/1,
         update_pool_properties/2,
         pool_status/1,
         destroy_pool/1,
         default_timeout/1
         ]).

%% @doc Constructor for new eriak object.
-spec create_pool(pool(), pool_props()) -> ok | {error, pool_already_exists} | {error, term()}.
create_pool(Pool, Props) when is_atom (Pool) ->

%% @doc Constructor for new eriak object.
-spec list_pools() -> [pool()].
list_pools() ->
    [].

%% @doc Retrieve list of pool configuration properties
-spec pool_properties(pool()) -> pool_props() | {error, pool_does_not_exist} | {error, term()}.
pool_properties(Pool) ->
    [].

%% @doc Update pool configuration properties
-spec update_pool_properties(pool(), pool_props()) -> ok | {error, pool_does_not_exist} | {error, term()}.
update_pool_properties(Pool, Props) when is_atom(Pool) andalso is_list(Props) ->
    ok.

%% @doc Get pool status
-spec pool_status(pool()) -> {ok, pool_status()} | {error, pool_does_not_exist} | {error, term()}.
pool_status(Pool) when is_atom(Pool) ->
    {ok, []}.

%% @doc Destroy pool and close all connections
-spec destroy_pool(pool()) -> ok | {error, pool_does_not_exist} | {error, term()}..
destroy_pool(Pool) when is_atom(Pool) ->
    ok.

