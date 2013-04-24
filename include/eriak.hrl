%% -------------------------------------------------------------------
%%
%% eriak: Riak Erlang Client
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

-define(PROTO_MAJOR, 1).
-define(PROTO_MINOR, 0).
-define(DEFAULT_TIMEOUT, 60000).
-define(FIRST_RECONNECT_INTERVAL, 100).
-define(MAX_RECONNECT_INTERVAL, 30000).

-type bucket() :: binary().
-type key() :: binary().
-type id() :: {bucket(), key()}.
-type data() :: term().
-type content_type() :: string().
-type charset() :: string().
-type encoding() :: string().
-type index_name() :: string().
-type integer_index() :: {index_name(), integer()}.
-type binary_index() :: {index_name(), binary()}.
-type index() :: integer_index() | binary_index().
-type usermeta_parameter() :: string().
-type usermeta_value() :: string().
-type usermeta() :: {usermeta_parameter(), usermeta_value()}.
-type tag() :: string().
-type link() :: {id(), tag()}.
-type vclock() :: binary().
-type pool() :: atom().

-type quorum() :: non_neg_integer() | one | all | quorum | default.
-type read_quorum() :: {r, quorum()} | {pr, quorum()}.
-type write_quorum() :: {w, quorum()} | {dw, quorum()} | {pw, quorum()}.
-type delete_quorum() :: read_quorum() | write_quorum() | {rw, quorum()}.
-type get_option() :: read_quorum() | {if_modified, vclock()} | {notfound_ok, boolean()} | {basic_quorum, boolean()}.
-type put_option() :: write_quorum() | return_body | return_head | if_not_modified | if_none_match.
-type get_options() :: [get_option()].
-type put_options() :: [put_option()].
-type multiget_option() :: get_option() | {concurrency, integer()}.
-type multiget_options() :: [multiget_option()]. 

-type bucket_prop() :: {n_val, pos_integer()} | {allow_mult, boolean()}.
-type bucket_props() :: [bucket_prop()].

-type pool_prop() :: {n_val, pos_integer()} | {allow_mult, boolean()}.
-type pool_props() :: [pool_prop()].

-type pool_status_item() :: {current_pool_size, integer()} | {available_connections, integer()}.
-type pool_status() :: [pool_status_item()].








-type search_options() :: [search_option()]. %% A list of options for a search request.
-type delete_options() :: [delete_quorum()]. %% A list of options for a delete request.
-type mapred_queryterm() ::  {map, mapred_funterm(), Arg::term(), Accumulate :: boolean()} |
                             {reduce, mapred_funterm(), Arg::term(),Accumulate :: boolean()} |
                             {link, Bucket :: riakc_obj:bucket(), Tag :: term(), Accumulate :: boolean()}.
%% A MapReduce phase specification. `map' functions operate on single
%% K/V objects. `reduce' functions operate across collections of
%% inputs from other phases. `link' is a special type of map phase
%% that matches links in the fetched objects. The `Arg' parameter will
%% be passed as the last argument to the phase function. The
%% `Accumulate' param determines whether results from this phase will
%% be returned to the client.
-type mapred_funterm() :: {modfun, Module :: atom(), Function :: atom()} |
                          {qfun, function()} |
                          {strfun, list() | binary()} |
                          {jsanon, binary() | {bucket(), key()}} |
                          {jsfun, binary()}.
%% A MapReduce phase function specification. `modfun' requires that
%% the compiled module be available on all Riak nodes. `qfun' will
%% only work from the shell (compiled fun() terms refer to compiled
%% code only). `strfun' contains the textual source of an Erlang
%% function but the functionality must be enabled on the Riak cluster.
%% `jsanon' either contains javascript code that will be evaluated 
%% as an anonymous function, or a bucket-value pair, pointing
%% to a record stored in riak containing the source of an anonymous 
%% javascript function. `jsfun' contains the name of a javascript
%% function, that when evaluated points to a built-in javascript function.
-type mapred_result() :: [term()].
%% The results of a MapReduce job.
-type mapred_inputs() :: [{bucket(), key()} | {{bucket(), key()}, term()}] |
                         {modfun, Module::atom(), Function::atom(), [term()]} |
                         bucket() |
                         {index, bucket(), Index::binary()|secondary_index_id(), key()|integer()} |
                         {index, bucket(), Index::binary()|secondary_index_id(), StartKey::key()|integer(), EndKey::key()|integer()}.
%% Inputs for a MapReduce job.
-type connection_failure() :: {Reason::term(), FailureCount::integer()}.
%% The reason for connection failure and how many times that type of
%% failure has occurred since startup.
-type timeout_name() :: ping_timeout | get_client_id_timeout |
                        set_client_id_timeout | get_server_info_timeout |
                        get_timeout | put_timeout | delete_timeout |
                        list_buckets_timeout | list_buckets_call_timeout |
                        list_keys_timeout | stream_list_keys_timeout |
                        stream_list_keys_call_timeout | get_bucket_timeout |
                        get_bucket_call_timeout | set_bucket_timeout |
                        set_bucket_call_timeout | mapred_timeout |
                        mapred_call_timeout | mapred_stream_timeout |
                        mapred_stream_call_timeout | mapred_bucket_timeout |
                        mapred_bucket_call_timeout | mapred_bucket_stream_call_timeout |
                        search_timeout | search_call_timeout |
                        timeout.

-type secondary_index_id() :: {binary_index, string()} | {integer_index, string()}.
-type index_result() :: [key()].

-type search_option() ::
        {rows, non_neg_integer()} |  %% Limit rows
        {start, non_neg_integer()} | %% Starting offset
        {sort, binary()} |           %% sort order
        {filter, binary()} |         %% Inline fields filtering query
        {df, binary() } |            %% Default field
        {op, binary() } |            %% Default op
        {fl, [binary()]} |           %% Return fields limit (for ids only, generally)
        {presort, binary()}.         %% Presor (key / score)

-type search_doc() :: [{binary(), binary()}].
-type search_maxscore() :: float().
-type search_number_found() :: non_neg_integer().

-record(search_results, {
          docs :: [search_doc()],         %% Result documents
          max_score :: float(),           %% Maximum score
          num_found :: non_neg_integer()  %% Number of results
         }).

-type search_result() :: #search_results{}.
