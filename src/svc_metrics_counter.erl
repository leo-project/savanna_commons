%%======================================================================
%%
%% LeoProject - Savanna Commons
%%
%% Copyright (c) 2014 Rakuten, Inc.
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
%%======================================================================
-module(svc_metrics_counter).
-author('Yosuke Hara').

-behaviour(svc_operate_behaviour).

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([handle_get_values/1,
         handle_get_histogram_statistics/1,
         handle_update/3,
         trim_and_notify/1]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve a metric
handle_get_values(_)->
    [].


%% @doc Retrieve a calculated statistics
handle_get_histogram_statistics(_) ->
    [].


%% @doc Input a value into the sample
handle_update(_,_,_) ->
    ok.


%% @doc Remove oldest values and notify metric with callback-func
-spec(trim_and_notify(#sv_metric_state{}) ->
             ok | {error, any()}).
trim_and_notify(#sv_metric_state{id = ServerId,
                                 notify_to = Callback}) ->
    %% Retrieve the current value, then execute the callback-function
    Count = folsom_metrics_counter:get_value(ServerId),
    {SchemaName, Key} = ?sv_schema_and_key(ServerId),
    catch Callback:notify(SchemaName, {Key, Count}),

    %% Clear oldest data
    folsom_metrics_counter:clear(ServerId),
    ok.
