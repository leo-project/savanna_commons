%%======================================================================
%%
%% LeoProject - Savanna Commons
%%
%% Copyright (c) 2014-2015 Rakuten, Inc.
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
-module(svc_metrics_gauge).
-author('Yosuke Hara').

-behaviour(svc_operate_behaviour).

-include("savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([handle_to_get_values/1,
         handle_to_get_hist_stats/1,
         handle_to_update/3,
         trim_and_notify/2]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve a metric
handle_to_get_values(_)->
    [].


%% @doc Retrieve a calculated statistics
handle_to_get_hist_stats(_) ->
    [].


%% @doc Input a value into the sample
handle_to_update(_,_,_) ->
    0.


%% @doc Remove oldest values and notify metric with callback-func
-spec(trim_and_notify(#sv_metric_state{}, #sv_result{}) ->
             ok | {error, any()}).
trim_and_notify(#sv_metric_state{id = ServerId,
                                 notify_to = Callback}, #sv_result{} = Result) ->
    %% Retrieve the current value, then execute the callback-function
    Gauge = folsom_metrics_gauge:get_value(ServerId),
    {MetricGroup, Key} = ?sv_schema_and_key(ServerId),

    %% Notify a calculated metric,
    %% then clear oldest data
    Ret = case svc_tbl_metric_group:get(MetricGroup) of
              {ok, #sv_metric_group{schema_name = SchemaName}} ->
                  catch Callback:notify(Result#sv_result{metric_type = ?METRIC_GAUGE,
                                                         schema_name = SchemaName,
                                                         metric_group_name = MetricGroup,
                                                         col_name = Key,
                                                         result = Gauge}),
                   ok;
              _Error ->
                  {error, ?ERROR_COULD_NOT_GET_SCHEMA}
          end,
    folsom_metrics_gauge:clear(ServerId),
    Ret.
