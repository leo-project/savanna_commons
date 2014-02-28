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
-module(svc_metrics_histogram).
-author('Yosuke Hara').

-behaviour(svc_operate_behaviour).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([handle_get_values/1,
         handle_get_histogram_statistics/1,
         handle_update/3,
         trim_and_notify/2]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrive values
handle_get_values(Hist) ->
    get_values_1(Hist#histogram.type, Hist#histogram.sample).


%% @doc Retrive histogram-stats
handle_get_histogram_statistics(Hist) ->
    get_current_statistics(Hist).


%% @doc Insert the value
handle_update(?HISTOGRAM_SLIDE, Sample, Value) ->
    folsom_sample_slide:update(Sample, Value);
handle_update(?HISTOGRAM_UNIFORM, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
handle_update(?HISTOGRAM_EXDEC, Sample, Value) ->
    folsom_sample_exdec:update(Sample, Value).


%% @doc Remove oldest values and notify metric with callback-func
-spec(trim_and_notify(#sv_metric_state{}, #sv_result{}) ->
             ok | {error, any()}).
trim_and_notify(#sv_metric_state{id = ServerId,
                                 type = SampleType,
                                 notify_to = Callback}, #sv_result{} = Result)->
    %% Retrieve the current value, then execute the callback-function
    {MetricGroup, Key} = ?sv_schema_and_key(ServerId),
    Hist = get_value(ServerId),
    CurrentStat = get_current_statistics(Hist),

    %% Notify a calculated statistics,
    %% then clear oldest data
    case svc_tbl_metric_group:get(MetricGroup) of
        {ok, #sv_metric_group{schema_name = SchemaName}} ->
            catch Callback:notify(Result#sv_result{schema_name = SchemaName,
                                                   metric_group_name = MetricGroup,
                                                   col_name = Key,
                                                   result = CurrentStat}),
            try
                ok = trim_1(SampleType, ServerId)
            catch
                _:Cause ->
                    error_logger:error_msg("~p,~p,~p,~p~n",
                                           [{module, ?MODULE_STRING},
                                            {function, "trim_and_notify/1"},
                                            {line, ?LINE}, {body, Cause}])
            end,
            ok;
        _ ->
            {error, ?ERROR_COULD_NOT_GET_SCHEMA}
    end.

%% @private
trim_1(?HISTOGRAM_SLIDE, ServerId) ->
    Hist = get_value(ServerId),
    Sample = Hist#histogram.sample,
    ets:delete_all_objects(Sample#slide.reservoir),
    ok;

trim_1(?HISTOGRAM_UNIFORM, ServerId) ->
    Hist = get_value(ServerId),
    Sample = Hist#histogram.sample,
    true = ets:insert(?HISTOGRAM_TABLE,
                      {ServerId,
                       Hist#histogram{sample = Sample#uniform{n = 1,
                                                              seed = os:timestamp()}}}),
    ets:delete_all_objects(Sample#uniform.reservoir),
    ok;
trim_1(?HISTOGRAM_EXDEC, ServerId) ->
    Hist = get_value(ServerId),
    Sample = Hist#histogram.sample,
    true = ets:insert(?HISTOGRAM_TABLE,
                      {ServerId,
                       Hist#histogram{sample = Sample#exdec{start = 0,
                                                            next = 0,
                                                            seed = os:timestamp(),
                                                            n = 1}}}),
    ets:delete_all_objects(Sample#exdec.reservoir),
    ok.


%%--------------------------------------------------------------------
%%% INNER FUNCTIONS
%%--------------------------------------------------------------------
%% @private
get_value(ServerId) ->
    [{_, Value}] = ets:lookup(?HISTOGRAM_TABLE, ServerId),
    Value.


%% @doc Retrieve values
%% @private
get_values_1(?HISTOGRAM_SLIDE, Sample) ->
    folsom_sample_slide:get_values(Sample);
get_values_1(?HISTOGRAM_UNIFORM, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values_1(?HISTOGRAM_EXDEC, Sample) ->
    folsom_sample_exdec:get_values(Sample).


%% @doc Retrieve the current statistics
%% @private
get_current_statistics(Hist) ->
    Values = get_values_1(Hist#histogram.type, Hist#histogram.sample),
    bear:get_statistics(Values).
