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

%% -behaviour(gen_server).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([get_status/1,
         get_values/1,
         update/2,
         get_histogram_statistics/1,
         resize/2,
         trim_and_notify/1
        ]).

-define(DEF_WIDTH,   16).
-define(DEF_WINDOW,  60).
-define(DEF_TIMEOUT, 30000).
-define(HOURSECS,    3600).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Retrieve current status
-spec(get_status(atom()) ->
             {ok, list(tuple())}).
get_status(ServerId) ->
    svc_metric_server:get_status(ServerId).


%% @doc Retrieve value
-spec(get_values(sv_metric()) ->
             {ok, list()}).
get_values(ServerId) ->
    Hist  = get_value(ServerId),
    get_values_1(Hist#histogram.type, Hist#histogram.sample).

%% @doc Retrieve values
%% @private
get_values_1(?HISTOGRAM_SLIDE, Sample) ->
    folsom_sample_slide:get_values(Sample);
get_values_1(?HISTOGRAM_UNIFORM, Sample) ->
    folsom_sample_uniform:get_values(Sample);
get_values_1(?HISTOGRAM_EXDEC, Sample) ->
    folsom_sample_exdec:get_values(Sample).


%% @doc Retrieve histogram-stat
-spec(get_histogram_statistics(sv_metric()) ->
             {ok, list()} | not_found | {error, any()}).
get_histogram_statistics(ServerId) ->
    CurrentStat = get_current_statistics(ServerId),
    {ok, CurrentStat}.


%% @doc Put a value
-spec(update(sv_metric(), any()) ->
             ok | {error, any()}).
update(ServerId, Value) ->
    Hist = get_value(ServerId),
    Sample = Hist#histogram.sample,
    Callback = fun update_1/3,
    svc_metric_server:update(ServerId, Value, Hist, Sample, Callback).

%% @doc Insert the value
%% @private
update_1(?HISTOGRAM_SLIDE, Sample, Value) ->
    folsom_sample_slide:update(Sample, Value);
update_1(?HISTOGRAM_UNIFORM, Sample, Value) ->
    folsom_sample_uniform:update(Sample, Value);
update_1(?HISTOGRAM_EXDEC, Sample, Value) ->
    folsom_sample_exdec:update(Sample, Value).


%% @doc Resize the metric
-spec(resize(sv_metric(), pos_integer()) ->
             ok | {error, any()}).
resize(ServerId, NewSize) ->
    svc_metric_server:resize(ServerId, NewSize).


%% @doc Remove oldest values and notify metric with callback-func
-spec(trim_and_notify(#sv_metric_state{}) ->
             ok | {error, any()}).
trim_and_notify(#sv_metric_state{id = ServerId,
                                 type = SampleType,
                                 notify_to = Callback} = State)->
    %% Retrieve the current value, then execute the callback-function
    {MetricGroup, Key} = ?sv_schema_and_key(ServerId),
    CurrentStat = get_current_statistics(ServerId),

    catch Callback:notify(MetricGroup, {Key, CurrentStat}),
    try
        trim_1(SampleType, ServerId)
    catch
        _:Cause ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "trim_1/3"},
                                    {line, ?LINE}, {body, Cause}])
    end,
    State.

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


%% @doc Retrieve the current statistics
%% @private
get_current_statistics(ServerId) ->
    Hist = get_value(ServerId),
    Values = get_values_1(Hist#histogram.type, Hist#histogram.sample),
    bear:get_statistics(Values).
