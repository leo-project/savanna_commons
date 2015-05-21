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
-module(svc_operate_behaviour).
-author('Yosuke Hara').

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").


%% @doc Retrieve a metric
-callback(handle_to_get_values(Hist::#histogram{}) ->
                 any() | {error, any()}).


%% @doc Retrieve a calculated statistics
-callback(handle_to_get_hist_stats(Hist::#histogram{}) ->
                 any() | {error, any()}).


%% @doc Input a value into the sample
-callback(handle_to_update(?HISTOGRAM_SLIDE   |
                           ?HISTOGRAM_UNIFORM |
                           ?HISTOGRAM_EXDEC, #slide{}|#uniform{}|#exdec{}, any()) ->
                 #slide{} | #uniform{} | #exdec{} | integer()).


%% @doc Input a value into the sample
-callback(trim_and_notify(State::#sv_metric_state{}, Result::#sv_result{}) ->
                 ok | {error, any()}).
