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
-module(savanna_commons_sup).
-author('Yosuke Hara').

-behaviour(supervisor).

-include("savanna_commons.hrl").
-include_lib("folsom/include/folsom.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         start_child/3,
         stop_child/1
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(DEF_WINDOW, 30).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Start children
%%
-spec(start_child(mod_sv_metrics(), atom(), #sv_metric_conf{}) ->
             ok | {error, any()}).
start_child(?MOD_METRICS_COUNTER = Mod, ServerId, MetricConf) ->
    #sv_metric_conf{window = Window,
                    step = Step,
                    callback = Callback} = MetricConf,
    ProcExpirationTime = ?env_proc_expiration_time(),
    ChildSpec = {ServerId,
                 {svc_metric_server, start_link, [ServerId, Mod, ?METRIC_COUNTER,
                                                  Window, Callback, Step, ProcExpirationTime]},
                 ?restart_type_of_child(ProcExpirationTime),
                 2000, worker, [svc_metric_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            ok;
        {error, Cause} ->
            {error, Cause}
    end;

start_child(?MOD_METRICS_GAUGE = Mod, ServerId, MetricConf) ->
    #sv_metric_conf{window = Window,
                    step = Step,
                    callback = Callback} = MetricConf,
    ProcExpirationTime = ?env_proc_expiration_time(),
    ChildSpec = {ServerId,
                 {svc_metric_server, start_link, [ServerId, Mod, ?METRIC_GAUGE,
                                                  Window, Callback, Step, ProcExpirationTime]},
                 ?restart_type_of_child(ProcExpirationTime),
                 2000, worker, [svc_metric_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            ok;
        {error, Cause} ->
            {error, Cause}
    end;

start_child(?MOD_METRICS_HISTOGRAM = Mod, ServerId, MetricConf) ->
    #sv_metric_conf{histogram_type = HistogramType,
                    window = Window,
                    step = Step,
                    sample_size = SampleSize,
                    alpha = Alpha,
                    callback = Callback} = MetricConf,
    ProcExpirationTime = ?env_proc_expiration_time(),
    ChildSpec = {ServerId,
                 {svc_metric_server, start_link, [ServerId, Mod, HistogramType,
                                                  Window, SampleSize, Alpha, Callback, Step, ProcExpirationTime]},
                 ?restart_type_of_child(ProcExpirationTime),
                 2000, worker, [svc_metric_server]},

    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok,_Pid} ->
            ok;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Stop a slide-server and a worker
%%
-spec(stop_child(list(pid())) ->
             ok).
stop_child(Pids) ->
    stop_child_1(Pids).

%% @private
stop_child_1([]) ->
    ok;
stop_child_1([Pid|Rest]) ->
    catch supervisor:terminate_child(?MODULE, Pid),
    catch supervisor:delete_child(?MODULE, Pid),
    stop_child_1(Rest).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    Children = [
                {folsom,
                 {folsom_sup, start_link, []},
                 permanent, 2000, supervisor, [folsom]}
               ],
    {ok, {{one_for_one, 5, 60}, Children}}.
